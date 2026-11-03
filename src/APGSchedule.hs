{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module APGSchedule
  ( -- * Core Data Types
    Actor (..),
    Edge (..),
    ActorInstance,
    PrecedenceEdge,
    ScheduledFiring (..),
    MultiCoreSchedule,

    -- * Graph Helper Functions
    incomingEdgeIndices,
    outgoingEdgeIndices,
    updateAt,
    fireOnce,

    -- * SDF to APG Construction
    dFunction,
    isActorRunnable,
    createAcyclicPresedenceGraph,
    helperAPG,

    -- * Multi-Core List Scheduling
    computeHuLevels,
    listSchedulingHuLevel,
    toPerCoreSchedule,
  )
where

import Data.List (foldl', partition, sortBy)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import qualified Data.Vector as V
import ForSyDeIR (IRId)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- 1. Core Data Types
--------------------------------------------------------------------------------

-- | Represents an SDF actor in the graph.
data Actor = Actor
  { name :: IRId,
    isInput :: Bool
  }
  deriving (Show, Eq, Generic)

-- | Represents a directed edge (FIFO channel) between two actors.
data Edge = Edge
  { edgeName :: IRId,
    src :: Actor,
    dst :: Actor,
    prod :: Int,
    cons :: Int,
    isDelay :: Bool,
    initTokens :: Int, -- Count of initial tokens for delay edges
    buffers :: Maybe [(IRId, IRId)]
  }
  deriving (Show, Eq, Generic)

-- | Represents a specific invocation of an actor: (Actor ID, 1-based instance number).
--   Example: ("actor_a", 1) is the first firing of actor_a.
type ActorInstance = (IRId, Int)

-- | Represents a directed precedence dependency: (Producer Instance, Consumer Instance).
--   Example: (("actor_a", 1), ("actor_b", 1)) means A^1 must finish before B^1 starts.
type PrecedenceEdge = (ActorInstance, ActorInstance)

-- | Represents a scheduled execution of an actor firing on a specific core.
data ScheduledFiring = ScheduledFiring
  { sfCore      :: Int,            -- ^ 0-based Core ID (e.g. 0 for Core 0, 1 for Core 1)
    sfInstance  :: ActorInstance,   -- ^ The specific firing (ActorId, IterationNumber)
    sfStartTime :: Int,            -- ^ Start time (tick / cycle)
    sfEndTime   :: Int             -- ^ Finish time (tick / cycle)
  }
  deriving (Show, Eq, Generic)

-- | A multi-core schedule mapping each core to its assigned sequence of firings.
type MultiCoreSchedule = Map.Map Int [ActorInstance]

-- | Internal helper type to track tasks currently in flight on a processor.
data ActiveTask = ActiveTask
  { atFinishTime :: Int,
    atCore       :: Int,
    atInstance   :: ActorInstance
  }
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- 2. Graph Helper Functions
--------------------------------------------------------------------------------

-- | Get all indices of incoming edges for a given actor (where dst edge == actor).
incomingEdgeIndices :: [Actor] -> [Edge] -> Int -> [Int]
incomingEdgeIndices actors edges actorIndex =
  [idx | (idx, edge) <- zip [0 ..] edges, dst edge == (actors !! actorIndex)]

-- | Get all indices of outgoing edges for a given actor (where src edge == actor).
outgoingEdgeIndices :: [Actor] -> [Edge] -> Int -> [Int]
outgoingEdgeIndices actors edges actorIndex =
  [idx | (idx, edge) <- zip [0 ..] edges, src edge == (actors !! actorIndex)]

-- | Update the element at a given index with a modifier function.
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt index func list =
  case splitAt index list of
    (before, element : after) -> before ++ func element : after
    (_, []) -> error $ "updateAt: index " ++ show index ++ " out of bounds for list of length " ++ show (length list)

-- | Simulate one firing of a given actor:
--   1. Consume tokens on incoming edges according to each edge's consumption rate.
--   2. Produce tokens on outgoing edges according to each edge's production rate.
fireOnce :: [Actor] -> [Edge] -> Int -> [Int] -> [Int]
fireOnce actors edges actorIndex tokens =
  let incomingEdges = incomingEdgeIndices actors edges actorIndex
      outgoingEdges = outgoingEdgeIndices actors edges actorIndex
      tokensAfterConsume = foldl' consume tokens incomingEdges
      tokensAfterProduce = foldl' produce tokensAfterConsume outgoingEdges
   in tokensAfterProduce
  where
    consume :: [Int] -> Int -> [Int]
    consume currentTokens edgeIndex =
      let consumption = cons (edges !! edgeIndex)
       in updateAt edgeIndex (subtract consumption) currentTokens

    produce :: [Int] -> Int -> [Int]
    produce currentTokens edgeIndex =
      let production = prod (edges !! edgeIndex)
       in updateAt edgeIndex (+ production) currentTokens

--------------------------------------------------------------------------------
-- 3. SDF to APG Construction (Lee & Messerschmitt 1987)
--------------------------------------------------------------------------------

-- | Equation (7) from Lee & Messerschmitt (1987):
--   d = ceil( (j * consRate - initToks) / prodRate )
--   Returns the number of producer instances that must precede the j-th consumer instance.
dFunction :: Int -> Int -> Int -> Int -> Int
dFunction j consRate initToks prodRate
  | prodRate <= 0 = error "Production rate must be strictly positive"
  | otherwise =
      let num = j * consRate - initToks
          (quotient, remainder) = num `divMod` prodRate
       in if remainder == 0 then quotient else quotient + 1

-- | Check if a specific actor (by index) is runnable in the current simulation state:
--   1. It has been fired fewer than targetReps[actorIdx] times.
--   2. All incoming buffers contain enough tokens for one firing.
isActorRunnable :: [Actor] -> [Edge] -> [Int] -> V.Vector Int -> [Int] -> Int -> Either String Bool
isActorRunnable actors edges targetReps q currentBuffer actorIdx = do
  let currentRuns = q V.! actorIdx
      maxRuns = targetReps !! actorIdx
      incomingEdges = incomingEdgeIndices actors edges actorIdx
      actor = actors !! actorIdx

  if currentRuns >= maxRuns
    then Right False
    else case incomingEdges of
      [] ->
        if isInput actor
          then Right True
          else Left $ "Invalid graph: actor " ++ show (name actor) ++ " has no incoming edges but is not marked as input."
      _ ->
        Right $
          all
            (\edgeIdx -> (currentBuffer !! edgeIdx) >= cons (edges !! edgeIdx))
            incomingEdges

-- | Constructs the Acyclic Precedence Graph (APG) for one period (or J periods).
--   Returns Either an error message (e.g. deadlock) or (APG Vertices, Precedence Edges).
createAcyclicPresedenceGraph ::
  [Actor] ->
  [Edge] ->
  [[Integer]] ->
  [Int] ->
  Either String ([ActorInstance], [PrecedenceEdge])
createAcyclicPresedenceGraph actors edges _matrix repCounts = do
  let -- Initial buffer state b(0) from initial tokens (delays)
      initBuffers :: [Int]
      initBuffers = map initTokens edges

      -- q(0) = 0 vector of size (number of actors)
      q0 :: V.Vector Int
      q0 = V.replicate (length actors) 0

  -- Run the recursive Class S simulation loop
  (finalQ, instances, presEdges) <- helperAPG initBuffers q0 actors edges _matrix repCounts [] []

  -- Check termination condition: did every actor run its required number of times?
  if V.toList finalQ == repCounts
    then Right (reverse instances, reverse presEdges)
    else Left "Error: Deadlock detected while constructing Acyclic Precedence Graph."

-- | Recursive helper implementing "The Main Body" simulation loop from page 1237 of the paper.
helperAPG ::
  [Int] ->
  V.Vector Int ->
  [Actor] ->
  [Edge] ->
  [[Integer]] ->
  [Int] ->
  [ActorInstance] ->
  [PrecedenceEdge] ->
  Either String (V.Vector Int, [ActorInstance], [PrecedenceEdge])
helperAPG currentBuffer q actors edges _matrix targetReps presActorInstances presEdges = do
  let actorIndices = [0 .. length actors - 1]

  -- Check if ANY actor is runnable to decide whether the loop continues
  runnableFlags <- mapM (isActorRunnable actors edges targetReps q currentBuffer) actorIndices

  if not (or runnableFlags)
    then Right (q, presActorInstances, presEdges)
    else do
      -- Fire runnable actors in index order
      (nextBuf, nextQ, nextInstances, nextEdges) <-
        foldl'
          ( \accEither actorIdx -> do
              (bufAcc, qAcc, instAcc, edgeAcc) <- accEither
              canRun <- isActorRunnable actors edges targetReps qAcc bufAcc actorIdx
              if not canRun
                then Right (bufAcc, qAcc, instAcc, edgeAcc)
                else do
                  let actor = actors !! actorIdx
                      -- Create the (q_\alpha(i) + 1)-th instance of actor \alpha
                      j = (qAcc V.! actorIdx) + 1
                      newInstance = (name actor, j)

                      -- For each input edge 'a' on \alpha, establish precedence links
                      inEdgeIdxs = incomingEdgeIndices actors edges actorIdx
                      newEdgesForAlpha =
                        concatMap
                          ( \edgeIdx ->
                              let edge = edges !! edgeIdx
                                  etaName = name (src edge)
                                  dRaw = dFunction j (cons edge) (initTokens edge) (prod edge)
                                  d = max 0 dRaw
                               in [((etaName, k), newInstance) | k <- [1 .. d]]
                          )
                          inEdgeIdxs

                      -- Update buffer state: b(i+1) = b(i) + Gamma * v(i)
                      updatedBuf = fireOnce actors edges actorIdx bufAcc

                      -- Update instance count vector q(i)
                      updatedQ = qAcc V.// [(actorIdx, j)]

                  Right
                    ( updatedBuf,
                      updatedQ,
                      newInstance : instAcc,
                      reverse newEdgesForAlpha ++ edgeAcc
                    )
          )
          (Right (currentBuffer, q, presActorInstances, presEdges))
          actorIndices

      -- Repeat simulation loop
      helperAPG nextBuf nextQ actors edges _matrix targetReps nextInstances nextEdges

--------------------------------------------------------------------------------
-- 4. Hu-Level Calculation on the APG
--------------------------------------------------------------------------------

-- | Computes the Hu-level (critical path length to a sink) for every instance in the APG.
--   Works for arbitrary DAGs, including disconnected components and multiple sinks.
computeHuLevels ::
  [ActorInstance] ->                       -- ^ APG Vertices (in topological order)
  [PrecedenceEdge] ->                      -- ^ APG Directed Precedence Edges
  (IRId -> Int) ->                         -- ^ Execution cost function for actors (use (\_ -> 1) for unit time)
  Map.Map ActorInstance Int                -- ^ Map of (ActorInstance -> Hu-level)
computeHuLevels instances presEdges actorCost =
  -- Fold over the topologically sorted instances in REVERSE order.
  -- Reversing topological order guarantees all successors of a node
  -- are processed BEFORE the node itself.
  foldl' processNode Map.empty (reverse instances)
  where
    -- 1. Build the adjacency list: Producer -> [Consumers]
    succMap :: Map.Map ActorInstance [ActorInstance]
    succMap =
      foldl'
        (\acc (srcInst, dstInst) -> Map.insertWith (++) srcInst [dstInst] acc)
        (Map.fromList [(inst, []) | inst <- instances])
        presEdges

    -- 2. Compute level for a single node whose children have already been computed
    processNode :: Map.Map ActorInstance Int -> ActorInstance -> Map.Map ActorInstance Int
    processNode levelMap inst@(actorId, _) =
      let cost = actorCost actorId
          successors = succMap Map.! inst
          nodeLevel =
            case successors of
              [] -> cost -- Sink node
              succs -> cost + maximum [levelMap Map.! s | s <- succs]
       in Map.insert inst nodeLevel levelMap

--------------------------------------------------------------------------------
-- 5. Multi-Core List Scheduling with Hu-Levels
--------------------------------------------------------------------------------

-- | List Scheduling using Hu-levels as the priority metric onto `numCores` parallel processors.
listSchedulingHuLevel ::
  Int ->                                   -- ^ Number of available cores (e.g. 2)
  [ActorInstance] ->                       -- ^ APG Vertices (firings)
  [PrecedenceEdge] ->                      -- ^ APG Edges (precedences)
  (IRId -> Int) ->                         -- ^ Actor execution duration function
  Either String [ScheduledFiring]          -- ^ List of scheduled firings with core and time assignments
listSchedulingHuLevel numCores instances presEdges actorCost
  | numCores <= 0 = Left "Number of cores must be strictly positive"
  | null instances = Right []
  | otherwise =
      let -- 1. Precompute Hu-levels for priority ranking
          huLevels = computeHuLevels instances presEdges actorCost

          -- 2. Build map of outgoing edges: Producer -> [Consumers]
          succMap =
            foldl'
              (\m (s, d) -> Map.insertWith (++) s [d] m)
              (Map.fromList [(inst, []) | inst <- instances])
              presEdges

          -- 3. Calculate initial in-degrees (number of unmet dependencies) for all instances
          initInDegrees =
            foldl'
              (\m (_, d) -> Map.adjust (+ 1) d m)
              (Map.fromList [(inst, 0) | inst <- instances])
              presEdges

          -- 4. Initial pool of ready tasks: tasks with inDegree == 0 (Sources of all components)
          initReady =
            [ inst
            | (inst, deg) <- Map.toList initInDegrees,
              deg == 0
            ]

          -- 5. Initially, all cores are idle
          initIdleCores = [0 .. numCores - 1]

       in scheduleLoop 0 initIdleCores [] initInDegrees initReady [] succMap huLevels
  where
    -- Discrete-event scheduling loop stepping forward through time
    scheduleLoop ::
      Int ->                                     -- ^ Current simulation time (currentTime)
      [Int] ->                                   -- ^ Idle core IDs
      [ActiveTask] ->                            -- ^ Tasks currently running on cores
      Map.Map ActorInstance Int ->               -- ^ Remaining in-degrees (unmet dependency count)
      [ActorInstance] ->                         -- ^ Ready queue (tasks whose dependencies are all met)
      [ScheduledFiring] ->                       -- ^ Accumulator of scheduled firings
      Map.Map ActorInstance [ActorInstance] ->   -- ^ Successor map
      Map.Map ActorInstance Int ->               -- ^ Hu-levels lookup map
      Either String [ScheduledFiring]
    scheduleLoop currentTime idleCores activeTasks inDegrees readyQueue schedAcc succs levels =
      let -- Check if any running tasks finish at or before currentTime
          (finishedTasks, runningTasks) =
            partition (\at -> atFinishTime at <= currentTime) activeTasks
       in if not (null finishedTasks)
            then
              -- A task has completed!
              -- 1. Free its core
              let newlyFreedCores = map atCore finishedTasks
                  updatedIdleCores = idleCores ++ newlyFreedCores

                  -- 2. Decrement in-degree for all downstream consumers of completed tasks
                  finishedInstances = map atInstance finishedTasks
                  allConsumers = concatMap (\inst -> Map.findWithDefault [] inst succs) finishedInstances

                  (updatedInDegrees, newReadyTasks) =
                    foldl'
                      ( \(degMap, newlyReady) consumer ->
                          let newDeg = (degMap Map.! consumer) - 1
                              newMap = Map.insert consumer newDeg degMap
                           in if newDeg == 0
                                then (newMap, consumer : newlyReady)
                                else (newMap, newlyReady)
                      )
                      (inDegrees, [])
                      allConsumers

                  updatedReadyQueue = readyQueue ++ newReadyTasks
               in -- Re-evaluate at currentTime with the newly available cores and tasks
                  scheduleLoop
                    currentTime
                    updatedIdleCores
                    runningTasks
                    updatedInDegrees
                    updatedReadyQueue
                    schedAcc
                    succs
                    levels
            else
              -- No tasks finished at this exact tick. Can we assign any ready tasks to idle cores?
              if not (null idleCores) && not (null readyQueue)
                then
                  let -- Sort ready queue by Hu-level DESCENDING (highest priority first).
                      -- Ties broken deterministically by (Actor ID, Instance #).
                      sortedReady =
                        sortBy
                          ( \a b ->
                              case compare (levels Map.! b) (levels Map.! a) of
                                EQ -> compare a b
                                other -> other
                          )
                          readyQueue

                      chosenTask@(chosenActor, _) = head sortedReady
                      remainingReady = tail sortedReady

                      freeCore = head idleCores
                      remainingIdleCores = tail idleCores

                      duration = actorCost chosenActor
                      startTime = currentTime
                      finishTime = currentTime + duration

                      newFiring =
                        ScheduledFiring
                          { sfCore = freeCore,
                            sfInstance = chosenTask,
                            sfStartTime = startTime,
                            sfEndTime = finishTime
                          }

                      newActive =
                        ActiveTask
                          { atFinishTime = finishTime,
                            atCore = freeCore,
                            atInstance = chosenTask
                          }
                   in -- Keep assigning in the same tick if more idle cores exist
                      scheduleLoop
                        currentTime
                        remainingIdleCores
                        (newActive : runningTasks)
                        inDegrees
                        remainingReady
                        (newFiring : schedAcc)
                        succs
                        levels
                else
                  -- No assignments possible right now. Check termination, deadlock, or advance time.
                  if length schedAcc == length instances && null runningTasks
                    then Right (reverse schedAcc)
                    else
                      if null runningTasks && null readyQueue
                        then Left "Scheduler deadlock: cyclic dependency or unreachable tasks detected in APG."
                        else
                          -- Advance simulation clock to the earliest completion time among active tasks
                          let nextTime = minimum (map atFinishTime runningTasks)
                           in scheduleLoop
                                nextTime
                                idleCores
                                runningTasks
                                inDegrees
                                readyQueue
                                schedAcc
                                succs
                                levels

-- | Groups the flat scheduled firings by Core ID, ordered chronologically.
toPerCoreSchedule :: Int -> [ScheduledFiring] -> MultiCoreSchedule
toPerCoreSchedule numCores firings =
  let emptyMap = Map.fromList [(c, []) | c <- [0 .. numCores - 1]]
      sorted = sortBy (comparing sfStartTime) firings
   in foldl'
        (\acc sf -> Map.adjust (++ [sfInstance sf]) (sfCore sf) acc)
        emptyMap
        sorted
