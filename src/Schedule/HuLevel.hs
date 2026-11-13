{- |
Module      : Schedule.HuLevel
Description : Multi-core static list scheduler based on Acyclic Precedence Graphs (APG)
              and Hu's level-based priority algorithm.
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Schedule.HuLevel
  ( -- * Core APG Data Types
    ScheduledFiring (..),
    ActiveTask (..),
    MultiCoreSchedule,
    PrecedenceEdge,

    -- * SDF to APG Construction
    dFunction,
    isActorRunnable,
    createAcyclicPresedenceGraph,
    helperAPG,

    -- * Multi-Core List Scheduling
    computeHuLevels,
    listSchedulingHuLevel,
    toPerCoreSchedule,

    -- * Generator and Pretty-Printer
    generateHuLevelSchedule,
    prettyHuLevelSchedule,
  )
where

import Data.List (elemIndex, intercalate, partition, sortBy)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import qualified Data.Vector as V
import ForSyDeIR (IRId, IRSystem)
import GHC.Generics (Generic)
import Schedule.Common

--------------------------------------------------------------------------------
-- 1. Core Data Types
--------------------------------------------------------------------------------

-- | Represents a directed precedence dependency: (Producer Instance, Consumer Instance).
--   Example: (("actor_a", 1), ("actor_b", 1)) means A^1 must finish before B^1 starts.
type PrecedenceEdge = (ActorInstance, ActorInstance)

-- | Represents a scheduled execution of an actor firing on a specific core.
data ScheduledFiring = ScheduledFiring
  { sfCore :: Int,
    sfInstance :: ActorInstance,
    sfStartTime :: Int,
    sfEndTime :: Int
  }
  deriving (Show, Eq, Generic)

-- | A multi-core schedule mapping each core to its assigned sequence of firings.
type MultiCoreSchedule = Map.Map Int [ActorInstance]

-- | Internal helper type to track tasks currently in flight on a processor.
data ActiveTask = ActiveTask
  { atFinishTime :: Int,
    atCore :: Int,
    atInstance :: ActorInstance
  }
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- 2. SDF to APG Construction (Lee & Messerschmitt 1987)
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

-- | Check if a specific actor (by index) is runnable in the current simulation state.
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

-- | Constructs the Acyclic Precedence Graph (APG) for one period.
createAcyclicPresedenceGraph ::
  [Actor] ->
  [Edge] ->
  [[Integer]] ->
  [Int] ->
  Either String ([ActorInstance], [PrecedenceEdge])
createAcyclicPresedenceGraph actors edges _matrix repCounts = do
  let initBuffers = map initTokens edges
      q0 = V.replicate (length actors) 0
  (finalQ, instances, presEdges) <- helperAPG initBuffers q0 actors edges _matrix repCounts [] []
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
  runnableFlags <- mapM (isActorRunnable actors edges targetReps q currentBuffer) actorIndices

  if not (or runnableFlags)
    then Right (q, presActorInstances, presEdges)
    else do
      (nextBuf, nextQ, nextInstances, nextEdges) <-
        foldl
          ( \accEither actorIdx -> do
              (bufAcc, qAcc, instAcc, edgeAcc) <- accEither
              canRun <- isActorRunnable actors edges targetReps qAcc bufAcc actorIdx
              if not canRun
                then Right (bufAcc, qAcc, instAcc, edgeAcc)
                else do
                  let actor = actors !! actorIdx
                      j = (qAcc V.! actorIdx) + 1
                      newInstance = (name actor, j)
                      inEdgeIdxs = incomingEdgeIndices actors edges actorIdx
                      selfPrecedence = if j > 1 then [((name actor, j - 1), newInstance)] else []
                      newEdgesForAlpha =
                        selfPrecedence
                          ++ concatMap
                            ( \edgeIdx ->
                                let edge = edges !! edgeIdx
                                    etaName = name (src edge)
                                    dRaw = dFunction j (cons edge) (initTokens edge) (prod edge)
                                    d = max 0 dRaw
                                 in [((etaName, k), newInstance) | k <- [1 .. d]]
                            )
                            inEdgeIdxs
                      updatedBuf = fireOnce actors edges actorIdx bufAcc
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
      helperAPG nextBuf nextQ actors edges _matrix targetReps nextInstances nextEdges

--------------------------------------------------------------------------------
-- 3. Hu-Level Calculation on the APG
--------------------------------------------------------------------------------

-- | Computes the Hu-level (critical path length to a sink) for every instance in the APG.
computeHuLevels ::
  [ActorInstance] ->
  [PrecedenceEdge] ->
  (IRId -> Int) ->
  Map.Map ActorInstance Int
computeHuLevels instances presEdges actorCost =
  foldl processNode Map.empty (reverse instances)
  where
    succMap =
      foldl
        (\acc (srcInst, dstInst) -> Map.insertWith (++) srcInst [dstInst] acc)
        (Map.fromList [(inst, []) | inst <- instances])
        presEdges

    processNode levelMap inst@(actorId, _) =
      let cost = actorCost actorId
          successors = succMap Map.! inst
          nodeLevel =
            case successors of
              [] -> cost
              succs -> cost + maximum [levelMap Map.! s | s <- succs]
       in Map.insert inst nodeLevel levelMap

--------------------------------------------------------------------------------
-- 4. Multi-Core List Scheduling with Hu-Levels
--------------------------------------------------------------------------------

-- | List Scheduling using Hu-levels as the priority metric onto `numCores` parallel processors.
listSchedulingHuLevel ::
  Int ->
  [ActorInstance] ->
  [PrecedenceEdge] ->
  (IRId -> Int) ->
  Either String [ScheduledFiring]
listSchedulingHuLevel numCores instances presEdges actorCost
  | numCores <= 0 = Left "Number of cores must be strictly positive"
  | null instances = Right []
  | otherwise =
      let huLevels = computeHuLevels instances presEdges actorCost
          succMap =
            foldl
              (\m (s, d) -> Map.insertWith (++) s [d] m)
              (Map.fromList [(inst, []) | inst <- instances])
              presEdges
          initInDegrees :: Map.Map ActorInstance Int
          initInDegrees =
            foldl
              (\m (_, d) -> Map.adjust (+ (1 :: Int)) d m)
              (Map.fromList [(inst, 0 :: Int) | inst <- instances])
              presEdges
          initReady =
            [ inst
            | (inst, deg) <- Map.toList initInDegrees,
              deg == (0 :: Int)
            ]
          initIdleCores = [0 .. numCores - 1]
       in scheduleLoop (0 :: Int) initIdleCores [] initInDegrees initReady [] succMap huLevels Map.empty
  where
    scheduleLoop ::
      Int ->
      [Int] ->
      [ActiveTask] ->
      Map.Map ActorInstance Int ->
      [ActorInstance] ->
      [ScheduledFiring] ->
      Map.Map ActorInstance [ActorInstance] ->
      Map.Map ActorInstance Int ->
      Map.Map IRId Int ->
      Either String [ScheduledFiring]
    scheduleLoop currentTime idleCores activeTasks inDegrees readyQueue schedAcc succs levels actorAffinity =
      let (finishedTasks, runningTasks) =
            partition (\at -> atFinishTime at <= currentTime) activeTasks
       in if not (null finishedTasks)
            then
              let newlyFreedCores = map atCore finishedTasks
                  updatedIdleCores = idleCores ++ newlyFreedCores
                  finishedInstances = map atInstance finishedTasks
                  allConsumers = concatMap (\inst -> Map.findWithDefault [] inst succs) finishedInstances
                  (updatedInDegrees, newReadyTasks) =
                    foldl
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
               in scheduleLoop
                    currentTime
                    updatedIdleCores
                    runningTasks
                    updatedInDegrees
                    updatedReadyQueue
                    schedAcc
                    succs
                    levels
                    actorAffinity
            else
              let sortedReady =
                    sortBy
                      ( \a b ->
                          case compare (levels Map.! b) (levels Map.! a) of
                            EQ -> compare a b
                            other -> other
                      )
                      readyQueue
                  findCandidate [] = Nothing
                  findCandidate (c : cs) =
                    let compatible =
                          filter
                            ( \(actId, _) ->
                                case Map.lookup actId actorAffinity of
                                  Just assignedCore -> assignedCore == c
                                  Nothing -> True
                            )
                            sortedReady
                     in case compatible of
                          [] -> findCandidate cs
                          (task : _) -> Just (c, task)
               in case (if not (null idleCores) && not (null readyQueue) then findCandidate idleCores else Nothing) of
                    Just (freeCore, chosenTask@(chosenActor, _)) ->
                      let remainingReady = filter (/= chosenTask) readyQueue
                          remainingIdleCores = filter (/= freeCore) idleCores
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
                          updatedAffinity = Map.insert chosenActor freeCore actorAffinity
                       in scheduleLoop
                            currentTime
                            remainingIdleCores
                            (newActive : runningTasks)
                            inDegrees
                            remainingReady
                            (newFiring : schedAcc)
                            succs
                            levels
                            updatedAffinity
                    Nothing ->
                      if length schedAcc == length instances && null runningTasks
                        then Right (reverse schedAcc)
                        else
                          if null runningTasks
                            then Left "Scheduler deadlock: cyclic dependency or unreachable tasks detected in APG."
                            else
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
                                    actorAffinity

toPerCoreSchedule :: Int -> [ScheduledFiring] -> MultiCoreSchedule
toPerCoreSchedule numCores firings =
  let emptyMap = Map.fromList [(c, []) | c <- [0 .. numCores - 1]]
      sorted = sortBy (comparing sfStartTime) firings
   in foldl
        (\acc sf -> Map.adjust (++ [sfInstance sf]) (sfCore sf) acc)
        emptyMap
        sorted

--------------------------------------------------------------------------------
-- 5. Multi-Core Schedule Generator & Pretty-Printer
--------------------------------------------------------------------------------

generateHuLevelSchedule :: Int -> IRSystem -> Either String Schedule
generateHuLevelSchedule numCores irSystem
  | numCores <= 0 = Left "Number of cores must be greater than 0."
  | otherwise = do
      analysis <- analyzeSDF irSystem
      case analysis of
        EmptyGraph actors _ ioBufSizes aliases ->
          let coreMatrix = [tagInstances (map name actors)] ++ replicate (numCores - 1) []
           in pure $
                ScheduleRecord
                  { schedStrategy = HuLevel numCores,
                    schedCores = coreMatrix,
                    schedBufferSizes = ioBufSizes,
                    schedDelayBuffers = aliases,
                    schedMetrics = Nothing
                  }
        AnalyzedSDF actors edges mat _ _ repCounts _ delayBuffers ioBufSizes aliases -> do
          (instances, presEdges) <- createAcyclicPresedenceGraph actors edges mat repCounts
          scheduledFirings <- listSchedulingHuLevel numCores instances presEdges (\_ -> 1)
          let huLevels = computeHuLevels instances presEdges (\_ -> 1)
              perCoreMap = toPerCoreSchedule numCores scheduledFirings
              coreMatrix = [Map.findWithDefault [] c perCoreMap | c <- [0 .. numCores - 1]]

              orderedFirings = sortBy (comparing sfStartTime) scheduledFirings
              schedNames = map (fst . sfInstance) orderedFirings
              actorNameList = map name actors
              lookupIdx aName = case elemIndex aName actorNameList of
                Just idx -> idx
                Nothing -> error $ "Actor not found in actor list: " ++ show aName
              schedIdxs = map lookupIdx schedNames
              internalBufSizes = simulateBufferUsage actors edges (map initTokens edges) schedIdxs

              seqSteps = length instances
              makespanVal = if null scheduledFirings then 0 else maximum (map sfEndTime scheduledFirings)
              speedupVal = if makespanVal > 0 then fromIntegral seqSteps / fromIntegral makespanVal else 1.0
              criticalPathVal = if null huLevels then 0 else maximum (Map.elems huLevels)
              ceilWoverP = (seqSteps + numCores - 1) `div` numCores
              minPossibleMakespan = max criticalPathVal ceilWoverP
              theoreticalSpeedupVal =
                if minPossibleMakespan > 0
                  then fromIntegral seqSteps / fromIntegral minPossibleMakespan
                  else 1.0
              metrics =
                ScheduleMetrics
                  { totalWork = seqSteps,
                    criticalPath = criticalPathVal,
                    makespan = makespanVal,
                    actualSpeedup = speedupVal,
                    theoreticalSpeedup = theoreticalSpeedupVal
                  }

          pure $
            ScheduleRecord
              { schedStrategy = HuLevel numCores,
                schedCores = coreMatrix,
                schedBufferSizes = ioBufSizes ++ internalBufSizes,
                schedDelayBuffers = delayBuffers ++ aliases,
                schedMetrics = Just metrics
              }

prettyHuLevelSchedule :: Int -> IRSystem -> Schedule -> String
prettyHuLevelSchedule numCores irSystem sched =
  case analyzeSDF irSystem of
    Left e -> "Error: " ++ e ++ "\n"
    Right (EmptyGraph _ _ _ _) -> "No internal edges found. All actors fire once.\n"
    Right (AnalyzedSDF actors edges mat _ _ repCounts repsWithNames _ _ _) ->
      let matrixStr = matrixEdgesRowsToString edges actors mat
          repVecStr = intercalate "\n" ["  " ++ show a ++ " = " ++ show r | (a, r) <- repsWithNames]
       in case createAcyclicPresedenceGraph actors edges mat repCounts of
            Left err -> "Error constructing APG: " ++ err ++ "\n"
            Right (instances, presEdges) ->
              let huLevels = computeHuLevels instances presEdges (\_ -> 1)
                  huLevelStr = unlines ["  " ++ show (fst inst) ++ "^" ++ show (snd inst) ++ " : Hu-Level " ++ show lvl | (inst, lvl) <- Map.toList huLevels]

                  formatCore (c, coreFirings) =
                    let summary = intercalate ", " [show a ++ "^" ++ show idx | (a, idx) <- coreFirings]
                     in "Core " ++ show c ++ ":\n"
                          ++ "  Sequence: ["
                          ++ summary
                          ++ "]\n"

                  perCoreStr = unlines (map formatCore (zip [0 .. numCores - 1] (schedCores sched)))

                  metricsStr = case schedMetrics sched of
                    Nothing -> ""
                    Just m ->
                      let ceilWoverP = (totalWork m + numCores - 1) `div` numCores
                       in "------------------------------------------------------------\n"
                            ++ "Schedule Metrics:\n"
                            ++ "------------------------------------------------------------\n"
                            ++ "  Total Work (W)         : "
                            ++ show (totalWork m)
                            ++ " firings\n"
                            ++ "  Critical Path (L)      : "
                            ++ show (criticalPath m)
                            ++ " ticks\n"
                            ++ "  Min Theoretical Makespan: "
                            ++ show (max (criticalPath m) ceilWoverP)
                            ++ " ticks (max(L, ceil(W/P)) = max("
                            ++ show (criticalPath m)
                            ++ ", "
                            ++ show ceilWoverP
                            ++ "))\n"
                            ++ "  Actual Makespan        : "
                            ++ show (makespan m)
                            ++ " cycles / ticks\n"
                            ++ "  Actual Speedup         : "
                            ++ show ((fromIntegral (round (actualSpeedup m * 100) :: Int) / 100.0) :: Double)
                            ++ "x\n"
                            ++ "  Theoretical Max Speedup: "
                            ++ show ((fromIntegral (round (theoreticalSpeedup m * 100) :: Int) / 100.0) :: Double)
                            ++ "x (for "
                            ++ show numCores
                            ++ " cores)\n"

                  bufStr =
                    "------------------------------------------------------------\n"
                      ++ "Buffer Sizes:\n"
                      ++ "------------------------------------------------------------\n"
                      ++ unlines ["  " ++ show b ++ ": " ++ show sz | (b, sz) <- schedBufferSizes sched]
               in "============================================================\n"
                    ++ "           Multi-Core Periodic SDF Schedule (Hu-Level)      \n"
                    ++ "============================================================\n\n"
                    ++ "Target Cores: "
                    ++ show numCores
                    ++ "\n\n"
                    ++ matrixStr
                    ++ "\n\n"
                    ++ "Repetition Vector (Firings per period):\n"
                    ++ repVecStr
                    ++ "\n\n"
                    ++ "Acyclic Precedence Graph (APG):\n"
                    ++ "  Total firings (vertices)      : "
                    ++ show (length instances)
                    ++ "\n"
                    ++ "  Precedence constraints (edges): "
                    ++ show (length presEdges)
                    ++ "\n\n"
                    ++ "Hu-Levels (Critical path distance to sink):\n"
                    ++ huLevelStr
                    ++ "\n"
                    ++ "------------------------------------------------------------\n"
                    ++ "Schedule per Core:\n"
                    ++ "------------------------------------------------------------\n"
                    ++ perCoreStr
                    ++ metricsStr
                    ++ bufStr
