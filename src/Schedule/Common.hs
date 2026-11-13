{- |
Module      : Schedule.Common
Description : Shared data types, graph primitives, topology matrix mathematics,
              and buffer simulation utilities common to all SDF scheduling strategies.
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Schedule.Common
  ( -- * Core Data Types
    Actor (..),
    Edge (..),
    ActorInstance,
    SchedulingStrategy (..),
    Schedule (Schedule, ..),
    ScheduleMetrics (..),
    SDFAnalysis (..),

    -- * Schedule Utilities
    toFlatSchedule,
    tagInstances,

    -- * Graph Primitives
    incomingEdgeIndices,
    outgoingEdgeIndices,
    updateAt,
    fireOnce,
    convertIRSystem,

    -- * Topology Matrix & Nullspace Analysis
    buildTopologyMatrixEdgesRows,
    rowReduce,
    matrixRank,
    nullspaceBasis,
    nullspaceVectorToString,
    toMinimalIntegers,
    multiplyMatrixVector,
    matrixEdgesRowsToString,
    analyzeSDF,

    -- * Buffer Simulation & Sizing
    simulateBufferUsage,
    computeIOBufferSizes,
    getBuffers,
  )
where

import Data.List (elemIndex, find, mapAccumL, nub)
import qualified Data.Map.Strict as Map
import Data.Ratio (denominator, numerator)
import ForSyDeIR
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

-- | Represents an individual actor firing instance: (ActorId, 1-based instance index).
type ActorInstance = (IRId, Int)

-- | Performance and scheduling metrics.
data ScheduleMetrics = ScheduleMetrics
  { totalWork :: Int,
    criticalPath :: Int,
    makespan :: Int,
    actualSpeedup :: Double,
    theoreticalSpeedup :: Double
  }
  deriving (Show, Eq)

-- | Available scheduling strategies.
data SchedulingStrategy
  = Greedy
  | HuLevel Int -- ^ Number of target cores
  deriving (Show, Eq)

-- | The unified Schedule representation.
--   'schedCores' is a 2D matrix (list of lists):
--     - In single-core, schedCores has 1 row: [[inst1, inst2, ...]]
--     - In multi-core, schedCores has P rows: [[core0_firings], [core1_firings], ...]
data Schedule = ScheduleRecord
  { schedStrategy :: SchedulingStrategy,
    schedCores :: [[ActorInstance]],
    schedBufferSizes :: [(IRId, Int)],
    schedDelayBuffers :: [(IRId, IRId)],
    schedMetrics :: Maybe ScheduleMetrics
  }
  deriving (Show, Eq)

-- | Pattern synonym providing backwards compatibility for legacy (scheduleList, buffers, delayBuffers) tuple.
pattern Schedule :: ([IRId], [(IRId, Int)], [(IRId, IRId)]) -> Schedule
pattern Schedule tuple <- (toLegacyTuple -> tuple)
  where
    Schedule (s, b, d) = fromLegacyTuple s b d

toLegacyTuple :: Schedule -> ([IRId], [(IRId, Int)], [(IRId, IRId)])
toLegacyTuple s = (toFlatSchedule s, schedBufferSizes s, schedDelayBuffers s)

fromLegacyTuple :: [IRId] -> [(IRId, Int)] -> [(IRId, IRId)] -> Schedule
fromLegacyTuple s b d =
  ScheduleRecord
    { schedStrategy = Greedy,
      schedCores = [tagInstances s],
      schedBufferSizes = b,
      schedDelayBuffers = d,
      schedMetrics = Nothing
    }

-- | Flattens the core matrix into a flat list of actor IDs.
toFlatSchedule :: Schedule -> [IRId]
toFlatSchedule = map fst . concat . schedCores

-- | Tags each occurrence of an actor ID with a 1-based sequential instance counter.
tagInstances :: [IRId] -> [ActorInstance]
tagInstances = snd . mapAccumL assignCount Map.empty
  where
    assignCount counts actorId =
      let count = Map.findWithDefault 0 actorId counts + 1
       in (Map.insert actorId count counts, (actorId, count))

-- | Unified analysis of an SDF system.
data SDFAnalysis
  = EmptyGraph
      { saActors :: [Actor],
        saEdges :: [Edge],
        saIOBuffers :: [(IRId, Int)],
        saAliases :: [(IRId, IRId)]
      }
  | AnalyzedSDF
      { saActors :: [Actor],
        saEdges :: [Edge],
        saMatrix :: [[Integer]],
        saBasisVec :: [Rational],
        saRepInt :: [Integer],
        saRepCounts :: [Int],
        saRepsWithNames :: [(IRId, Int)],
        saDelayBuffers :: [(IRId, IRId)],
        saIOBuffers :: [(IRId, Int)],
        saAliases :: [(IRId, IRId)]
      }
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- 2. Graph Primitives & IR Conversion
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
      tokensAfterConsume = foldl consume tokens incomingEdges
      tokensAfterProduce = foldl produce tokensAfterConsume outgoingEdges
   in tokensAfterProduce
  where
    consume currentTokens edgeIndex =
      let consumption = cons (edges !! edgeIndex)
       in updateAt edgeIndex (subtract consumption) currentTokens

    produce currentTokens edgeIndex =
      let production = prod (edges !! edgeIndex)
       in updateAt edgeIndex (+ production) currentTokens

-- | Convert ForSyDe IR to SDF data structures.
convertIRSystem :: IRSystem -> Either String ([Actor], [Edge])
convertIRSystem (IRSystem (inputNames, outputNames) constructors signals _) = do
  let delayNames = [n | IRDelay n _ (_, _) <- constructors]

      allActorNames =
        [ n
        | IRActor n _ _ (_, _) <- constructors
        ]

      inputActorNames =
        [ dstId
        | IRSignal _ (srcId, _) (dstId, _) <- signals,
          srcId `elem` inputNames
        ]

      baseActors =
        [ Actor n (n `elem` inputActorNames || n `elem` inputNames)
        | n <- nub allActorNames
        ]

      findActorByName n =
        case find (\a -> name a == n) baseActors of
          Just a -> pure a
          Nothing -> Left ("Actor not found: " ++ show n)

      normalSigs = filter f signals
        where
          f (IRSignal _ (srcId, _) (dstId, _)) =
            srcId `notElem` inputNames
              && dstId `notElem` outputNames
              && srcId `notElem` delayNames
              && dstId `notElem` delayNames

      makeNormalEdges
        (IRSignal signalId (srcId, prodRate) (dstId, consRate)) = do
          srcActor <- findActorByName srcId
          dstActor <- findActorByName dstId
          pure $ Edge signalId srcActor dstActor prodRate consRate False 0 Nothing

      makeDelayEdge delayName =
        let getDelayToken acc e =
              case (e, acc) of
                (IRDelay dName tokens _, Left _) | dName == delayName -> pure tokens
                (_, a) -> a
            incoming =
              [ (signalId, srcId, prodRate)
              | IRSignal signalId (srcId, prodRate) (dstId, _) <- signals,
                dstId == delayName
              ]
            outgoing =
              [ (signalId, dstId, consRate)
              | IRSignal signalId (srcId, _) (dstId, consRate) <- signals,
                srcId == delayName
              ]
         in case (incoming, outgoing) of
              ([(inSignalId, _, _)], _) | inSignalId `elem` inputNames -> pure []
              (_, [(outSignalId, _, _)]) | outSignalId `elem` outputNames -> pure []
              ([(inSignalId, srcIn, prodIn)], [(outSignalId, dstOut, consOut)]) -> do
                srcActor <- findActorByName srcIn
                dstActor <- findActorByName dstOut
                delayTokens <-
                  foldl
                    getDelayToken
                    (Left $ "Delay node " ++ show delayName ++ " not found")
                    constructors
                pure
                  [ Edge
                      inSignalId
                      srcActor
                      dstActor
                      prodIn
                      consOut
                      True
                      (length delayTokens)
                      (Just [(inSignalId, inSignalId), (outSignalId, inSignalId)])
                  ]
              ([], _) ->
                Left $ "Delay node " ++ show delayName ++ " has no input signal."
              (_, []) ->
                Left $ "Delay node " ++ show delayName ++ " has no output signal."
              _ ->
                Left $ "Delay node " ++ show delayName ++ " must have exactly one input and one output."

      normalizeSelfLoop e@(Edge edgeNameValue srcActor dstActor prodRate consRate _ _ _)
        | name srcActor == name dstActor =
            if prodRate /= consRate
              then
                Left $
                  "Invalid self-loop on actor "
                    ++ show (name srcActor)
                    ++ " (edge: "
                    ++ show edgeNameValue
                    ++ ")"
                    ++ ": prod="
                    ++ show prodRate
                    ++ ", cons="
                    ++ show consRate
              else pure e
        | otherwise = pure e

  delayEdges <- mconcat <$> sequence (map makeDelayEdge delayNames)
  normalEdges <- sequence $ map makeNormalEdges normalSigs
  finalEdges <- sequence $ map normalizeSelfLoop (normalEdges ++ delayEdges)
  pure (baseActors, finalEdges)

--------------------------------------------------------------------------------
-- 3. Topology Matrix & Nullspace Analysis
--------------------------------------------------------------------------------

buildTopologyMatrixEdgesRows :: [Actor] -> [Edge] -> [[Integer]]
buildTopologyMatrixEdgesRows actors edges =
  [[topo actor edge | actor <- actors] | edge <- edges]
  where
    topo actor edge
      | src edge == dst edge = 0
      | src edge == actor = fromIntegral (prod edge)
      | dst edge == actor = -fromIntegral (cons edge)
      | otherwise = 0

rowReduce :: [[Rational]] -> ([[Rational]], [Int])
rowReduce rowsIn = go rowsIn 0 []
  where
    nCols = case rowsIn of
      [] -> 0
      (r : _) -> length r
    go rows col pivots
      | col >= nCols = (rows, reverse pivots)
      | otherwise =
          let (done, rest) = splitAt (length pivots) rows
           in case break (\r -> r !! col /= 0) rest of
                (_, []) -> go rows (col + 1) pivots
                (above, pivotRow : below) ->
                  let normalized = map (/ (pivotRow !! col)) pivotRow
                      eliminate r =
                        let factor = r !! col
                         in if factor == 0
                              then r
                              else zipWith (\a b -> a - factor * b) r normalized
                   in go
                        (map eliminate done ++ [normalized] ++ map eliminate (above ++ below))
                        (col + 1)
                        (col : pivots)

matrixRank :: [[Integer]] -> Int
matrixRank = length . snd . rowReduce . map (map fromInteger)

nullspaceBasis :: [[Integer]] -> [[Rational]]
nullspaceBasis mat =
  let (rref, pivots) = rowReduce (map (map fromInteger) mat)
      nCols = case mat of
        [] -> 0
        (r : _) -> length r
      freeCols = [c | c <- [0 .. nCols - 1], c `notElem` pivots]
      entry freeCol col
        | col == freeCol = 1
        | otherwise = case elemIndex col pivots of
            Just i -> negate (rref !! i !! freeCol)
            Nothing -> 0
   in [[entry f c | c <- [0 .. nCols - 1]] | f <- freeCols]

toMinimalIntegers :: [Rational] -> [Integer]
toMinimalIntegers xs =
  let commonDenom = foldl lcm 1 (map denominator xs)
      ints = [numerator x * (commonDenom `div` denominator x) | x <- xs]
      gcdAll = foldl gcd 0 ints
      reduced = if gcdAll == 0 then ints else map (`div` gcdAll) ints
   in if all (< 0) reduced then map negate reduced else reduced

multiplyMatrixVector :: [[Integer]] -> [Integer] -> [Integer]
multiplyMatrixVector mat vec = map (sum . zipWith (*) vec) mat

matrixEdgesRowsToString :: [Edge] -> [Actor] -> [[Integer]] -> String
matrixEdgesRowsToString edges actors topoMatrix =
  let actorNameWidths = map (length . show . name) actors
      rowValueWidths = map (maximum . map (length . show)) topoMatrix
      colWidths = zipWith max actorNameWidths rowValueWidths
      totalColWidths = map (+ 2) colWidths
      edgeLabelWidth = maximum (map (length . show . edgeName) edges)

      header =
        pad (edgeLabelWidth + 2) "Edge\\Actor"
          ++ concatMap (\(actor, width) -> pad width (show $ name actor)) (zip actors totalColWidths)

      totalWidth = edgeLabelWidth + 2 + sum totalColWidths
      separator = replicate totalWidth '-'

      matrixRows = map (rowToString edgeLabelWidth totalColWidths) (zip edges topoMatrix)
   in "Topology Matrix (Edges × Actors):\n"
        ++ header
        ++ "\n"
        ++ separator
        ++ "\n"
        ++ unlines matrixRows
  where
    pad width str = take width (str ++ repeat ' ')
    rowToString labelWidth colWidths (edge, rowValues) =
      pad (labelWidth + 2) (show $ edgeName edge)
        ++ concatMap
          (\(val, width) -> pad width (show val))
          (zip rowValues colWidths)

nullspaceVectorToString :: [Rational] -> String
nullspaceVectorToString = unlines . map formatRational
  where
    formatRational x
      | denominator x == 1 = show (numerator x)
      | otherwise = show (numerator x) ++ "/" ++ show (denominator x)

-- | Validates and analyzes an IRSystem, computing the topology matrix,
-- rank, repetition vector, delay buffers, and I/O buffer requirements.
analyzeSDF :: IRSystem -> Either String SDFAnalysis
analyzeSDF irSystem = do
  (actors, edges) <- convertIRSystem irSystem
  if null edges
    then do
      let repsWithNames = zip (map name actors) (replicate (length actors) 1)
      (ioBufSizes, aliases) <- computeIOBufferSizes irSystem repsWithNames
      pure $ EmptyGraph actors edges ioBufSizes aliases
    else do
      let mat = buildTopologyMatrixEdgesRows actors edges
          rankMat = matrixRank mat
      if rankMat /= length actors - 1
        then Left "Matrix rank is not equal to number of actors minus one. Cannot compute repetition vector."
        else case nullspaceBasis mat of
          [] -> Left "No repetition vector found: null space is empty."
          (basisVec : _) -> do
            let repInt = toMinimalIntegers basisVec
                repCounts = map fromIntegral repInt :: [Int]
                repsWithNames = zip (map name actors) repCounts
                delayBuffers = getBuffers edges
            (ioBufSizes, aliases) <- computeIOBufferSizes irSystem repsWithNames
            pure $
              AnalyzedSDF
                { saActors = actors,
                  saEdges = edges,
                  saMatrix = mat,
                  saBasisVec = basisVec,
                  saRepInt = repInt,
                  saRepCounts = repCounts,
                  saRepsWithNames = repsWithNames,
                  saDelayBuffers = delayBuffers,
                  saIOBuffers = ioBufSizes,
                  saAliases = aliases
                }

--------------------------------------------------------------------------------
-- 4. Buffer Sizing & Simulation
--------------------------------------------------------------------------------

simulateBufferUsage :: [Actor] -> [Edge] -> [Int] -> [Int] -> [(IRId, Int)]
simulateBufferUsage actors edges initialTokens schedule =
  let edgeNames = map edgeName edges
      simulateStep (currentTokens, maxBuffer) actorIndex =
        let incomingEdges = incomingEdgeIndices actors edges actorIndex
            outgoingEdges = outgoingEdgeIndices actors edges actorIndex
            tokensAfterConsume = foldl consume currentTokens incomingEdges
            tokensAfterProduce = foldl produce tokensAfterConsume outgoingEdges
            newMaxBuffer = zipWith max maxBuffer tokensAfterProduce
         in (tokensAfterProduce, newMaxBuffer)
      (_, finalMaxBuffer) = foldl simulateStep (initialTokens, initialTokens) schedule
   in zip edgeNames finalMaxBuffer
  where
    consume tokens edgeIdx =
      let consumption = cons (edges !! edgeIdx)
       in updateAt edgeIdx (subtract consumption) tokens
    produce tokens edgeIdx =
      let production = prod (edges !! edgeIdx)
       in updateAt edgeIdx (+ production) tokens

computeIOBufferSizes :: IRSystem -> [(IRId, Int)] -> Either String ([(IRId, Int)], [(IRId, IRId)])
computeIOBufferSizes (IRSystem (inputs, outputs) constructors signals _) repsWithNames =
  let inputSignals = [s | s@(IRSignal _ (srcId, _) (_, _)) <- signals, srcId `elem` inputs]
      inputEdges = foldr f (Right []) $ map nonDelayInputSignal inputSignals
        where
          f el acc = do
            a <- acc
            (IRSignal signalId (srcId, _) (dstId, dstRate), aliases, nTokens) <- el
            if srcId `elem` inputs
              then do
                dstReps <- findActorRep dstId
                pure $ (signalId, dstRate, dstReps, aliases, nTokens) : a
              else acc

      nonDelayInputSignal s@(IRSignal sigId (srcId, srcRate) (dstId, _)) =
        case find (\c -> constructorName c == dstId) constructors of
          Just (IRActor _ _ _ _) -> pure (s, [], 0)
          Just (IRDelay dName tokens (_, next)) | dstId == dName ->
            case find (\(IRSignal sId _ _) -> sId == next) signals of
              Just (IRSignal sigAlias (_, _) (newDstId, newDstRate)) -> do
                (sig, aliases, nTokens) <- nonDelayInputSignal (IRSignal sigId (srcId, srcRate) (newDstId, newDstRate))
                pure (sig, (sigId, sigId) : (sigAlias, sigId) : aliases, nTokens + length tokens)
              Nothing -> Left $ "Could not find next signal " ++ show next
          _ -> Left $ "Could not find the process " ++ show srcId

      outputSignals = [s | s@(IRSignal _ (_, _) (dstId, _)) <- signals, dstId `elem` outputs]
      outputEdges = foldr f (Right []) $ map nonDelayOutputSignal outputSignals
        where
          f el acc = do
            a <- acc
            (IRSignal signalId (srcId, srcRate) (dstId, _), aliases, nTokens) <- el
            if dstId `elem` outputs
              then do
                srcReps <- findActorRep srcId
                pure $ (signalId, srcRate, srcReps, aliases, nTokens) : a
              else acc

      nonDelayOutputSignal s@(IRSignal sigId (srcId, _) (dstId, dstRate)) =
        case find (\c -> constructorName c == srcId) constructors of
          Just (IRActor _ _ _ _) -> pure (s, [], 0)
          Just (IRDelay dName tokens (next, _)) | srcId == dName ->
            case find (\(IRSignal sId _ _) -> sId == next) signals of
              Just (IRSignal sigAlias (newSrcId, newSrcRate) (_, _)) -> do
                (sig, aliases, nTokens) <- nonDelayOutputSignal (IRSignal sigId (newSrcId, newSrcRate) (dstId, dstRate))
                pure (sig, (sigId, sigId) : (sigAlias, sigId) : aliases, nTokens + length tokens)
              Nothing -> Left $ "Could not find next signal " ++ show next
          _ -> Left $ "Could not find the process " ++ show srcId

      constructorName = \case
        IRActor cName _ _ _ -> cName
        IRDelay cName _ _ -> cName

      findActorRep actorName =
        case lookup actorName repsWithNames of
          Just rep -> pure rep
          Nothing -> Left $ "Actor " ++ show actorName ++ " not found in repetition counts"

      foldBuffer (sigId, rate, rep, aliases, nTokens) (sigAcc, aliasAcc) =
        ((sigId, max (rate * rep) nTokens) : sigAcc, aliases ++ aliasAcc)
   in do
        inEdges <- inputEdges
        outEdges <- outputEdges
        pure $ foldr foldBuffer ([], []) $ inEdges ++ outEdges

getBuffers :: [Edge] -> [(IRId, IRId)]
getBuffers edges =
  concatMap
    ( \e ->
        case buffers e of
          Just bufList -> bufList
          Nothing -> []
    )
    edges
