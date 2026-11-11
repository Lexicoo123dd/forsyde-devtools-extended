{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Schedule
  ( SchedulingStrategy (..),
    Schedule (Schedule, ..),
    ActorInstance,
    ScheduleMetrics (..),
    generateSchedule,
    computeScheduleAndBuffers,
    prettySchedule,
    toFlatSchedule,
    tagInstances,
  )
where

import APGSchedule
  ( Actor (..),
    Edge (..),
    ScheduledFiring (..),
    computeHuLevels,
    createAcyclicPresedenceGraph,
    listSchedulingHuLevel,
    toPerCoreSchedule,
  )
import Data.List (elemIndex, intercalate, mapAccumL, sortBy)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import ForSyDeIR (IRId, IRSystem (..))
import SDFSchedule
  ( buildTopologyMatrixEdgesRows,
    computeIOBufferSizes,
    convertIRSystem,
    getBuffers,
    greedySchedule,
    matrixEdgesRowsToString,
    matrixRank,
    multiplyMatrixVector,
    nullspaceBasis,
    nullspaceVectorToString,
    simulateBufferUsage,
    toMinimalIntegers,
  )

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

-- | Generates a unified Schedule given a scheduling strategy and an IRSystem.
generateSchedule :: SchedulingStrategy -> IRSystem -> Either String Schedule
generateSchedule strategy irSystem = case strategy of
  Greedy -> generateGreedySchedule irSystem
  HuLevel numCores -> generateHuLevelSchedule numCores irSystem

-- | Backwards-compatible alias for the default greedy single-core scheduler.
computeScheduleAndBuffers :: IRSystem -> Either String Schedule
computeScheduleAndBuffers = generateSchedule Greedy

--------------------------------------------------------------------------------
-- Single-Core Greedy Schedule Generator
--------------------------------------------------------------------------------

generateGreedySchedule :: IRSystem -> Either String Schedule
generateGreedySchedule irSystem = do
  (actors, edges) <- convertIRSystem irSystem
  if null edges
    then do
      let schedNames = map name actors
          repsWithNames = zip schedNames (replicate (length actors) 1)
          internalBufSizes = zip (map edgeName edges) (replicate (length edges) 0)
      (ioBufSizes, aliases) <- computeIOBufferSizes irSystem repsWithNames
      pure $
        ScheduleRecord
          { schedStrategy = Greedy,
            schedCores = [tagInstances schedNames],
            schedBufferSizes = ioBufSizes ++ internalBufSizes,
            schedDelayBuffers = aliases,
            schedMetrics = Nothing
          }
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
            schedIdxs <- greedySchedule actors edges repCounts
            let schedNames = map (name . (actors !!)) schedIdxs
                internalBufSizes = simulateBufferUsage actors edges (map initTokens edges) schedIdxs
            (ioBufSizes, aliases) <- computeIOBufferSizes irSystem repsWithNames
            pure $
              ScheduleRecord
                { schedStrategy = Greedy,
                  schedCores = [tagInstances schedNames],
                  schedBufferSizes = ioBufSizes ++ internalBufSizes,
                  schedDelayBuffers = delayBuffers ++ aliases,
                  schedMetrics = Nothing
                }

--------------------------------------------------------------------------------
-- Multi-Core Hu-Level List Schedule Generator
--------------------------------------------------------------------------------

generateHuLevelSchedule :: Int -> IRSystem -> Either String Schedule
generateHuLevelSchedule numCores irSystem
  | numCores <= 0 = Left "Number of cores must be greater than 0."
  | otherwise = do
      (actors, edges) <- convertIRSystem irSystem
      if null edges
        then do
          let schedNames = map name actors
              repsWithNames = zip schedNames (replicate (length actors) 1)
              internalBufSizes = zip (map edgeName edges) (replicate (length edges) 0)
          (ioBufSizes, aliases) <- computeIOBufferSizes irSystem repsWithNames
          let coreMatrix = [tagInstances schedNames] ++ replicate (numCores - 1) []
          pure $
            ScheduleRecord
              { schedStrategy = HuLevel numCores,
                schedCores = coreMatrix,
                schedBufferSizes = ioBufSizes ++ internalBufSizes,
                schedDelayBuffers = aliases,
                schedMetrics = Nothing
              }
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

                (ioBufSizes, aliases) <- computeIOBufferSizes irSystem repsWithNames

                let seqSteps = length instances
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

--------------------------------------------------------------------------------
-- Pretty Printing
--------------------------------------------------------------------------------

-- | Pretty-prints a schedule according to its strategy and the IRSystem.
prettySchedule :: IRSystem -> Schedule -> String
prettySchedule irSystem sched = case schedStrategy sched of
  Greedy -> prettyGreedySchedule irSystem sched
  HuLevel numCores -> prettyHuLevelSchedule numCores irSystem sched

prettyGreedySchedule :: IRSystem -> Schedule -> String
prettyGreedySchedule irSystem sched =
  case convertIRSystem irSystem of
    Left e -> "Error: " ++ e ++ "\n"
    Right (actors, edges) ->
      if null edges
        then
          let schedNames = toFlatSchedule sched
           in "No internal edges found.\n\n"
                ++ "Schedule (all actors fire once):\n"
                ++ intercalate ", " (map show schedNames)
                ++ "\n\nBuffer sizes:\n"
                ++ unlines ["  " ++ show b ++ ": " ++ show sz | (b, sz) <- schedBufferSizes sched]
        else
          let mat = buildTopologyMatrixEdgesRows actors edges
              matrixStr = matrixEdgesRowsToString edges actors mat
              rankMat = matrixRank mat
              header =
                matrixStr
                  ++ "\n\nMatrix Rank: "
                  ++ show rankMat
                  ++ "\nNumber of actors: "
                  ++ show (length actors)
                  ++ "\nNumber of edges: "
                  ++ show (length edges)
           in case nullspaceBasis mat of
                [] -> header ++ "\n\nNo repetition vector found: null space is empty."
                (basisVec : _) ->
                  let repInt = toMinimalIntegers basisVec
                      verificationResult = multiplyMatrixVector mat repInt
                      isZeroVector = all (== 0) verificationResult

                      nullSpaceStr =
                        "\n\nNull Space (fractional repetition vector for actors):\n"
                          ++ nullspaceVectorToString basisVec

                      verificationStr =
                        "\n\nVerification of null space vector:"
                          ++ "\nTopology Matrix × Repetition Vector ≈ Zero Vector? "
                          ++ show isZeroVector
                          ++ if not isZeroVector
                            then
                              "\nWarning: Product is not zero!\nProduct vector:\n"
                                ++ show verificationResult
                            else ""

                      repCounts = map fromIntegral repInt :: [Int]
                      repsWithNames = zip (map name actors) repCounts
                      repVecStr =
                        "\n\nNormalized repetition vector for ACTORS (integers):\n"
                          ++ unlines ["  " ++ show a ++ " = " ++ show r | (a, r) <- repsWithNames]

                      firings = concat (schedCores sched)
                      seqStr = intercalate ", " [show a ++ "^" ++ show idx | (a, idx) <- firings]
                      schedStr =
                        "\nSingle-Core Execution Schedule (one period):\n  Sequence: ["
                          ++ seqStr
                          ++ "]\n"

                      bufStr =
                        "\nBuffer sizes:\n"
                          ++ unlines ["  " ++ show b ++ ": " ++ show sz | (b, sz) <- schedBufferSizes sched]
                   in header ++ nullSpaceStr ++ verificationStr ++ repVecStr ++ schedStr ++ bufStr

prettyHuLevelSchedule :: Int -> IRSystem -> Schedule -> String
prettyHuLevelSchedule numCores irSystem sched =
  case convertIRSystem irSystem of
    Left e -> "Error: " ++ e ++ "\n"
    Right (actors, edges) ->
      if null edges
        then "No internal edges found. All actors fire once.\n"
        else
          let mat = buildTopologyMatrixEdgesRows actors edges
              matrixStr = matrixEdgesRowsToString edges actors mat
           in case nullspaceBasis mat of
                [] -> "No repetition vector found: null space is empty.\n"
                (basisVec : _) ->
                  let repInt = toMinimalIntegers basisVec
                      repCounts = map fromIntegral repInt :: [Int]
                      repsWithNames = zip (map name actors) repCounts
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
