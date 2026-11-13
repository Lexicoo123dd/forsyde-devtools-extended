{- |
Module      : Schedule.Greedy
Description : Single-core Periodic Admissible Sequential Schedule (PASS) generator
              using a greedy token-driven simulation.
-}
module Schedule.Greedy
  ( greedySchedule,
    generateGreedySchedule,
    prettyGreedySchedule,
  )
where

import Data.List (intercalate)
import ForSyDeIR (IRSystem)
import Schedule.Common

-- | Greedy forced schedule simulation algorithm.
--   Fires actors one-by-one by tracking FIFO token counts until all repetition requirements reach zero.
greedySchedule :: [Actor] -> [Edge] -> [Int] -> Either String [Int]
greedySchedule actors edges repetitionCounts =
  let nActors = length actors
      initialTokens = map initTokens edges

      worker :: [Int] -> [Int] -> [Int] -> Either String [Int]
      worker remainingReps currentTokens accSchedule
        | sum remainingReps == 0 = pure $ reverse accSchedule
        | otherwise =
            let isFireable i = do
                  let incomingEdges = incomingEdgeIndices actors edges i
                      actor = actors !! i
                      canFire = case incomingEdges of
                        [] ->
                          if isInput actor
                            then pure True
                            else Left $ "Invalid graph: actor " ++ show (name actor) ++ " has no incoming edges but is not marked as input."
                        _ ->
                          Right $
                            all
                              (\edgeIdx -> currentTokens !! edgeIdx >= cons (edges !! edgeIdx))
                              incomingEdges
                   in do
                        firable <- canFire
                        pure (i, firable && (remainingReps !! i) > 0)

                fireableActors = map fst <$> filter snd <$> sequence (map isFireable [0 .. nActors - 1])
             in case fireableActors of
                  Right (actorIdx : _) ->
                    let newTokens = fireOnce actors edges actorIdx currentTokens
                        newRemaining = updateAt actorIdx (subtract 1) remainingReps
                     in worker newRemaining newTokens (actorIdx : accSchedule)
                  Right [] -> Left "Error: Deadlock detected, cannot find fireable actor.\n"
                  Left e -> Left e
   in worker repetitionCounts initialTokens []

-- | Generates a single-core PASS schedule record for an IRSystem.
generateGreedySchedule :: IRSystem -> Either String Schedule
generateGreedySchedule irSystem = do
  analysis <- analyzeSDF irSystem
  case analysis of
    EmptyGraph actors _ ioBufSizes aliases ->
      pure $
        ScheduleRecord
          { schedStrategy = Greedy,
            schedCores = [tagInstances (map name actors)],
            schedBufferSizes = ioBufSizes,
            schedDelayBuffers = aliases,
            schedMetrics = Nothing
          }
    AnalyzedSDF actors edges _ _ _ repCounts _ delayBuffers ioBufSizes aliases -> do
      schedIdxs <- greedySchedule actors edges repCounts
      let schedNames = map (name . (actors !!)) schedIdxs
          internalBufSizes = simulateBufferUsage actors edges (map initTokens edges) schedIdxs
      pure $
        ScheduleRecord
          { schedStrategy = Greedy,
            schedCores = [tagInstances schedNames],
            schedBufferSizes = ioBufSizes ++ internalBufSizes,
            schedDelayBuffers = delayBuffers ++ aliases,
            schedMetrics = Nothing
          }

-- | Pretty-prints a single-core greedy schedule.
prettyGreedySchedule :: IRSystem -> Schedule -> String
prettyGreedySchedule irSystem sched =
  case analyzeSDF irSystem of
    Left e -> "Error: " ++ e ++ "\n"
    Right (EmptyGraph _ _ _ _) ->
      let schedNames = toFlatSchedule sched
       in "No internal edges found.\n\n"
            ++ "Schedule (all actors fire once):\n"
            ++ intercalate ", " (map show schedNames)
            ++ "\n\nBuffer sizes:\n"
            ++ unlines ["  " ++ show b ++ ": " ++ show sz | (b, sz) <- schedBufferSizes sched]
    Right (AnalyzedSDF actors edges mat basisVec repInt _ repsWithNames _ _ _) ->
      let matrixStr = matrixEdgesRowsToString edges actors mat
          rankMat = matrixRank mat
          header =
            matrixStr
              ++ "\n\nMatrix Rank: "
              ++ show rankMat
              ++ "\nNumber of actors: "
              ++ show (length actors)
              ++ "\nNumber of edges: "
              ++ show (length edges)

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
