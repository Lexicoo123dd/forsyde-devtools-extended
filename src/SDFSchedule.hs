{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module SDFSchedule
  ( Schedule (..),
    Actor (..),
    Edge (..),
    computeScheduleAndBuffers,
    computeScheduleAndBuffersPrint,
    computeScheduleAndBuffersMCPOC,
    computeScheduleAndBuffersMCPoC,
    computeScheduleAndBuffersMCPOCWithCores,
    computeScheduleAndBuffersMCPoCWithCores,
    computeScheduleAndBuffersMCPOCPrint,
    computeScheduleAndBuffersMCPoCPrint,
    computeScheduleAndBuffersMCPOCPrintWithCores,
    computeScheduleAndBuffersMCPoCPrintWithCores,
  )
where

import APGSchedule
  ( Actor (..),
    Edge (..),
    ScheduledFiring (..),
    computeHuLevels,
    createAcyclicPresedenceGraph,
    fireOnce,
    incomingEdgeIndices,
    listSchedulingHuLevel,
    outgoingEdgeIndices,
    toPerCoreSchedule,
    updateAt,
  )
import Data.List (elemIndex, find, intercalate, nub, sortBy)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Data.Ratio (denominator, numerator)
import ForSyDeIR

-- | Convert ForSyDe IR to SDF data structures
convertIRSystem :: IRSystem -> Either String ([Actor], [Edge])
convertIRSystem (IRSystem (inputNames, outputNames) constructors signals _) = do
  let -- 1. Delay node names (IRDelay has two parameters)
      delayNames = [n | IRDelay n _ (_, _) <- constructors]

      -- 2. All actor names (excluding delays)
      allActorNames =
        [ n
        | IRActor n _ _ (_, _) <- constructors
        ]

      -- 3. Actors that receive from inputs
      inputActorNames =
        [ dstId
        | IRSignal _ (srcId, _) (dstId, _) <- signals,
          srcId `elem` inputNames
        ]

      -- 4. Build actor list
      baseActors =
        [ Actor n (n `elem` inputActorNames || n `elem` inputNames)
        | n <- nub allActorNames
        ]

      -- 5. Find helper
      findActorByName n =
        case find (\a -> name a == n) baseActors of
          Just a -> pure a
          Nothing -> Left ("Actor not found: " ++ show n)

      -- Non-delay, non-input/output signals
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
              -- input is a global system input -> ignore
              ([(inSignalId, _, _)], _) | inSignalId `elem` inputNames -> pure []
              -- output is a global system output -> ignore
              (_, [(outSignalId, _, _)]) | outSignalId `elem` outputNames -> pure []
              ([(inSignalId, srcIn, prodIn)], [(outSignalId, dstOut, consOut)]) -> do
                srcActor <- findActorByName srcIn
                dstActor <- findActorByName dstOut
                delayTokens <-
                  foldl'
                    getDelayToken
                    (Left $ "Delay node " ++ show delayName ++ " not found")
                    constructors
                pure
                  [ Edge
                      -- use the input signal id as the delay edge id
                      inSignalId
                      srcActor
                      dstActor
                      prodIn
                      consOut
                      True
                      (length delayTokens)
                      -- allow the edge to be found by the output signal id as well.
                      -- Note that we need the in->in as we only lookup the aliaes.
                      (Just [(inSignalId, inSignalId), (outSignalId, inSignalId)])
                  ]
              ([], _) ->
                Left $ "Delay node " ++ show delayName ++ " has no input signal."
              (_, []) ->
                Left $ "Delay node " ++ show delayName ++ " has no output signal."
              _ ->
                Left $ "Delay node " ++ show delayName ++ " must have exactly one input and one output."

      -- 8. Normalize self-loops: ensure prod == cons, otherwise error
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
              else pure e -- prod == cons is valid
        | otherwise = pure e

  -- Folded delay edges (A -> delay -> B → becomes one edge)
  delayEdges <- mconcat <$> (sequence $ map makeDelayEdge delayNames)
  normalEdges <- sequence $ map makeNormalEdges normalSigs
  finalEdges <- sequence $ map normalizeSelfLoop (normalEdges ++ delayEdges)
  pure (baseActors, finalEdges)

----------------------------------------------------------
-- Topology matrix and null space calculation
----------------------------------------------------------

-- | Build the topology matrix, where rows are edges and columns are actors
--
-- Topo building logic:
-- - If actor is the source of the edge, the value is the production rate
-- - If actor is the destination of the edge, the value is the negative consumption rate
-- - Otherwise, the value is 0
buildTopologyMatrixEdgesRows :: [Actor] -> [Edge] -> [[Integer]]
buildTopologyMatrixEdgesRows actors edges =
  [[topo actor edge | actor <- actors] | edge <- edges]
  where
    topo actor edge
      | src edge == dst edge = 0
      | src edge == actor = fromIntegral (prod edge)
      | dst edge == actor = -fromIntegral (cons edge)
      | otherwise = 0

-- | Reduced row echelon form via exact Gaussian elimination over the rationals.
-- Returns the reduced rows and the pivot column indices, in pivot order.
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

-- | Basis vectors of the null space of an integer matrix, as exact rationals.
-- One basis vector per free column of the reduced matrix.
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

-- | Scale an exact rational vector to the smallest integer vector pointing in
-- the same direction. An all-negative vector is flipped to positive.
toMinimalIntegers :: [Rational] -> [Integer]
toMinimalIntegers xs =
  let commonDenom = foldl' lcm 1 (map denominator xs)
      ints = [numerator x * (commonDenom `div` denominator x) | x <- xs]
      gcdAll = foldl' gcd 0 ints
      reduced = if gcdAll == 0 then ints else map (`div` gcdAll) ints
   in if all (< 0) reduced then map negate reduced else reduced

multiplyMatrixVector :: [[Integer]] -> [Integer] -> [Integer]
multiplyMatrixVector mat vec = map (sum . zipWith (*) vec) mat

-- | Return the matrix as a string with edges as rows and actors as columns
matrixEdgesRowsToString :: [Edge] -> [Actor] -> [[Integer]] -> String
matrixEdgesRowsToString edges actors topoMatrix =
  let -- Calculate the maximum width of each column
      actorNameWidths = map (length . show . name) actors
      rowValueWidths = map (maximum . map (length . show)) topoMatrix
      colWidths = zipWith max actorNameWidths rowValueWidths
      totalColWidths = map (+ 2) colWidths -- Add 2 to each column width for spacing

      -- Calculate the maximum width of the edge labels
      edgeLabelWidth = maximum (map (length . show . edgeLabel) edges)

      -- Build the header
      header =
        pad (edgeLabelWidth + 2) "Edge\\Actor"
          ++ concatMap (\(actor, width) -> pad width (show $ name actor)) (zip actors totalColWidths)

      -- Build the separator line
      totalWidth = edgeLabelWidth + 2 + sum totalColWidths
      separator = replicate totalWidth '-'

      -- Build the matrix rows
      matrixRows = map (rowToString edgeLabelWidth totalColWidths) (zip edges topoMatrix)

      -- Combine everything
      result =
        "Topology Matrix (Edges × Actors):\n"
          ++ header
          ++ "\n"
          ++ separator
          ++ "\n"
          ++ unlines matrixRows
   in result
  where
    pad width str = take width (str ++ repeat ' ')
    edgeLabel edge = edgeName edge

    rowToString labelWidth colWidths (edge, rowValues) =
      pad (labelWidth + 2) (show $ edgeLabel edge)
        ++ concatMap
          (\(val, width) -> pad width (show val))
          (zip rowValues colWidths)

----------------------------------------------------------
-- Scheduler: Greedy + Forced approach with special handling for inputs.
-- Actors marked as isInput=True have a infinite external tokens
-- To fire an actor, it must have enough input tokens for all its incoming edges.
----------------------------------------------------------



-- | Greedy forced schedule algorithm
--
-- Parameters:
--   actors: list of actors
--   edges: list of edges
--   repetitionCounts: number of times each actor needs to be executed
-- Return: schedule sequence
--
-- Steps:
-- 1. Find all the fireable actors (repetition count > 0 and has enough input tokens)
-- 2. Fire all the fireable actors with the sequence order (could be optimized for minimal buffer size)
-- 3. If there is no fireable actor, force fire an remaining actor with delay edges check (to determine the initial token count)
-- 4. Record the minimum token value of each edge during the execution
-- 5. If there are negative minimum token values, it will be the required initial token count
greedySchedule :: [Actor] -> [Edge] -> [Int] -> Either String [Int]
greedySchedule actors edges repetitionCounts =
  let nActors = length actors
      initialTokens = map initTokens edges -- map initial tokens

      -- Recursive worker function:
      -- - remainingReps: remaining times
      -- - currentTokens: current token state
      -- - accSchedule: accumulated schedule sequence
      worker :: [Int] -> [Int] -> [Int] -> Either String [Int]
      worker remainingReps currentTokens accSchedule
        | sum remainingReps == 0 = pure $ reverse accSchedule
        | otherwise =
            let isFireable i = do
                  let incomingEdges = incomingEdgeIndices actors edges i -- has remaining fire repetitions
                      actor = actors !! i
                      canFire = case incomingEdges of
                        [] ->
                          if isInput actor
                            then pure $ True -- If is input actor and no incoming edges from other actors
                            else Left $ "Invalid graph: actor " ++ show (name actor) ++ " has no incoming edges but is not marked as input."
                        _ ->
                          Right $
                            all
                              ( \edgeIdx ->
                                  -- All incoming edges must have enough tokens
                                  currentTokens !! edgeIdx >= cons (edges !! edgeIdx)
                              )
                              incomingEdges
                   in do
                        firable <- canFire
                        pure $ (i, firable && (remainingReps !! i) > 0)
                -- fireableActors = [i | i <- [0 .. nActors - 1], isFireable i]
                fireableActors = map fst <$> filter (\(_, v) -> v) <$> sequence (map isFireable [0 .. nActors - 1])
             in case fireableActors of
                  Right (actorIdx : _) ->
                    -- If there is a fireable actor, fire the first one
                    let newTokens = fireOnce actors edges actorIdx currentTokens
                        newRemaining = updateAt actorIdx (subtract 1) remainingReps
                     in worker newRemaining newTokens (actorIdx : accSchedule)
                  Right [] -> Left "Error: Deadlock detected, cannot find fireable actor.\n"
                  Left e -> Left e
   in worker repetitionCounts initialTokens []

----------------------------------------------------------
-- Simulation to compute buffer size per edge
-- After generating a valid schedule, simulate it once over one period
-- The buffer size of each edge is defined as the maximum token count observed
----------------------------------------------------------

simulateBufferUsage :: [Actor] -> [Edge] -> [Int] -> [Int] -> [(IRId, Int)]
simulateBufferUsage actors edges initialTokens schedule =
  let -- Get all edge names
      edgeNames = map edgeName edges

      -- Simulate one step：fire one actor, update token state and max buffer record
      simulateStep (currentTokens, maxBuffer) actorIndex =
        let incomingEdges = incomingEdgeIndices actors edges actorIndex
            outgoingEdges = outgoingEdgeIndices actors edges actorIndex
            tokensAfterConsume = foldl consume currentTokens incomingEdges
            tokensAfterProduce = foldl produce tokensAfterConsume outgoingEdges
            newMaxBuffer = zipWith max maxBuffer tokensAfterProduce
         in (tokensAfterProduce, newMaxBuffer)

      -- Use foldl to simulate the whole schedule
      (_, finalMaxBuffer) = foldl simulateStep (initialTokens, initialTokens) schedule

      -- Combine buffer sizes and names
      bufferSizesWithNames = zip edgeNames finalMaxBuffer
   in bufferSizesWithNames
  where
    consume tokens edgeIdx =
      let consumption = cons (edges !! edgeIdx)
       in updateAt edgeIdx (subtract consumption) tokens
    produce tokens edgeIdx =
      let production = prod (edges !! edgeIdx)
       in updateAt edgeIdx (+ production) tokens

computeIOBufferSizes :: IRSystem -> [(IRId, Int)] -> Either String ([(IRId, Int)], [(IRId, IRId)])
computeIOBufferSizes (IRSystem (inputs, outputs) constructors signals _) repsWithNames =
  let -- Find all external input edges
      inputSignals = [s | s@(IRSignal _ (srcId, _) (_, _)) <- signals, srcId `elem` inputs]
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

      -- Find the first non-delay process connected to the input signal
      nonDelayInputSignal s@(IRSignal sigId (srcId, srcRate) (dstId, _)) =
        case find (\c -> constructorName c == dstId) constructors of
          -- The signal directly connects to an actor
          Just (IRActor _ _ _ _) -> pure (s, [], 0)
          -- The signal connects to a delay, keep going
          Just (IRDelay dName tokens (_, next)) | dstId == dName ->
            case find (\(IRSignal sId _ _) -> sId == next) signals of
              Just (IRSignal sigAlias (_, _) (newDstId, newDstRate)) -> do
                (sig, aliases, nTokens) <- nonDelayInputSignal (IRSignal sigId (srcId, srcRate) (newDstId, newDstRate))
                pure (sig, (sigId, sigId) : (sigAlias, sigId) : aliases, nTokens + length tokens)
              Nothing -> Left $ "Could not find next signal " ++ show next
          _ -> Left $ "Could not find the process " ++ show srcId

      -- Find all external output edges
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

      -- Find the first non-delay process connected to the output signal
      nonDelayOutputSignal s@(IRSignal sigId (srcId, _) (dstId, dstRate)) =
        case find (\c -> constructorName c == srcId) constructors of
          -- The signal directly connects to an actor
          Just (IRActor _ _ _ _) -> pure (s, [], 0)
          -- The signal connects to a delay, keep going
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

      -- Find repetition count by actor names
      findActorRep actorName =
        case lookup actorName repsWithNames of
          Just rep -> pure rep
          Nothing -> Left $ "Actor " ++ show actorName ++ " not found in repetition counts"

      -- separate the buffers and aliases
      foldBuffer (sigId, rate, rep, aliases, nTokens) (sigAcc, aliasAcc) =
        ((sigId, max (rate * rep) nTokens) : sigAcc, aliases ++ aliasAcc)
   in do
        inEdges <- inputEdges
        outEdges <- outputEdges
        pure $ foldr foldBuffer ([], []) $ inEdges ++ outEdges

----------------------------------------------------------
-- Verification of the schedule
----------------------------------------------------------

verifySchedule :: [Actor] -> [Edge] -> [Int] -> [Int] -> [Int] -> Bool
verifySchedule actors edges initialTokens schedule _repetitionCounts =
  let -- Recursively check the schedule
      check :: [Int] -> [Int] -> Bool
      check _ [] = True -- Base case: empty schedule is valid
      check currentTokens (actorIndex : remainingSchedule) =
        let incomingEdges = incomingEdgeIndices actors edges actorIndex

            -- Simulate consumption and check if it is valid
            tokensAfterConsume = foldl consume currentTokens incomingEdges
            consumptionValid = all (>= 0) tokensAfterConsume

            -- Simulate production
            outgoingEdges = outgoingEdgeIndices actors edges actorIndex
            tokensAfterProduce = foldl produce tokensAfterConsume outgoingEdges
         in consumptionValid && check tokensAfterProduce remainingSchedule
   in check initialTokens schedule
  where
    consume tokens edgeIdx =
      let consumption = cons (edges !! edgeIdx)
       in updateAt edgeIdx (subtract consumption) tokens
    produce tokens edgeIdx =
      let production = prod (edges !! edgeIdx)
       in updateAt edgeIdx (+ production) tokens

----------------------------------------------------------
-- Library Function: compute schedule & buffer sizes
----------------------------------------------------------

-- | Represents a complete schedule for an IR system, including the order
--   in which actors execute, the sizes of all required buffers, and the
--   mappings between original signals and their corresponding delay buffers.
--
--   The first list contains the actor identifiers in their execution order.
--   An actor may appear multiple times if it must execute more than once
--   to satisfy the repetition vector of the graph.
--
--   The second list contains pairs of buffer identifiers and their required
--   capacities, expressed as integer sizes.
--
--   The third list contains pairs mapping an original signal identifier
--   to the identifier of the delay buffer associated with that signal.
newtype Schedule = Schedule ([IRId], [(IRId, Int)], [(IRId, IRId)])
  deriving (Show)

-- | Computes a valid actor execution schedule and determines the buffer
--   sizes and delay buffer mappings required by an IR system.
--
--   The function first converts the input IR system into a collection
--   of actors and edges. It then handles two cases:
--
--   1. The graph has no internal edges. In this case, every actor executes
--      exactly once, and no internal edge buffers are required.
--
--   2. The graph has internal edges. In this case, the function constructs
--      a topology matrix and checks its rank. If the rank satisfies the
--      required condition, it calculates a repetition vector, verifies
--      that vector, generates a valid execution schedule, simulates buffer
--      usage, and computes the input/output buffer sizes and aliases.
--
--   The function returns either an error message explaining why a schedule
--   could not be computed or a Schedule containing the actor execution
--   order, buffer sizes, and delay buffer mappings.
--
--   Arguments:
--     irSystem - The intermediate representation of the system to schedule.
--
--   Returns:
--     Right Schedule - The computed schedule, buffer sizes, and mappings.
--     Left String    - An error message if the schedule cannot be computed.
computeScheduleAndBuffers :: IRSystem -> Either String Schedule
computeScheduleAndBuffers irSystem = do
  -- Convert the IR system into its constituent actors and edges.
  -- The conversion may fail, in which case the error is propagated
  -- through the Either monad.
  (actors, edges) <- convertIRSystem irSystem

  -- Check whether the graph contains any internal edges.
  -- An edge represents an internal signal connection between actors.
  if null edges
    then
      -- If there are no internal edges, fire all actor once, no buffer required
      -- Obtain the name of each actor in the order in which it appears.
      -- Since there are no internal dependencies to schedule, each actor
      -- needs to execute exactly once.
      let schedNames = map name actors

          -- Associate every actor name with a repetition count of one.
          -- These counts are used when calculating the input/output buffers.
          repsWithNames = zip (map name actors) (replicate (length actors) 1)

          -- Create buffer entries for all internal edges.
          -- Because the edge list is empty in this branch, this produces
          -- an empty list of internal buffer sizes.
          internalBufSizes = zip (map edgeName edges) (replicate (length edges) 0)
       in do
            -- Calculate the buffer sizes required for the system's inputs
            -- and outputs, using the repetition counts computed above.
            -- Also obtain the aliases mapping signals to their associated
            -- buffers, as determined by the I/O buffer calculation.
            (ioBufSizes, aliases) <- computeIOBufferSizes irSystem repsWithNames

            -- Construct the final schedule.
            -- The schedule contains each actor exactly once, the computed
            -- I/O buffer sizes followed by the internal buffer sizes, and
            -- the signal-to-buffer aliases.
            pure $ Schedule (schedNames, ioBufSizes ++ internalBufSizes, aliases)

    else
      -- When internal edges exist, construct the topology matrix that
      -- describes the relationships between actors and edges.
      -- This matrix is subsequently used to calculate the repetition vector.
      let mat = buildTopologyMatrixEdgesRows actors edges

          -- Compute the rank of the topology matrix.
          -- The rank is used to determine whether the graph satisfies
          -- the structural condition required by this scheduling algorithm.
          rankMat = matrixRank mat
       in
        -- Require the matrix rank to equal the number of actors minus one.
        -- If this condition is not satisfied, the repetition vector cannot
        -- be computed using the method implemented below.
        if rankMat == length actors - 1
            then
              -- Find a basis for the null space of the topology matrix.
              -- A vector in this null space satisfies the balance equations
              -- represented by the matrix and can therefore serve as the
              -- basis for deriving actor repetition counts.
              case nullspaceBasis mat of

              -- If the null space has no basis vectors, there is no vector
              -- available from which to construct a repetition vector.
              [] -> Left "No repetition vector found: null space is empty."

              -- Use the first basis vector to derive the repetition vector.
              -- The resulting vector is converted to minimal integer values
              -- so that the actor counts can be used for scheduling.
              (basisVec : _) ->
                let
                    -- Convert the selected null-space basis vector into
                    -- a minimal integer vector.
                    -- This gives integer repetition counts while preserving
                    -- the proportional relationships between the entries.
                    repInt = toMinimalIntegers basisVec

                    -- Verification
                    -- Multiply the topology matrix by the proposed repetition
                    -- vector and check whether every resulting entry is zero.
                    -- A zero result means the vector satisfies the balance
                    -- equations represented by the topology matrix.
                    isZeroVector = all (== 0) (multiplyMatrixVector mat repInt)

                    -- Determine the final result by checking the validity
                    -- of the repetition vector before constructing a schedule.
                    finalResult
                      -- Reject the vector if it does not belong to the
                      -- null space of the topology matrix.
                      | not isZeroVector = Left "Verification failed: repetition vector is not in null space"

                      -- Reject the vector if any actor has a repetition count
                      -- less than or equal to zero. A valid repetition vector
                      -- for this graph must contain only strictly positive
                      -- repetition counts.
                      | any (<= 0) repInt = Left "Cannot compute a strictly positive repetition vector for this graph."

                      -- Continue with schedule construction only after the
                      -- repetition vector passes both verification checks.
                      | otherwise =
                          let
                              -- Convert the integer repetition counts into
                              -- Int values for use by the scheduling and
                              -- buffer simulation functions.
                              repCounts = map fromIntegral repInt :: [Int]

                              -- Pair every actor name with its repetition
                              -- count. This representation is used to compute
                              -- the required I/O buffer sizes.
                              repsWithNames = zip (map name actors) repCounts

                              -- Obtain the delay buffer mappings for the
                              -- internal edges. These mappings associate
                              -- original signals with their delay buffers.
                              delayBuffers = getBuffers edges

                           in do

                                -- Generate a valid actor execution schedule
                                -- using the actors, edges, and repetition
                                -- counts. The returned indices identify the
                                -- actors in the original actors list.
                                schedIdxs <- greedySchedule actors edges repCounts

                                -- Simulate buffer usage across the generated
                                -- schedule to determine the required sizes
                                -- of the internal edge buffers.
                                -- The initial token count of each edge is
                                -- supplied as the initial buffer occupancy.
                                internalBufSizes <- pure $ simulateBufferUsage actors edges (map initTokens edges) schedIdxs

                                -- Convert the actor indices in the schedule
                                -- into actor names, preserving execution order
                                -- and repeated occurrences of the same actor.
                                schedNames <- pure $ map (name . (actors !!)) schedIdxs

                                -- Calculate the required input/output buffer
                                -- sizes and obtain the corresponding aliases,
                                -- using the repetition counts of the actors.
                                (ioBufSizes, aliases) <- computeIOBufferSizes irSystem repsWithNames

                                -- Assemble the final schedule:
                                --   - schedNames specifies the actor execution order.
                                --   - ioBufSizes ++ internalBufSizes combines
                                --     I/O buffer sizes with internal buffer sizes.
                                --   - delayBuffers ++ aliases combines the
                                --     internal delay mappings with the I/O aliases.
                                pure $ Schedule (schedNames, ioBufSizes ++ internalBufSizes, delayBuffers ++ aliases)
                 in finalResult

            -- If the topology matrix rank does not equal the number of actors
            -- minus one, reject the graph because the required repetition
            -- vector cannot be computed under this algorithm's rank condition.
            else
              Left "Matrix rank is not equal to number of actors minus one. Cannot compute repetition vector."

getBuffers :: [Edge] -> [(IRId, IRId)]
getBuffers edges =
  concatMap
    ( \e ->
        case buffers e of
          Just bufList -> bufList
          Nothing -> []
    )
    edges

----------------------------------------------------------
-- Pretty-print version
----------------------------------------------------------

-- | Returns a string with topology matrix, repetition vector, schedule, verification results, and buffer usage.
-- Note: partial function
computeScheduleAndBuffersPrint :: IRSystem -> String
computeScheduleAndBuffersPrint irSystem =
  case convertIRSystem irSystem of
    Left e -> error e
    Right (actors, edges) ->
      let outputString =
            if null edges
              then
                -- If there are no internal edges, fire all actor once, no buffer required
                let schedNames = map name actors
                    repsWithNames = zip (map name actors) (replicate (length actors) 1)
                    internalBufSizes = zip (map edgeName edges) (replicate (length edges) 0)
                    ioBufSizes = case computeIOBufferSizes irSystem repsWithNames of
                      Left e -> error e
                      Right v -> v
                    allBufSizes = fst ioBufSizes ++ internalBufSizes
                 in "No internal edges found.\n\n"
                      ++ "Schedule (all actors fire once):\n"
                      ++ intercalate ", " (map show schedNames)
                      ++ "\n\nI/O buffer sizes:"
                      ++ concatMap
                        (\(edgeNameVal, sz) -> "\n  " ++ show edgeNameVal ++ ": " ++ show sz)
                        allBufSizes
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
                 in if rankMat == length actors - 1
                      then
                        let basisVec = case nullspaceBasis mat of
                              (v : _) -> v
                              [] -> error "No repetition vector found: null space is empty."
                            repInt = toMinimalIntegers basisVec

                            -- Verification: multiply the integer repetition vector back to the topology matrix
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
                              "\n\nNormalized repetition vector for ACTORS (integers):"
                                ++ intercalate "\n" [show label ++ "=" ++ show r | (label, r) <- zip (map name actors) repInt]

                            repCounts = map fromIntegral repInt :: [Int]
                            repsWithNames = zip (map name actors) repCounts
                            schedIdxs = case greedySchedule actors edges repCounts of
                              Left e -> error e
                              Right v -> v
                            schedNames = map (name . (actors !!)) schedIdxs

                            schedStr =
                              "\n\nGenerated schedule (actor firing order):\n"
                                ++ intercalate ", " (map show schedNames)

                            initialTokensStr =
                              "\n\nInitial tokens (provided by IR):"
                                ++ concatMap
                                  (\(ename, e) -> "\n " ++ ename ++ ": " ++ show (initTokens e))
                                  (zip (map (show . edgeName) edges) edges)

                            ok = verifySchedule actors edges (map initTokens edges) schedIdxs repCounts
                            verificationSchedStr =
                              "\n\nVerification of schedule with computed initial tokens: "
                                ++ (if ok then "OK" else "FAILED")

                            -- Simulate buffer usage for one period
                            internalBufSizes = simulateBufferUsage actors edges (map initTokens edges) schedIdxs
                            ioBufSizes = case computeIOBufferSizes irSystem repsWithNames of
                              Left e -> error e
                              Right v -> v
                            allBufSizes = fst ioBufSizes ++ internalBufSizes

                            internalBufStr =
                              "\n\nInternal buffer sizes (maximum tokens observed per edge):"
                                ++ concatMap
                                  (\(ename, sz) -> "\n  " ++ show ename ++ ": " ++ show sz)
                                  internalBufSizes

                            ioBufStr =
                              "\n\nI/O buffer sizes (rate × repetition count):"
                                ++ concatMap
                                  (\(ename, sz) -> "\n  " ++ show ename ++ ": " ++ show sz)
                                  (fst ioBufSizes)

                            allBufStr =
                              "\n\nAll buffer sizes (I/O + internal):"
                                ++ concatMap
                                  (\(ename, sz) -> "\n  " ++ show ename ++ ": " ++ show sz)
                                  allBufSizes
                         in header
                              ++ nullSpaceStr
                              ++ verificationStr
                              ++ repVecStr
                              ++ schedStr
                              ++ initialTokensStr
                              ++ verificationSchedStr
                              ++ internalBufStr
                              ++ ioBufStr
                              ++ allBufStr
                      else
                        header ++ "\n\nMatrix rank is not equal to number of actors minus one. Cannot compute repetition vector."
       in outputString ++ "\n"

-- Helper function to display the null space vector, one component per line
nullspaceVectorToString :: [Rational] -> String
nullspaceVectorToString = unlines . map formatRational
  where
    formatRational x
      | denominator x == 1 = show (numerator x)
      | otherwise = show (numerator x) ++ "/" ++ show (denominator x)



computeScheduleAndBuffersMCPoC :: IRSystem -> Either String Schedule
computeScheduleAndBuffersMCPoC = computeScheduleAndBuffersMCPoCWithCores 2

computeScheduleAndBuffersMCPOC :: IRSystem -> Either String Schedule
computeScheduleAndBuffersMCPOC = computeScheduleAndBuffersMCPoC

computeScheduleAndBuffersMCPOCWithCores :: Int -> IRSystem -> Either String Schedule
computeScheduleAndBuffersMCPOCWithCores = computeScheduleAndBuffersMCPoCWithCores

computeScheduleAndBuffersMCPoCWithCores :: Int -> IRSystem -> Either String Schedule
computeScheduleAndBuffersMCPoCWithCores numCores irSystem = do
  (actors, edges) <- convertIRSystem irSystem

  if null edges
    then
      let schedNames = map name actors

          repsWithNames = zip (map name actors) (replicate (length actors) 1)

          internalBufSizes = zip (map edgeName edges) (replicate (length edges) 0)
       in do
            (ioBufSizes, aliases) <- computeIOBufferSizes irSystem repsWithNames

            pure $ Schedule (schedNames, ioBufSizes ++ internalBufSizes, aliases)

    else
      let mat = buildTopologyMatrixEdgesRows actors edges

          rankMat = matrixRank mat
       in
        if rankMat == length actors - 1
            then
              case nullspaceBasis mat of

              [] -> Left "No repetition vector found: null space is empty."

              (basisVec : _) ->
                let
                    repInt = toMinimalIntegers basisVec

                    isZeroVector = all (== 0) (multiplyMatrixVector mat repInt)

                    finalResult
                      | not isZeroVector = Left "Verification failed: repetition vector is not in null space"

                      | any (<= 0) repInt = Left "Cannot compute a strictly positive repetition vector for this graph."

                      | otherwise =
                          let
                              repCounts = map fromIntegral repInt :: [Int]

                              repsWithNames = zip (map name actors) repCounts

                              delayBuffers = getBuffers edges

                           in do
                                (instances, presEdges) <- createAcyclicPresedenceGraph actors edges mat repCounts
                                scheduledFirings <- listSchedulingHuLevel numCores instances presEdges (\_ -> 1)

                                let orderedFirings = sortBy (comparing sfStartTime) scheduledFirings
                                    schedNames = map (fst . sfInstance) orderedFirings
                                    actorNameList = map name actors
                                    lookupIdx aName = case elemIndex aName actorNameList of
                                      Just idx -> idx
                                      Nothing -> error $ "Actor not found in actor list: " ++ show aName
                                    schedIdxs = map lookupIdx schedNames

                                internalBufSizes <- pure $ simulateBufferUsage actors edges (map initTokens edges) schedIdxs

                                (ioBufSizes, aliases) <- computeIOBufferSizes irSystem repsWithNames
                                pure $ Schedule (schedNames, ioBufSizes ++ internalBufSizes, delayBuffers ++ aliases)
                 in finalResult
            else
              Left "Matrix rank is not equal to number of actors minus one. Cannot compute repetition vector."

-- | Formatted string output of the multi-core schedule per core.
computeScheduleAndBuffersMCPOCPrint :: IRSystem -> String
computeScheduleAndBuffersMCPOCPrint = computeScheduleAndBuffersMCPOCPrintWithCores 2

computeScheduleAndBuffersMCPoCPrint :: IRSystem -> String
computeScheduleAndBuffersMCPoCPrint = computeScheduleAndBuffersMCPOCPrint

computeScheduleAndBuffersMCPoCPrintWithCores :: Int -> IRSystem -> String
computeScheduleAndBuffersMCPoCPrintWithCores = computeScheduleAndBuffersMCPOCPrintWithCores

computeScheduleAndBuffersMCPOCPrintWithCores :: Int -> IRSystem -> String
computeScheduleAndBuffersMCPOCPrintWithCores numCores irSystem =
  case convertIRSystem irSystem of
    Left e -> error e
    Right (actors, edges) ->
      if null edges
        then "No internal edges found. All actors fire once.\n"
        else
          let mat = buildTopologyMatrixEdgesRows actors edges
              matrixStr = matrixEdgesRowsToString edges actors mat
              rankMat = matrixRank mat
           in if rankMat /= length actors - 1
                then "Matrix rank is not equal to number of actors minus one. Cannot compute repetition vector.\n"
                else case nullspaceBasis mat of
                  [] -> error "No repetition vector found: null space is empty."
                  (basisVec : _) ->
                    let repInt = toMinimalIntegers basisVec
                        repCounts = map fromIntegral repInt :: [Int]
                        repsWithNames = zip (map name actors) repCounts
                     in case createAcyclicPresedenceGraph actors edges mat repCounts of
                          Left err -> "Error constructing APG: " ++ err ++ "\n"
                          Right (instances, presEdges) ->
                            let huLevels = computeHuLevels instances presEdges (\_ -> 1)
                             in case listSchedulingHuLevel numCores instances presEdges (\_ -> 1) of
                                  Left err -> "Error in list scheduling: " ++ err ++ "\n"
                                  Right scheduledFirings ->
                                    let perCore = toPerCoreSchedule numCores scheduledFirings
                                        makespan = if null scheduledFirings then 0 else maximum (map sfEndTime scheduledFirings)
                                        seqSteps = length instances
                                        speedup = if makespan > 0 then fromIntegral seqSteps / fromIntegral makespan :: Double else 1.0

                                        -- Theoretical lower bound on makespan: max(L, ceil(W / P))
                                        criticalPath = if null huLevels then 0 else maximum (Map.elems huLevels)
                                        ceilWoverP = (seqSteps + numCores - 1) `div` numCores
                                        minPossibleMakespan = max criticalPath ceilWoverP
                                        theoreticalMaxSpeedup =
                                          if minPossibleMakespan > 0
                                            then fromIntegral seqSteps / fromIntegral minPossibleMakespan :: Double
                                            else 1.0

                                        formatCore (c, firings) =
                                          let coreFirings = filter (\sf -> sfCore sf == c) (sortBy (comparing sfStartTime) scheduledFirings)
                                              linesList = map (\sf -> "  [Tick " ++ show (sfStartTime sf) ++ " -> " ++ show (sfEndTime sf) ++ "]: " ++ show (fst (sfInstance sf)) ++ " (instance #" ++ show (snd (sfInstance sf)) ++ ")") coreFirings
                                              summary = intercalate ", " (map (\inst -> show (fst inst) ++ "^" ++ show (snd inst)) firings)
                                           in "Core " ++ show c ++ ":\n"
                                                ++ unlines linesList
                                                ++ "  Sequence: [" ++ summary ++ "]\n"

                                        perCoreStr = unlines (map formatCore (Map.toList perCore))
                                        repVecStr = intercalate "\n" [ "  " ++ show label ++ " = " ++ show r | (label, r) <- repsWithNames ]
                                        huLevelStr = unlines [ "  " ++ show (fst inst) ++ "^" ++ show (snd inst) ++ " : Hu-Level " ++ show lvl | (inst, lvl) <- Map.toList huLevels ]
                                     in "============================================================\n"
                                          ++ "           Multi-Core Periodic SDF Schedule (Hu-Level)      \n"
                                          ++ "============================================================\n\n"
                                          ++ "Target Cores: " ++ show numCores ++ "\n\n"
                                          ++ matrixStr ++ "\n\n"
                                          ++ "Repetition Vector (Firings per period):\n"
                                          ++ repVecStr ++ "\n\n"
                                          ++ "Acyclic Precedence Graph (APG):\n"
                                          ++ "  Total firings (vertices)      : " ++ show (length instances) ++ "\n"
                                          ++ "  Precedence constraints (edges): " ++ show (length presEdges) ++ "\n\n"
                                          ++ "Hu-Levels (Critical path distance to sink):\n"
                                          ++ huLevelStr ++ "\n"
                                          ++ "------------------------------------------------------------\n"
                                          ++ "Schedule per Core:\n"
                                          ++ "------------------------------------------------------------\n"
                                          ++ perCoreStr ++ "\n"
                                          ++ "------------------------------------------------------------\n"
                                          ++ "Schedule Metrics:\n"
                                          ++ "------------------------------------------------------------\n"
                                          ++ "  Total Work (W)         : " ++ show seqSteps ++ " firings\n"
                                          ++ "  Critical Path (L)      : " ++ show criticalPath ++ " ticks\n"
                                          ++ "  Min Theoretical Makespan: " ++ show minPossibleMakespan ++ " ticks (max(L, ceil(W/P)) = max(" ++ show criticalPath ++ ", " ++ show ceilWoverP ++ "))\n"
                                          ++ "  Actual Makespan        : " ++ show makespan ++ " cycles / ticks\n"
                                          ++ "  Actual Speedup         : " ++ show (fromIntegral (round (speedup * 100) :: Int) / 100.0) ++ "x\n"
                                          ++ "  Theoretical Max Speedup: " ++ show (fromIntegral (round (theoreticalMaxSpeedup * 100) :: Int) / 100.0) ++ "x (for " ++ show numCores ++ " cores)\n"



