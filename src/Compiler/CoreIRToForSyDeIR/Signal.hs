-- | Handles the creation and resolution of signals between process
-- constructors and system inputs and outputs.
--
-- This module creates signals based on process constructor arguments
-- and updates the corresponding input and output connections of
-- process constructors. It also resolves temporary signal sources
-- once all system bindings have been identified.
--
-- Should handle:
--   * Creating signals between process constructors.
--   * Identifying the source of a signal.
--   * Updating process constructor input connections.
--   * Updating process constructor output connections.
--   * Resolving temporary signal sources using binder information.
module Compiler.CoreIRToForSyDeIR.Signal (createSignalsFromArguments, updateConstructorsAndSignals, getSourceFromArgument) where

import Compiler.CoreIRToForSyDeIR.Context
import Compiler.ForSyDeIR
import Debug.Trace

-- | Creates signals based on the arguments of a process constructor. All
-- signals created with this function have the process constructor as the
-- target.
createSignalsFromArguments :: TranslationContext -> IRId -> [IRId] -> Either String TranslationContext
createSignalsFromArguments context pcId arguments =
  -- The input rates of the process constructor are used to determine the
  -- targetRate of created signals, the index for the input is the same as the
  -- current arguments index.
  case (lookup pcId (pcRates context)) of
    Just (inputRates, _) -> foldr aux (pure context) $ zip arguments inputRates
    Nothing -> Left ("createSignalsFromArguments - No rates found for process constructor: " ++ show pcId)
  where
    aux :: (IRId, Int) -> Either String TranslationContext -> Either String TranslationContext
    aux (argument, rate) context0 = do
      currentContext <- context0
      case getSourceFromArgument currentContext argument of
        Left e -> Left e
        Right (sourceId, sourceRate) ->
          let newSignal = IRSignal argument (sourceId, sourceRate) (pcId, rate)
              newSignals = (argument, newSignal) : (signals currentContext)
              context1 = currentContext {signals = newSignals}
           in pure $ updateConstructorsInputs context1 pcId argument

-- | Updates the inputs of constructors within a `TranslationContext`. Adds a
-- signal to the head of a process constructors input signals list.
updateConstructorsInputs :: TranslationContext -> IRId -> IRId -> TranslationContext
updateConstructorsInputs initialContext pcId signalId =
  let newConstructors = map (updateConstructor) (constructors initialContext)
      context1 = initialContext {constructors = newConstructors}
   in context1
  where
    updateConstructor :: (IRId, IRConstructor) -> (IRId, IRConstructor)
    updateConstructor (currentPcId, currentConstructor) =
      if pcId == currentPcId
        then case currentConstructor of
          IRDelay _ tokens (_, outputSignal) ->
            let newConstructor = IRDelay currentPcId tokens (signalId, outputSignal)
             in (currentPcId, newConstructor)
          IRActor _ actorType functionId (inputSignals, outputSignals) ->
            let newConstructor = IRActor currentPcId actorType functionId (signalId : inputSignals, outputSignals)
             in (currentPcId, newConstructor)
        else (currentPcId, currentConstructor)

--- HELPERS ----

-- | Updates the outputs constructors within `TranslationContext`. It adds a
-- signal to the output list of a specific process constructor at the specified
-- index
updateConstructorsOutputs :: TranslationContext -> IRId -> IRId -> Int -> Either String TranslationContext
updateConstructorsOutputs initialContext signalId pcId index =
  case sequence $ map (updateConstructor) (constructors initialContext) of
    Left e -> Left e
    Right newConstructors -> pure initialContext {constructors = newConstructors}
  where
    updateConstructor :: (IRId, IRConstructor) -> Either String (IRId, IRConstructor)
    updateConstructor (currentPcId, currentConstructor) =
      if pcId == currentPcId
        then case currentConstructor of
          IRDelay _ tokens (inputSignal, _) ->
            if index /= 0
              then Left ("updateConstructorsOutputs - Process constructor id matches a delay but has a non-zero index")
              else
                let newConstructor = IRDelay currentPcId tokens (inputSignal, signalId)
                 in pure (currentPcId, newConstructor)
          IRActor _ actorType functionId (inputSignals, outputSignals) ->
            let newOutputSignals = take index outputSignals ++ [signalId] ++ drop (index + 1) outputSignals
                newConstructor = IRActor currentPcId actorType functionId (inputSignals, newOutputSignals)
             in pure (currentPcId, newConstructor)
        else pure (currentPcId, currentConstructor)

-- | Updates all the signals within `TranslationContext` which are temporarily
-- using a binder as the signal source. Replaces them with their associated
-- process constructor based on the binders accumulated within the
-- `TranslationContext`. Also updates the outputs of the associated process
-- constructors. Since if its a source of to signal the signal is an output.
updateConstructorsAndSignals :: TranslationContext -> Either String TranslationContext
updateConstructorsAndSignals initialContext = do
  let initialSignals = map snd (signals initialContext)
  (context1, sigs) <- foldl' aux (pure (initialContext, [])) initialSignals
  let finalContext = context1 {signals = sigs}

  let signalList = signals finalContext
  let actorFunctions =
        [ (actor, function)
        | (_, actor@(IRActor _ _ functionId _)) <- constructors finalContext,
          Just function <- [lookup functionId (functions finalContext)]
        ]
  let connections =
        [ (inputSignals, outputSignals, inputTypes, outputTypes)
        | ( IRActor _ _ _ (inputSignals, outputSignals),
            IRFunction _ _ (inputTypes, outputTypes)
            ) <-
            actorFunctions
        ]
  let typedInputs =
        [ zip inputSignals inputTypes
        | (inputSignals, _, inputTypes, _) <- connections
        ]

  let typedOutputs =
        [ zip outputSignals outputTypes
        | (_, outputSignals, _, outputTypes) <- connections
        ]
  let allTypedConnections = concat typedInputs ++ concat typedOutputs

  let updatedSignals =
        [ case lookup signalId allTypedConnections of
            Just signalType ->
              (signalId, IRSignal signalId signalType source target)
            Nothing ->
              (signalId, IRSignal signalId IRUnknownType source target)
        | (signalId, IRSignal _ _ source target) <- signals finalContext
        ]

  trace
    ( "udpated signals: "
        ++ show updatedSignals
    )
    (pure finalContext)
  where
    aux :: Either String (TranslationContext, [(IRId, IRSignal)]) -> IRSignal -> Either String (TranslationContext, [(IRId, IRSignal)])
    aux acc' currentSignal@(IRSignal signalId (sourceId, sourceRate) (targetId, targetRate)) = do
      (currentContext, acc) <- acc'
      let maybebinder = lookup sourceId (binders currentContext)
       in case maybebinder of
            Just associatedbinder -> case associatedbinder of
              PcId pcId ->
                -- Signal source was a temporary binder, sourceRate represents
                -- the index of the output rather than a rate
                case (lookup pcId (pcRates currentContext)) of
                  Nothing -> Left ("updateConstructorsAndSignals - No rates found for process constructor: " ++ show pcId)
                  Just (_, outRates) -> do
                    let newSig = IRSignal signalId (pcId, outRates !! sourceRate) (targetId, targetRate)
                    context1 <- updateConstructorsOutputs currentContext signalId pcId sourceRate
                    pure (context1, (signalId, newSig) : acc)
              _ -> Left ("updateConstructorsAndSignals - Binder is not associated with any process constructors")
            Nothing -> do
              -- Signal source was already a process constructor meaning it
              -- only had 1 output, thus passing index zero
              context1 <- updateConstructorsOutputs currentContext signalId sourceId 0
              pure (context1, (signalId, currentSignal) : acc)

-- | Helper function for `createSignalsFromArguments` which returns the id and
-- rate for the source of a signal based on an argument.
--
-- NOTE: If argument does not directly represent a process constructor then a
-- temporary source is returned. It will have to be updated later by
-- `updateConstructorsAndSignals` when all binders have been identified.
getSourceFromArgument :: TranslationContext -> IRId -> Either String (IRId, Int)
getSourceFromArgument context argument =
  if elem argument (systemInputs context)
    then pure (argument, 1)
    else
      let maybeBinder = (lookup argument (binders context))
       in case maybeBinder of
            -- Argument is associated with a binder which directly represents a
            -- process constructor meaning said constructor only has 1 output.
            Just (PcId pcId) ->
              case (lookup pcId (pcRates context)) of
                Just (_, rates) -> pure (pcId, rates !! 0)
                Nothing -> Left ("getSourceFromArgument - No rates found for process constructor: " ++ show pcId)
            -- Argument is associated with a binder which indirectly represents
            -- a process constructor meaning said constructor has multiple
            -- outputs. Will temporarily use the binder as the signal source.
            -- This will be updated later in the translation when all binders
            -- have been identified.
            Just (Binding bindingId index) -> pure (bindingId, index)
            Nothing -> Left ("getSourceFromArgument - Unable to identify argument as a system input or binder: " ++ show argument)
