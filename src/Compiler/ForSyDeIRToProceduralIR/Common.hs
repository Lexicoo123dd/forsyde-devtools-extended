module Compiler.ForSyDeIRToProceduralIR.Common
  ( TranslationContext (..),
    translateActorType,
    translateIRFunctionToGlobals,
    translateIRConstructor,
  )
where

import ArgumentsMain (InputType (..), Runs)
import Compiler.CoreIRToProceduralIR (translateIRFunction)
import Compiler.ForSyDeIR
import Compiler.ProceduralIR
import GHC (DynFlags)

-- | The `TranslationContext` is a data type which is used to pass around
-- context required to complete the translation of ForSyDeIR to
-- ProceduralIR.
data TranslationContext = TranslationContext
  { flags :: DynFlags, -- Stores `DynFlags` for safely obtaining strings
    inputType :: InputType, -- Type of input handling for the program (stin or predefined)
    runs :: Runs, -- In case of predefined input, how many times to run the SDF model
    lookupSignals :: [(IRId, IRSignal)],
    actors :: [(IRId, [Statement])], -- Associated list of actor ids and statements
    signals :: [(IRId, Statement)], -- Associated list of signal ids and statement
    ioTokens :: [Statement], -- List of statements for input/output tokens
    initBuffers :: [Statement], -- List of statements for initialising signal buffers
    freeBuffers :: [Statement], -- List of statements for freeing signal buffers
    schedulerBufferList :: [(IRId, Int)], -- The buffer list that comes from the schedule
    systemInputs :: [IRId], -- List of system inputs
    systemOutputs :: [IRId], -- List of system outputs
    delayBuffers :: [(IRId, IRId)], -- Associated list of signal ids and buffer name for delay signals
    initDelay :: [Statement], -- List of statements for initialising delay tokens
    initFunctions :: [Global],
    functions :: [Global]
  }

translateActorType :: ActorType -> String
translateActorType actorType = case actorType of
  Actor11 -> "actor11SDF"
  Actor12 -> "actor12SDF"
  Actor13 -> "actor13SDF"
  Actor14 -> "actor14SDF"
  Actor21 -> "actor21SDF"
  Actor22 -> "actor22SDF"
  Actor23 -> "actor23SDF"
  Actor24 -> "actor24SDF"
  Actor31 -> "actor31SDF"
  Actor32 -> "actor32SDF"
  Actor33 -> "actor33SDF"
  Actor34 -> "actor34SDF"
  Actor41 -> "actor41SDF"
  Actor42 -> "actor42SDF"
  Actor43 -> "actor43SDF"
  Actor44 -> "actor44SDF"

getSourceRate :: TranslationContext -> IRId -> Int
getSourceRate context signalId =
  let signal = lookup signalId (lookupSignals context)
   in case signal of
        Just (IRSignal _ (_, rate) _) -> rate
        Nothing -> error ("getSourceRate - signal not found: " ++ show signalId)

getTargetRate :: TranslationContext -> IRId -> Int
getTargetRate context signalId =
  let signal = lookup signalId (lookupSignals context)
   in case signal of
        Just (IRSignal _ _ (_, rate)) -> rate
        Nothing -> error ("getTargetRate - signal not found: " ++ show signalId)

-- Get the rate from the schedule given an ActorId and bufferList
getScheduleBufferRate :: IRId -> [(IRId, Int)] -> Int
getScheduleBufferRate actorId bufferList =
  case bufferList of
    [] -> error ("buffer rate finder - actor not found: " ++ show actorId)
    (x, rate) : xs ->
      if (x == actorId)
        then
          rate
        else
          getScheduleBufferRate actorId xs

translateIRFunctionToGlobals :: [IRConstructor] -> TranslationContext -> IRFunction -> TranslationContext
translateIRFunctionToGlobals constructors currentContext function =
  let context1 = case (translateIRFunction function (flags currentContext) constructors) of
        Just (functionDeclaration, Just functionDefinition) -> currentContext {initFunctions = functionDeclaration : (initFunctions currentContext), functions = functionDefinition : (functions currentContext)}
        Just (functionDeclaration, Nothing) -> currentContext {initFunctions = functionDeclaration : (initFunctions currentContext)}
        Nothing -> currentContext
   in context1

-- | Translates an `IRConstructor` into a set of statements which are used to
-- update the `TranslationContext`.
translateIRConstructor :: TranslationContext -> IRConstructor -> TranslationContext
translateIRConstructor initialContext constructor = case constructor of
  IRDelay _ tokens (inputSignal, outputSignal) ->
    -- `IRDelay` results in a list of statements adding initial delay tokens
    -- into a signal buffer.
    let maybeBufferId = lookup outputSignal (delayBuffers initialContext)
     in case maybeBufferId of
          Just bufferId ->
            let foldDelayTokens :: [Statement] -> Int -> [Statement]
                foldDelayTokens acc token =
                  let stmt =
                        if elem bufferId (systemOutputs initialContext)
                          -- TODO: Match the output with the actor on the signal
                          then SExpr (ECall "printf" [EString "%d\n", EInt token])
                          else SExpr (ECall "write_token" [EVar $ show bufferId, EInt token])
                   in (stmt : acc)
                delayStmts = (foldl' foldDelayTokens [] tokens)
             in initialContext {initDelay = delayStmts ++ initDelay initialContext}
          Nothing -> error ("translateIRConstructor - delay buffer not found for signals: " ++ show inputSignal ++ ", " ++ show outputSignal)
  IRActor actorId actorType functionId (inputSignals, outputSignals) ->
    -- `IRActor` results in a statement calling an SDF actor function within
    -- source rates, target rates, and a function as arguments.
    let inputRates = map (getTargetRate initialContext) inputSignals
        outputRates = map (getSourceRate initialContext) outputSignals
        actorName = translateActorType actorType
        translatedInputSignals = map (translateSignalId initialContext) inputSignals
        translatedOutputSignals = map (translateSignalId initialContext) outputSignals
        actorCallStmt =
          SExpr
            ( ECall
                actorName
                ( (map (\rate -> EInt (fromIntegral rate)) inputRates)
                    ++ (map (\rate -> EInt (fromIntegral rate)) outputRates)
                    ++ (map (\bufferId -> EVar $ show bufferId) translatedInputSignals)
                    ++ (map (\bufferId -> EVar $ show bufferId) translatedOutputSignals)
                    ++ [EVar $ show functionId]
                )
            )
        inputStmts = foldl' foldActorSignals [] translatedInputSignals
        outputStmts = foldl' foldActorSignals [] translatedOutputSignals
        stmts = reverse inputStmts ++ [actorCallStmt] ++ reverse outputStmts
        context1 = initialContext {actors = (actorId, stmts) : actors initialContext}
     in context1
  where
    -- Helper function used to update a signal id into its respecitve signal
    -- buffer id. There is only a difference in the translation for signals
    -- which are source and targets of delays.
    translateSignalId :: TranslationContext -> IRId -> IRId
    translateSignalId context signalId =
      case lookup signalId (delayBuffers context) of
        Just bufferId -> bufferId
        Nothing -> signalId
    foldActorSignals :: [Statement] -> IRId -> [Statement]
    foldActorSignals acc signalId =
      if (elem signalId (systemInputs initialContext))
        then case (inputType initialContext) of
          -- If input type is stdin, then do scanf->break->write
          StdIn ->
            let bufferSize = getTargetRate initialContext signalId
                -- If actor has inputs which are system inputs the following
                -- adds statements which relate to obtaining inputs from
                -- standard in.
                scanForStmt =
                  SFor
                    (SVarDef TInt "i" (ECall "contained_tokens" [EVar $ show signalId]))
                    (EBinOp Less (EVar "i") (EInt (bufferSize)))
                    (SExpr (EUnOp Increment (EVar "i")))
                    (SScope [SVarAssign "status" (ECall "scanf" [EString "%d", EReference (EArrayAccess (EVar ("input_" ++ show signalId)) (EVar "i"))])])
                breakIfStmt = SIf (EBinOp Less (EVar "status") (EInt 1)) (SScope [SBreak]) Nothing
                writeForStmt =
                  SFor
                    (SVarDef TInt "i" (ECall "contained_tokens" [EVar $ show signalId]))
                    (EBinOp Less (EVar "i") (EInt (bufferSize)))
                    (SExpr (EUnOp Increment (EVar "i")))
                    (SScope [SExpr (ECall "write_token" [EVar $ show signalId, EArrayAccess (EVar ("input_" ++ show signalId)) (EVar "i")])])
             in (writeForStmt : breakIfStmt : scanForStmt : acc)
          -- If input type is predefined, then do write
          Predefined ->
            let bufferSize = getTargetRate initialContext signalId
                writeForStmt =
                  SFor
                    (SVarDef TInt "i" (ECall "contained_tokens" [EVar $ show signalId]))
                    (EBinOp Less (EVar "i") (EInt (bufferSize)))
                    (SExpr (EUnOp Increment (EVar "i")))
                    ( SScope
                        ( [ SExpr
                              ( ECall
                                  "write_token"
                                  [ EVar $ show signalId,
                                    EArrayAccess
                                      (EVar ("input_" ++ show signalId))
                                      ( EBinOp
                                          Plus
                                          ( EBinOp
                                              Multiply
                                              (EVar ("iteration_current"))
                                              (EInt (getScheduleBufferRate signalId (schedulerBufferList initialContext)))
                                          )
                                          (EVar ("i_" ++ show signalId))
                                      )
                                  ]
                              )
                          ]
                            ++ [SVarAssign ("i_" ++ show signalId) (EBinOp Plus (EVar ("i_" ++ show signalId)) (EInt 1))]
                        )
                    )
             in (writeForStmt : acc)
        else
          if (elem signalId (systemOutputs initialContext))
            then
              let bufferSize = getSourceRate initialContext signalId
                  -- If actor has outputs which are system outputs the
                  -- following adds statements which relate to printing
                  -- outputs to standard out.
                  scopeStmt =
                    SScope
                      [ SExpr (ECall "read_token" [EVar $ show signalId, EReference (EVar ("output_" ++ show signalId))]),
                        SExpr (ECall "printf" [EString "%d ", EVar ("output_" ++ show signalId)])
                      ]
                  forStmt =
                    SFor
                      (SVarDef TInt "i" (EInt 0))
                      (EBinOp Less (EVar "i") (EInt (bufferSize)))
                      (SExpr (EUnOp Increment (EVar "i")))
                      scopeStmt
                  newLineStmt = SExpr (ECall "printf" [EString "\n"])
               in (newLineStmt : forStmt : acc)
            else
              acc

