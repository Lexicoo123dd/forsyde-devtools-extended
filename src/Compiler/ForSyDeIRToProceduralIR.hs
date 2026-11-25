module Compiler.ForSyDeIRToProceduralIR where

import ArgumentsMain (InputType (Predefined, StdIn), Runs (Limited, Perpetual), Target (..))
import Data.List (find, nub)
import Compiler.CoreIRToProceduralIR (translateIRFunction)
import Compiler.ForSyDeIR
import Compiler.ProceduralIR
import Compiler.Scheduler.Common (ActorInstance, Schedule (..), recordToFlatSchedule)
import GHC hiding (Target, targetId)

-- | Buffer representation kind
data BufferKind = BufferNonBlocking | BufferBlocking
  deriving (Show, Eq)

-- | Determines whether a signal buffer should be blocking or non-blocking.
-- In single-core execution (<= 1 core), buffers are non-blocking.
-- In multi-core execution, buffers use thread-safe blocking SPSC queues.
determineBufferKind :: [[ActorInstance]] -> BufferKind
determineBufferKind coreSchedule
  | length coreSchedule <= 1 = BufferNonBlocking
  | otherwise = BufferBlocking

-- | Generates an `input.h` header with #define NUM_ITERATIONS, consumption rates,
-- and placeholder static arrays for each system input.
generateInputHeader :: IRSystem -> Schedule -> String
generateInputHeader (IRSystem (inputList, _) _ irSignals _) schedule =
  let flatSched = recordToFlatSchedule schedule
      headerGuard = "#ifndef INPUT_H\n#define INPUT_H\n\n#define NUM_ITERATIONS 1\n#define iteration_max NUM_ITERATIONS\n\n"
      footerGuard = "#endif // INPUT_H\n"

      genInputDef sigId =
        let (tgtActorId, tgtRate) = case find (\(IRSignal id' _ _) -> id' == sigId) irSignals of
              Just (IRSignal _ _ (dstId, rate)) -> (dstId, rate)
              Nothing -> (sigId, 1)
            reps = length (filter (== tgtActorId) flatSched)
            totalRate = max 1 (reps * tgtRate)
            rateDef = "#define RATE_" ++ show sigId ++ " " ++ show totalRate ++ "\n"
            arrayDecl = "static const token input_" ++ show sigId ++ "[NUM_ITERATIONS * RATE_" ++ show sigId ++ "] = { 0 };\n"
         in rateDef ++ arrayDecl ++ "\n"

      body =
        if null inputList
          then ""
          else "/* Input consumption rates per schedule period and placeholder input arrays */\n"
                 ++ concatMap genInputDef inputList
   in headerGuard ++ body ++ footerGuard

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

initialTranslationContext :: DynFlags -> InputType -> Runs -> [(IRId, IRSignal)] -> [(IRId, Int)] -> [IRId] -> [IRId] -> [(IRId, IRId)] -> TranslationContext
initialTranslationContext dflags input r lookupSignalList bList inputList outputList delayBufferList =
  TranslationContext
    { flags = dflags,
      inputType = input,
      runs = r,
      lookupSignals = lookupSignalList,
      actors = [],
      signals = [],
      ioTokens = [],
      initBuffers = [],
      freeBuffers = [],
      schedulerBufferList = bList,
      systemInputs = inputList,
      systemOutputs = outputList,
      delayBuffers = delayBufferList,
      initDelay = [],
      initFunctions = [],
      functions = []
    }

-- | Translates a ForSyDe IR `IRSystem` into a Procedural IR `Program`. Requires
-- the schedule, buffer, and delay buffer lists from the SDF scheduler as an
-- input. Also requires an associated list of signals for lookups.
translateIRSystemToProgram :: DynFlags -> Schedule -> [(IRId, IRSignal)] -> Target -> InputType -> Runs -> IRSystem -> Program
translateIRSystemToProgram dflags schedule lookupSignalList target input r irSystem@(IRSystem (inputList, outputList) constructors _signalList functionList) = do
  let schedMatrix = schedPerCore schedule
      bufferList = schedBufferSizes schedule
      delayBufferList = schedDelayBuffers schedule
      initialContext = initialTranslationContext dflags input r lookupSignalList bufferList inputList outputList delayBufferList
  if length schedMatrix <= 1
    then
      translateSingleCoreProgram schedule initialContext irSystem
    else
      translateMultiCoreProgram schedule initialContext target constructors functionList inputList outputList irSystem

translateSingleCoreProgram :: Schedule -> TranslationContext -> IRSystem -> Program
translateSingleCoreProgram schedule initialContext (IRSystem (_inputList, _outputList) constructors _signalList functionList) = do
  let scheduleList = recordToFlatSchedule schedule
      bufferList = schedBufferSizes schedule
      context1 = foldl' translateIRConstructor initialContext constructors
      context2 = foldl' translateBuffer context1 bufferList
      context3 = foldl' (translateIRFunctionToGlobals constructors) context2 functionList
      main = translateContextToMain context3 scheduleList
   in Prog (reverse (initFunctions context3) ++ [main] ++ reverse (functions context3))

translateMultiCoreProgram :: Schedule -> TranslationContext -> Target -> [IRConstructor] -> [IRFunction] -> [IRId] -> [IRId] -> IRSystem -> Program
translateMultiCoreProgram schedule initialContext target constructors functionList inputList outputList _irSystem =
  let schedMatrix = schedPerCore schedule
      bufferList = schedBufferSizes schedule
      numCores = length schedMatrix
      -- Multi-core uses Predefined input mode with input.h
      context1 = foldl' translateIRConstructor initialContext constructors
      context2 = foldl' (translateIRFunctionToGlobals constructors) context1 functionList
      -- 1. Global buffer declarations: buffer_t *s_X;
      globalBufferDecls =
        map (\(bId, _) -> GVarDeclare (TPointer (TIdent "buffer_t")) (show bId)) bufferList
      -- 2. Worker functions for each core
      workerFunctions =
        [ makeWorkerFunction context1 (schedMatrix !! c) c target inputList outputList
        | c <- [0 .. numCores - 1]
        ]
      -- 3. Multi-core main()
      mainFunc = makeMultiCoreMain context1 schedule bufferList target numCores
   in Prog (globalBufferDecls ++ reverse (initFunctions context2) ++ workerFunctions ++ [mainFunc] ++ reverse (functions context2))

makeWorkerFunction :: TranslationContext -> [ActorInstance] -> Int -> Target -> [IRId] -> [IRId] -> Global
makeWorkerFunction context coreFirings coreIdx target inputList outputList =
  let coreActorIds = map fst coreFirings
      sigLookup = lookupSignals context
      coreInputs =
        nub
          [ sigId
          | sigId <- inputList,
            case lookup sigId sigLookup of
              Just (IRSignal _ _ (dstId, _)) -> elem dstId coreActorIds
              Nothing -> False
          ]
      coreOutputs =
        nub
          [ sigId
          | sigId <- outputList,
            case lookup sigId sigLookup of
              Just (IRSignal _ (srcId, _) _) -> elem srcId coreActorIds
              Nothing -> False
          ]
      inputVarDefs = map (\s -> SVarDef TInt ("i_" ++ show s) (EInt 0)) coreInputs
      outputVarDecls = map (\s -> SVarDecl TInt ("output_" ++ show s)) coreOutputs
      resetInputStmts = map (\s -> SVarAssign ("i_" ++ show s) (EInt 0)) coreInputs
      firingStmts =
        concatMap
          ( \(actId, _) ->
              case lookup actId (actors context) of
                Just stmts -> stmts
                Nothing -> error ("makeWorkerFunction: actor not found: " ++ show actId)
          )
          coreFirings
      loopBody = SScope (resetInputStmts ++ firingStmts)
      iterLoop =
        SFor
          (SVarDef TInt "iteration_current" (EInt 0))
          (EBinOp Less (EVar "iteration_current") (EVar "NUM_ITERATIONS"))
          (SExpr (EUnOp Increment (EVar "iteration_current")))
          loopBody
      returnStmt = case target of
        PC -> SReturn (Just (EVar "NULL"))
        PICO2 -> SReturn Nothing
      workerBody = SScope (inputVarDefs ++ outputVarDecls ++ [iterLoop, returnStmt])
      funcName = "run_core_" ++ show coreIdx
   in case target of
        PC -> GFuncDef Nothing (TPointer TVoid) funcName [(TPointer TVoid, "arg")] workerBody
        PICO2 -> GFuncDef Nothing TVoid funcName [] workerBody

makeMultiCoreMain :: TranslationContext -> Schedule -> [(IRId, Int)] -> Target -> Int -> Global
makeMultiCoreMain context sched bufferList target numCores =
  let schedMatrix = schedPerCore sched
      initStmt = SExpr (ECall "init" [])
      allocBufferStmt (bId, bSize) =
        let kind = determineBufferKind schedMatrix
            allocFn = case kind of
              BufferNonBlocking -> "buffer_nonblocking_new"
              BufferBlocking -> "buffer_blocking_new"
         in SAssign (EVar (show bId)) (ECall allocFn [EInt bSize])
      allocStmts = map allocBufferStmt bufferList
      delayStmts = reverse $ initDelay context

      -- benchStartStmts = if benchmarkEnabled context then [SExpr (ECall "BENCH_START" [])] else []
      -- benchEndStmts = if benchmarkEnabled context then [SExpr (ECall "BENCH_END" [])] else []

      threadStmts = case target of
        PC ->
          let otherCores = [1 .. numCores - 1]
              threadDecl c = SVarDecl (TIdent "pthread_t") ("thread_" ++ show c)
              createThread c =
                SExpr
                  ( ECall
                      "pthread_create"
                      [ EReference (EVar ("thread_" ++ show c)),
                        EVar "NULL",
                        EVar ("run_core_" ++ show c),
                        EVar "NULL"
                      ]
                  )
              runCore0 = SExpr (ECall "run_core_0" [EVar "NULL"])
              joinThread c = SExpr (ECall "pthread_join" [EVar ("thread_" ++ show c), EVar "NULL"])
           in map threadDecl otherCores
                -- ++ benchStartStmts
                ++ map createThread otherCores
                ++ [runCore0]
                ++ map joinThread otherCores
                -- ++ benchEndStmts
        PICO2 ->
          let launchCore1 = SExpr (ECall "multicore_launch_core1" [EVar "run_core_1"])
              runCore0 = SExpr (ECall "run_core_0" [])
           -- in benchStartStmts ++ [launchCore1, runCore0] ++ benchEndStmts
           in [launchCore1, runCore0]

      freeStmts = map (\(bId, _) -> SExpr (ECall "buffer_free" [EVar (show bId)])) bufferList
      retStmt = SReturn (Just (EInt 0))

      mainBody = SScope ([initStmt] ++ allocStmts ++ delayStmts ++ threadStmts ++ freeStmts ++ [retStmt])
   in GFuncDef Nothing TInt "main" [] mainBody

-- | Translates the `TranslationContext` and Schedule into a main function.
translateContextToMain :: TranslationContext -> [IRId] -> Global
translateContextToMain context scheduleList =
  let scheduledStmts = scheduleActors context scheduleList
      scheduledInputStmts = case ((inputType context), (runs context)) of
        (StdIn, _) -> []
        (Predefined, Perpetual) ->
          [SVarAssign "iteration_current" (EBinOp Plus (EVar "iteration_current") (EInt 1))]
            ++ (foldl' resetIterationVariablesFromInputs [] (systemInputs context))
            ++ [ SIf (EBinOp Equal (EVar "iteration_current") (EVar "iteration_max")) (SScope [SVarAssign "iteration_current" (EInt 0)]) Nothing
               ]
        (Predefined, Limited _) ->
          [SVarAssign "iteration_current" (EBinOp Plus (EVar "iteration_current") (EInt 1))]
            ++ (foldl' resetIterationVariablesFromInputs [] (systemInputs context))
            ++ [ SIf
                   (EBinOp Equal (EVar "iteration_current") (EVar "iteration_max"))
                   ( SScope
                       [ SVarAssign "iteration_current" (EInt 0),
                         SVarAssign
                           "run_current"
                           (EBinOp Plus (EVar "run_current") (EInt 1))
                       ]
                   )
                   Nothing
               ]
        where
          resetIterationVariablesFromInputs acc x = SVarAssign ("i_" ++ show x) (EInt (0)) : acc
      scheduledRunsStmts = case ((inputType context), (runs context)) of
        (StdIn, _) -> []
        (Predefined, Perpetual) -> []
        (Predefined, (Limited _)) -> [SIf (EBinOp Equal (EVar "run_current") (EVar "run_max")) (SScope [SBreak]) Nothing]
      whileStmt = SWhile (EInt 1) (SScope (scheduledStmts ++ scheduledInputStmts ++ scheduledRunsStmts))
      mainInitInputStmts = case (inputType context) of
        StdIn -> [SExpr (ECall "init" []), SVarDef TInt "status" (EInt 1)]
        Predefined -> [SExpr (ECall "init" []), SVarDef TInt "iteration_current" (EInt (0))]
      mainInitRunsStmts = case ((inputType context), (runs context)) of
        (StdIn, _) -> []
        (Predefined, Perpetual) -> []
        (Predefined, Limited x) -> [SVarDef TInt "run_current" (EInt (0)), SVarDef TInt "run_max" (EInt (x))]
      mainInitIterStmts = case (inputType context) of
        StdIn -> []
        Predefined -> foldl' defineIterationVariablesFromInputs [] (systemInputs context)
        where
          defineIterationVariablesFromInputs acc x = SVarDef TInt ("i_" ++ show x) (EInt (0)) : acc
      mainInitStmts =
        mainInitInputStmts
          ++ mainInitIterStmts
          ++ mainInitRunsStmts
          ++ reverse (initBuffers context)
          ++ reverse (ioTokens context)
          ++ reverse (initDelay context)
      mainFreeStmts = reverse (freeBuffers context) ++ [SReturn (Just (EInt 0))]
      mainBody = SScope (mainInitStmts ++ [whileStmt] ++ mainFreeStmts)
   in GFuncDef Nothing TInt "main" [] mainBody

-- | Returns a list of statements corresponding to actor function calls based
-- on a provided Schedule. Note that actor function call statements include
-- the associated minimum runtime enviorment statements for interacting with
-- standard in and out.
scheduleActors :: TranslationContext -> [IRId] -> [Statement]
scheduleActors context scheduleList = foldl' foldSchedule [] scheduleList
  where
    foldSchedule :: [Statement] -> IRId -> [Statement]
    foldSchedule acc actorId =
      let actorStmts = case lookup actorId (actors context) of
            Just stmts -> stmts
            Nothing -> error ("schedule - actor not found: " ++ show actorId)
       in (acc ++ actorStmts)

-- | Translates a component of the buffer size list provided by the SDF
-- scheduler into an io token statment, initialisation statement, and freeing
-- statement which are used to update the `TranslationContext`. Note that io
-- token statements are only created for buffers which relate to with system
-- inputs and outputs.
translateBuffer :: TranslationContext -> (IRId, Int) -> TranslationContext
translateBuffer initialContext (bufferId, bufferSize) =
  let initStmt = SVarDef (TPointer (TIdent "buffer_nonblocking")) (show bufferId) (ECall "buffer_nonblocking_new" [EInt bufferSize])
      freeStmt = SExpr (ECall "buffer_nonblocking_free" [EVar $ show bufferId])
   in if (elem bufferId (systemInputs initialContext))
        then case (inputType initialContext) of
          -- If stdin is used, then need to define input handling arrays
          StdIn ->
            let ioTokenStmt = SArrayDecl TInt ("input_" ++ show bufferId) [EInt bufferSize]
             in initialContext {ioTokens = ioTokenStmt : ioTokens initialContext, initBuffers = initStmt : initBuffers initialContext, freeBuffers = freeStmt : freeBuffers initialContext}
          -- If input is predefined, then do not create "input_" arrays.
          -- But you still need to create the "s_" buffers for those signals
          Predefined ->
            initialContext {initBuffers = initStmt : initBuffers initialContext, freeBuffers = freeStmt : freeBuffers initialContext}
        else
          if (elem bufferId (systemOutputs initialContext))
            then
              let ioTokenStmt = SVarDecl TInt ("output_" ++ show bufferId)
               in initialContext {ioTokens = ioTokenStmt : ioTokens initialContext, initBuffers = initStmt : initBuffers initialContext, freeBuffers = freeStmt : freeBuffers initialContext}
            else
              initialContext {initBuffers = initStmt : initBuffers initialContext, freeBuffers = freeStmt : freeBuffers initialContext}

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
