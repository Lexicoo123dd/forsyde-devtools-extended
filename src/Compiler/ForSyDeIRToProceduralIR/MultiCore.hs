module Compiler.ForSyDeIRToProceduralIR.MultiCore (translateMultiCoreProgram) where

import ArgumentsMain (Target (..))
import Data.List (nub)
import Compiler.ForSyDeIR
import Compiler.ForSyDeIRToProceduralIR.Common
import Compiler.ProceduralIR
import Compiler.Scheduler.Common

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
