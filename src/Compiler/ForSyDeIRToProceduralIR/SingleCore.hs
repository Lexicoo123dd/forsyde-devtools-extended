module Compiler.ForSyDeIRToProceduralIR.SingleCore (translateSingleCoreProgram) where

import ArgumentsMain (InputType (..), Runs (..))
import Compiler.ForSyDeIR
import Compiler.ForSyDeIRToProceduralIR.Common
import Compiler.ProceduralIR
import Compiler.Scheduler.Common (Schedule (..), recordToFlatSchedule)

translateSingleCoreProgram :: Schedule -> TranslationContext -> IRSystem -> Program
translateSingleCoreProgram schedule initialContext (IRSystem (_inputList, _outputList) constructors _signalList functionList) = do
  let scheduleList = recordToFlatSchedule schedule
      bufferList = schedBufferSizes schedule
      context1 = foldl' translateIRConstructor initialContext constructors
      context2 = foldl' translateBuffer context1 bufferList
      context3 = foldl' (translateIRFunctionToGlobals constructors) context2 functionList
      main = translateContextToMain context3 scheduleList
   in Prog (reverse (initFunctions context3) ++ [main] ++ reverse (functions context3))

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
