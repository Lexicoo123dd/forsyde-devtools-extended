-- | Handles the translation of the top-level system definition.
--
-- This module traverses the Core expression representing a system,
-- identifies its inputs, outputs and internal bindings, and updates
-- the TranslationContext accordingly.
--
-- Should handle:
--   * Traversing the system Core expression.
--   * Identifying system inputs and outputs.
--   * Translating bindings within the system.
--   * Identifying connections between process constructors.
--   * Finalising the system's signals and outputs.
module Compiler.CoreIRToForSyDeIR.System (translateSystem, translateSystemExpr) where

import Compiler.CoreIR
import Compiler.CoreIRToForSyDeIR.Context
import Compiler.CoreIRToForSyDeIR.CoreAnalysis
import Compiler.CoreIRToForSyDeIR.Signal
import Compiler.ForSyDeIR
import GHC.Core
import GHC.Plugins

translateSystem :: TranslationContext -> CoreExpr -> Either String TranslationContext
translateSystem initialContext expr = case expr of
  -- Explicitly ignores lambdas refering to type-level binders
  Lam b e | (isTyCoVar b || (isPredTy . varType) b) -> translateSystem initialContext e
  Lam b e ->
    let newInput = IRVar b
        context1 = initialContext {systemInputs = newInput : (systemInputs initialContext)}
     in translateSystem context1 e
  Let (Rec binds) out -> do
    context1 <- translateSystemBinds initialContext binds
    translateSystem context1 out
  Let (NonRec b e) out -> do
    -- A `NonRec` can just be translated as a single bind
    context1 <- translateSystemBinds initialContext [(b, e)]
    translateSystem context1 out
  Case (Let (Rec binds) out) _ _ _alts -> do
    context1 <- translateSystemBinds initialContext binds
    translateSystem context1 out
  _ -> do
    outputs <- translateOutputs initialContext expr
    context1 <- foldl' updateSystemOutput (pure initialContext) outputs
    let context2 =
          context1
            { systemInputs = reverse (systemInputs context1),
              systemOutputs = reverse (systemOutputs context1)
            }
    updateConstructorsAndSignals context2

-- | Translates system binds and adds identified binders to `TranslationContext`
translateSystemBinds :: TranslationContext -> [(CoreBndr, CoreExpr)] -> Either String (TranslationContext)
translateSystemBinds initialcontext = foldl' translateSystemBind $ pure initialcontext
  where
    translateSystemBind initialContext (b, e) = do
      context <- initialContext
      (binder, context1) <- translateSystemExpr context e
      pure context1 {binders = (IRVar b, binder) : (binders context1)}

-- | Translates system `CoreExpr`. Identifies if the expression represents an
-- application of a process constructor or connection to a specific output of a
-- process constructor. In either case returns a `Binder` along side a
-- potentially updated `TranslationContext`.
translateSystemExpr :: TranslationContext -> CoreExpr -> Either String (Binder, TranslationContext)
translateSystemExpr initialContext expr = case expr of
  -- Explicitly ignores lambdas refering to type-level binders
  Lam b e | (isTyCoVar b || (isPredTy . varType) b) -> translateSystemExpr initialContext e
  Let (NonRec b e) out -> do
    let bindingId = IRVar b
    context1 <- translateSystemBinds initialContext [(b, e)]
    context2 <- translateSystem context1 out
    let binder = PcId bindingId
    pure (binder, context2)
  Case (Var i) _ _ alts -> do
    let bindingId = IRVar i
    index <- getIndexFromAlts initialContext alts
    let binder = Binding bindingId index
    pure (binder, initialContext)
  _ -> do
    (pcId, argumentIds) <- translateProcessApplication initialContext expr
    context1 <- createSignalsFromArguments initialContext pcId argumentIds
    let binder = PcId pcId
    pure (binder, context1)

-- | Helper function which identifies the outputs of a system `CoreExpr`.
translateOutputs :: TranslationContext -> CoreExpr -> Either String [IRId]
translateOutputs context expr = aux expr []
  where
    aux currentExpr outputs = case currentExpr of
      Var out -> pure $ (IRVar out) : outputs
      App (Var out) (Type _) -> pure $ (IRVar out) : outputs
      App _ (Type _) -> pure outputs
      App e (Var out) -> aux e ((IRVar out) : outputs)
      Lam _ e -> aux e outputs
      App e1 e2 -> aux e1 outputs >>= \acc -> aux e2 acc
      -- `Case` can be ignored, as the outputs defined within were translated
      -- when the binder was defined
      Case _ _ _ _alts -> pure outputs
      e -> Left $ "translateOutputs - Unsupported expression\n" ++ prettyCoreExpr (flags context) e

updateSystemOutput :: Either String TranslationContext -> IRId -> Either String TranslationContext
updateSystemOutput initialContext outputId = do
  context1 <- initialContext
  (sourceId, sourceRate) <- getSourceFromArgument context1 outputId
  if not (outputId `elem` (map fst (signals context1)))
    then
      let newSignal = IRSignal outputId (sourceId, sourceRate) (outputId, 1)
          context2 =
            context1
              { systemOutputs = outputId : (systemOutputs context1),
                signals = (outputId, newSignal) : (signals context1)
              }
       in pure context2
    else initialContext
