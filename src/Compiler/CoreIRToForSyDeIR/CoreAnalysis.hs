-- | Provides helper functions for analysing and inspecting GHC Core
-- expressions.
--
-- This module contains reusable functions for recognising patterns
-- in Core expressions and extracting information needed by the
-- translation modules.
--
-- Should handle:
--   * Identifying process applications and their arguments.
--   * Analysing case alternatives and identifying output indices.
--   * Stripping lambdas and applications from Core expressions.
--   * Extracting literals and rates from Core expressions.
--
-- Should not construct ForSyDe IR objects or update the TranslationContext.
module Compiler.CoreIRToForSyDeIR.CoreAnalysis where

import Compiler.CoreIR
import Compiler.CoreIRToForSyDeIR.Context
import Compiler.ForSyDeIR
import Data.List (elemIndex)
import GHC.Core

--- HELPERS ---

-- | Helper function which identifies the process being applied and its arguments, within a system `CoreExpr`.
translateProcessApplication :: TranslationContext -> CoreExpr -> Either String (IRId, [IRId])
translateProcessApplication context expr = aux expr []
  where
    aux currentExpr arguments = case currentExpr of
      Var i -> pure (IRVar i, arguments)
      (App (App (Var i) (Type _)) (Var _)) -> pure (IRVar i, arguments)
      App e (Var a) | isId a -> aux e ((IRVar a) : arguments)
      App e (App (Var a) (Type _)) -> aux e ((IRVar a) : arguments)
      App e (Type _) -> aux e arguments
      Lam _ e -> aux e arguments
      e -> Left $ "translateProcessApplication - Unsupported CoreExpr:\n" ++ prettyCoreExpr (flags context) e

-- | Helper function which identifies the id chosen by an `AltCon` for a `Case`.
-- Returns the index of the identified id from a list of ids. These ids
-- represent the outputs of a process constructor.
getIndexFromAlts :: TranslationContext -> [Alt CoreBndr] -> Either String Int
getIndexFromAlts context alts = case alts of
  [] -> Left $ "getIndexFromAlts - Empty AltCon list"
  (Alt _ ids (Var (i))) : [] ->
    let maybeIndex = elemIndex i ids
     in case maybeIndex of
          Just index -> pure $ index
          Nothing -> Left $ "getIndexFromAlts - Unable to find: " ++ show (IRVar i)
  _ -> Left $ "getIndexFromAlts - More than one AltCon:\n" ++ prettyCoreAltList (flags context) alts
