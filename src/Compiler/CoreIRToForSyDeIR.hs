module Compiler.CoreIRToForSyDeIR (translateCoreProgram) where

import Compiler.CoreIRToForSyDeIR.Context
import Compiler.CoreIRToForSyDeIR.Process
import Compiler.CoreIRToForSyDeIR.System
import Compiler.ForSyDeIR
import GHC.Core
import GHC.Plugins

-- | Translates a `CoreProgram` into an `IRSystem` requires `DynFlags` to
-- safely convert GHC Core elements into strings. Starts with an empty
-- `TranslationContext`.
translateCoreProgram :: DynFlags -> CoreProgram -> Either String (IRSystem, [(IRId, IRSignal)])
translateCoreProgram dflags program = do
  finalContext <- foldl translateCoreBind (pure $ initialTranslationContext dflags) program
  pure
    ( IRSystem
        (systemInputs finalContext, systemOutputs finalContext)
        (map snd (constructors finalContext))
        (map snd (signals finalContext))
        (map snd (functions finalContext)),
      signals finalContext
    )

-- | Translates a top level `CoreBind`. Module information is currently ignored.
translateCoreBind :: Either String TranslationContext -> CoreBind -> Either String TranslationContext
translateCoreBind context (NonRec b e)
  | IRVar b == IRString "$trModule" = context
  | isSystemName (varName b) = context
  | IRVar b == IRString "system" = context >>= \c -> translateSystem c e
  | otherwise = context >>= \c -> translateCoreExpr c b e
-- NOTE: Currently no SDF examples have a `Rec` in the top level of their Core
-- output, thus unsure what the expected outcome should be.
translateCoreBind _ (Rec _) = Left "translateCoreBind - `Rec` used in top level of Core output"
