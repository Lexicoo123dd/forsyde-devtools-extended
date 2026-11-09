-- | Handles the translation of process constructors from GHC Core
-- into ForSyDe IR.
--
-- This module identifies supported process constructors, such as SDF
-- actors and delays, and creates their corresponding ForSyDe IR
-- representations. It also records their input and output rates.
--
-- Should handle:
--   * Identifying supported process constructors.
--   * Translating actor and delay constructors.
--   * Extracting process constructor rates and literals.
--   * Determining actor types and their input/output configuration.
--   * Registering process constructors in the TranslationContext.
--
-- Should not handle the creation or resolution of signal connections.
module Compiler.CoreIRToForSyDeIR.Process (translateCoreExpr) where

import Compiler.CoreIRToForSyDeIR.Context
import Compiler.CoreIRToForSyDeIR.Function
import Compiler.ForSyDeIR
import GHC.Core
import GHC.Plugins

-- | Creates actors and delays by pattern matching based on the number of
-- inputs to the top level function, represented by `Lam`, and inputs to the
-- process constructor, represented by `App`. If it cannot match an actor or
-- delay then it creates a function.
--
-- NOTE: This function needs to be updated with new pattern matches for
-- translation to support additional process constructors.
translateCoreExpr :: TranslationContext -> CoreBndr -> CoreExpr -> Either String TranslationContext
translateCoreExpr context' binder expr' =
  let (n, expr1) = stripLams 0 expr'
      (context, expr2) = case expr1 of
        Let (NonRec bn be) e -> (createFunction context' bn be, e)
        Let _ e -> (context', e)
        _ -> (context', expr1)
      expr = maybe expr' id (stripApps n expr2)
   in case expr of
        -- ForSyDe-Shallow SDF
        App (App (Var i) _) _
          | IRVar i == IRString "delaySDF" -> pure $ createDelaySDF context binder expr
        App (App (App (App (App (Var i) _) _) _) _) _
          | IRVar i == IRString "actor11SDF" -> createActorSDF context Actor11 binder expr
        App (App (App (App (App (App (Var i) _) _) _) _) _) _
          | IRVar i == IRString "actor12SDF" -> createActorSDF context Actor12 binder expr
          | IRVar i == IRString "actor21SDF" -> createActorSDF context Actor21 binder expr
        App (App (App (App (App (App (App (Var i) _) _) _) _) _) _) _
          | IRVar i == IRString "actor13SDF" -> createActorSDF context Actor13 binder expr
          | IRVar i == IRString "actor22SDF" -> createActorSDF context Actor22 binder expr
          | IRVar i == IRString "actor31SDF" -> createActorSDF context Actor31 binder expr
        App (App (App (App (App (App (App (App (Var i) _) _) _) _) _) _) _) _
          | IRVar i == IRString "actor14SDF" -> createActorSDF context Actor14 binder expr
          | IRVar i == IRString "actor23SDF" -> createActorSDF context Actor23 binder expr
          | IRVar i == IRString "actor32SDF" -> createActorSDF context Actor32 binder expr
          | IRVar i == IRString "actor41SDF" -> createActorSDF context Actor41 binder expr
        App (App (App (App (App (App (App (App (App (Var i) _) _) _) _) _) _) _) _) _
          | IRVar i == IRString "actor24SDF" -> createActorSDF context Actor24 binder expr
          | IRVar i == IRString "actor33SDF" -> createActorSDF context Actor33 binder expr
          | IRVar i == IRString "actor42SDF" -> createActorSDF context Actor42 binder expr
        App (App (App (App (App (App (App (App (App (App (Var i) _) _) _) _) _) _) _) _) _) _
          | IRVar i == IRString "actor34SDF" -> createActorSDF context Actor34 binder expr
          | IRVar i == IRString "actor43SDF" -> createActorSDF context Actor43 binder expr
        App (App (App (App (App (App (App (App (App (App (App (Var i) _) _) _) _) _) _) _) _) _) _) _
          | IRVar i == IRString "actor44SDF" -> createActorSDF context Actor44 binder expr
        -- ForSyDe-Atom SDF
        App (App (Var i) _) _
          | IRVar i == IRString "delay" -> pure $ createDelaySDF context binder expr
        App (App (App (Var i) _) _) _
          | IRVar i == IRString "actor11" -> createActorSDF context Actor11 binder expr
        App (App (App (App (Var i) _) _) _) _
          | IRVar i == IRString "actor12" -> createActorSDF context Actor12 binder expr
          | IRVar i == IRString "actor21" -> createActorSDF context Actor21 binder expr
        App (App (App (App (App (Var i) _) _) _) _) _
          | IRVar i == IRString "actor13" -> createActorSDF context Actor13 binder expr
          | IRVar i == IRString "actor22" -> createActorSDF context Actor22 binder expr
          | IRVar i == IRString "actor31" -> createActorSDF context Actor31 binder expr
        App (App (App (App (App (App (Var i) _) _) _) _) _) _
          | IRVar i == IRString "actor14" -> createActorSDF context Actor14 binder expr
          | IRVar i == IRString "actor23" -> createActorSDF context Actor23 binder expr
          | IRVar i == IRString "actor32" -> createActorSDF context Actor32 binder expr
          | IRVar i == IRString "actor41" -> createActorSDF context Actor41 binder expr
        App (App (App (App (App (App (App (Var i) _) _) _) _) _) _) _
          | IRVar i == IRString "actor24" -> createActorSDF context Actor24 binder expr
          | IRVar i == IRString "actor33" -> createActorSDF context Actor33 binder expr
          | IRVar i == IRString "actor42" -> createActorSDF context Actor42 binder expr
        App (App (App (App (App (App (App (App (Var i) _) _) _) _) _) _) _) _
          | IRVar i == IRString "actor34" -> createActorSDF context Actor34 binder expr
          | IRVar i == IRString "actor43" -> createActorSDF context Actor43 binder expr
        App (App (App (App (App (App (App (App (App (Var i) _) _) _) _) _) _) _) _) _
          | IRVar i == IRString "actor44" -> createActorSDF context Actor44 binder expr
        _ -> pure $ createFunction context binder expr

--- HELPERS ---

createDelaySDF :: TranslationContext -> CoreBndr -> CoreExpr -> TranslationContext
createDelaySDF context binder expr =
  let tokens = (getLits expr [])
      delayId = IRVar binder
      newDelay = IRDelay delayId tokens (Empty, Empty)
      newActorsList = (delayId, ([1], [1])) : (pcRates context)
   in context {constructors = (delayId, newDelay) : (constructors context), pcRates = newActorsList}

createActorSDF :: TranslationContext -> ActorType -> CoreBndr -> CoreExpr -> Either String TranslationContext
createActorSDF initialContext actorType binder expr =
  let lits = getRates expr
      maybeFunctionName = getFunctionName initialContext expr
   in case maybeFunctionName of
        Nothing -> Left "createActorSDF - No function found for actor"
        Just functionName ->
          let (inputRates, outputRates) = splitAt (getActorSplit actorType) lits
              actorId = IRVar binder
              baseOutputs = replicate (length outputRates) Empty
              newActor = IRActor actorId actorType functionName ([], baseOutputs)
              newActors = (actorId, (inputRates, outputRates)) : (pcRates initialContext)
              newConstructors = (actorId, newActor) : (constructors initialContext)
              context1 = initialContext {pcRates = newActors, constructors = newConstructors}
           in pure context1

-- | Helper function for `createActorSDF` which returns the index to split the
-- literals within an actor expression based on the actor type.
getActorSplit :: ActorType -> Int
getActorSplit actorType = case actorType of
  Actor11 -> 1
  Actor12 -> 1
  Actor13 -> 1
  Actor14 -> 1
  Actor21 -> 2
  Actor22 -> 2
  Actor23 -> 2
  Actor24 -> 2
  Actor31 -> 3
  Actor32 -> 3
  Actor33 -> 3
  Actor34 -> 3
  Actor41 -> 4
  Actor42 -> 4
  Actor43 -> 4
  Actor44 -> 4

-- Strip all Lams so we don't need to bother with non-eta-reduced processes.
-- We con't count the type variables as those won't produce an App in the top
-- level definition.
stripLams :: Integer -> CoreExpr -> (Integer, CoreExpr)
stripLams n expr = case expr of
  -- Explicitly strips lambdas refering to type-level binders
  Lam b e | (isTyCoVar b || (isPredTy . varType) b) -> stripLams n e
  Lam _ e | otherwise -> stripLams (n + 1) e
  _ -> (n, expr)

-- Strip n Apps if possible, otherwise Nothing
stripApps :: Integer -> CoreExpr -> Maybe CoreExpr
stripApps n expr = case expr of
  App e _ | n > 0 -> stripApps (n - 1) e
  _ | n > 0 -> Nothing
  _ | otherwise -> Just expr

-- | Helper function for `createDelaySDF` and `createActorSDF` which returns
-- all integer literals within their expression as a list.
getLits :: CoreExpr -> [IRLiteral] -> [IRLiteral]
getLits expr acc = case expr of
  Lit l ->
    case coreLiteralToIR l of
      Just irLit -> irLit : acc
      Nothing -> acc
  App e a -> getLits e (getLits a acc)
  Lam _ e -> getLits e acc
  Let _ e -> getLits e acc
  _ -> acc

-- Extract integer values for rates.
getRates :: CoreExpr -> [Int]
getRates expr =
  [n | IRInt n <- getLits expr []]
