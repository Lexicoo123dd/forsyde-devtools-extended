-- | Handles the translation and type analysis of functions.
--
-- This module creates ForSyDe IR function representations from Core
-- bindings. It extracts function input and output types and identifies
-- functions referenced by process constructors.
--
-- Should handle:
--   * Creating IRFunction representations.
--   * Splitting function types into inputs and outputs.
--   * Translating supported Core types into IR types.
--   * Extracting output types from tuples and lists.
--   * Identifying function names used by process constructors.
module Compiler.CoreIRToForSyDeIR.Function (createFunction, getFunctionName) where

import Compiler.CoreIRToForSyDeIR.Context
import Compiler.ForSyDeIR
import GHC.Core
import GHC.Plugins

createFunction :: TranslationContext -> CoreBndr -> CoreExpr -> TranslationContext
createFunction context binder expr =
  let (inputTypes, outputType) = splitFunctionType (varType binder)

      translatedTypes = do
        irInputTypes <- mapM translateCoreTypeToIR inputTypes
        irOutputType <- extractOutputTypes outputType
        return (irInputTypes, irOutputType)

      functionId = IRVar binder

      newFunction =
        case translatedTypes of
          Just functionType ->
            case expr of
              App (Var i) (Type _)
                | IRVar i == IRString "undefined" ->
                    IRFunction functionId Nothing functionType
              _ ->
                IRFunction functionId (Just expr) functionType
          Nothing ->
            IRFunction functionId (Just expr) ([], [])
   in context
        { functions =
            (functionId, newFunction) : functions context
        }

-- | Helper function for `createActorSDF` which returns name of the function
-- used by the process constructor.
getFunctionName :: TranslationContext -> CoreExpr -> Maybe IRId
getFunctionName context expr = case expr of
  App e a ->
    let getFirst = getFunctionName context a
     in case getFirst of
          Just name -> Just name
          Nothing -> getFunctionName context e
  Lam _ e -> getFunctionName context e
  Let _ e -> getFunctionName context e
  -- Not the following results in undefined behaviour in C
  Var i | IRVar i == IRString "undefined" -> Just (IRString "NULL")
  Var i ->
    let name = IRVar i
     in if any (\x -> case x of IRFunction functionName _ _ -> functionName == name) (map snd (functions context))
          then
            Just name
          else Nothing
  _ -> Nothing

--- HELPERS ---

splitFunctionType :: Type -> ([Type], Type)
splitFunctionType ty =
  case splitFunTy_maybe ty of
    Just (_, _, argType, resultType) ->
      let (inputTypes, outputType) = splitFunctionType resultType
       in (argType : inputTypes, outputType)
    Nothing ->
      ([], ty)

translateCoreTypeToIR :: Type -> Maybe IRType
translateCoreTypeToIR ty =
  case splitTyConApp_maybe ty of
    Just (tyCon, [])
      | tyCon == intTyCon -> Just IRIntType
      | tyCon == floatTyCon -> Just IRFloatType
      | tyCon == boolTyCon -> Just IRBoolType
    Just (tyCon, [elementType])
      | tyCon == listTyCon ->
          translateCoreTypeToIR elementType
    _ -> Nothing

extractOutputTypes :: Type -> Maybe [IRType]
extractOutputTypes ty =
  case splitTyConApp_maybe ty of
    Just (tyCon, typeArguments)
      | isUnboxedTupleTyCon tyCon ->
          Nothing
      | isTupleTyCon tyCon ->
          fmap concat (mapM extractOutputTypes typeArguments)
    Just (tyCon, [elementType])
      | tyCon == listTyCon ->
          extractOutputTypes elementType
    _ ->
      fmap (: []) (translateCoreTypeToIR ty)
