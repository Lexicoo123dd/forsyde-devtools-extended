module Compiler.CoreType
  ( splitFunctionType,
    translateCoreTypeToIR,
    extractOutputTypes,
  )
where

import Compiler.Types
import GHC.Plugins

splitFunctionType :: Type -> ([Type], Type)
splitFunctionType ty =
  case splitFunTy_maybe ty of
    Just (_, _, argType, resultType) ->
      let (inputTypes, outputType) = splitFunctionType resultType
       in (argType : inputTypes, outputType)
    Nothing ->
      ([], ty)

translateCoreTypeToIR :: Type -> Maybe DataType
translateCoreTypeToIR ty =
  case splitTyConApp_maybe ty of
    Just (tyCon, [])
      | tyCon == intTyCon -> Just TInt
      | tyCon == floatTyCon -> Just TFloat
      | tyCon == boolTyCon -> Just TBool
    Just (tyCon, [elementType])
      | tyCon == listTyCon ->
          translateCoreTypeToIR elementType
    _ -> Nothing

extractOutputTypes :: Type -> Maybe [DataType]
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
