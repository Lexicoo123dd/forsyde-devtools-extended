{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Compiler.CoreIRToProceduralIR where

import Compiler.CoreIR (prettyCoreAlt)
import Compiler.ForSyDeIR
import Compiler.ProceduralIR
import Compiler.Types (cUnionField)
import Data.List (elemIndex)
import Data.Maybe (mapMaybe)
import GHC hiding (Type, targetId)
import GHC.Core
import GHC.Plugins hiding (Type)
import Utilities (Stack, emptyStack, pop, push, stackToList)

-- | The `TranslationContext` is a data type which is used to pass around
-- context required to complete the translation of Core IR to Procedural IR.
data TranslationContext = TranslationContext
  { flags :: DynFlags, -- Stores `DynFlags` for safely obtaining strings
    inputIds :: [(String, DataType)], -- List of input signal names
    outputIds :: [(String, DataType)], -- List of output signal names
    functionInputs :: [IRId], -- List of function inputs ids
    functionVariables :: [(IRId, Expression)], -- Associated list of functionIds and expression
    scope :: Statement -- Scope of function definition
  }

-- | The `FunctionContent` data type is used to represent the individual
-- components that make up the contents of a function expression using Core IR.
data FunctionContent
  = FVar Var
  | FLit DataLiteral
  | FCons
  | FTuple
  | FEmptyList
  | FPlus
  | FMinus
  | FMultiply
  | FDiv
  | FNegate

initialTranslationContext :: DynFlags -> TranslationContext
initialTranslationContext dflags =
  TranslationContext
    { flags = dflags,
      inputIds = [],
      outputIds = [],
      functionInputs = [],
      functionVariables = [],
      scope = SScope []
    }

-- | Translates an `IRFunction` into a Procedural IR function declaration and
-- optional definition.
translateIRFunction :: IRFunction -> DynFlags -> [IRConstructor] -> Maybe (Global, Maybe Global)
translateIRFunction function dflags constructors =
  let context = initialTranslationContext dflags
   in case function of
        IRFunction functionId Nothing typeDef ->
          case (findActorFromFunctionId functionId constructors) of
            [] -> Nothing
            actor : _ ->
              let (parameterList, _context1) = getParameterList context actor typeDef
                  (functionDeclarationGlobal) = getFunctionDeclaration parameterList functionId typeDef
               in Just (functionDeclarationGlobal, Nothing)
        IRFunction functionId (Just functionExpr) typeDef ->
          case (findActorFromFunctionId functionId constructors) of
            [] -> Nothing
            actor : _ ->
              let (parameterList, context1) = getParameterList context actor typeDef
                  functionDeclarationGlobal = getFunctionDeclaration parameterList functionId typeDef
                  context2 = translateFunctionExpr context1 0 0 functionExpr
                  functionDefinitionGlobal = getFunctionDefinition context2 parameterList functionId typeDef
               in Just (functionDeclarationGlobal, Just functionDefinitionGlobal)

-- Helper function which returns the `IRActor`s associated with a provided
-- function id
findActorFromFunctionId :: IRId -> [IRConstructor] -> [IRConstructor]
findActorFromFunctionId targetFunctionId constructors =
  let checkIRConstructor :: IRConstructor -> Maybe IRConstructor
      checkIRConstructor actor@(IRActor _ _ functionId _)
        | functionId == targetFunctionId = Just actor
        | otherwise = Nothing
      checkIRConstructor _ = Nothing
   in mapMaybe checkIRConstructor constructors

-- Helper function which builds the parameter list for a `ProceduralIR` function
-- based on the input and output signals of provided actor. Updates the
-- `TranslationContext` with the input and output ids.
getParameterList :: TranslationContext -> IRConstructor -> ([DataType], [DataType]) -> ([(Type, String)], TranslationContext)
getParameterList context (IRActor _ _ _ (inputSignals, outputSignals)) (inputTypes, outputTypes) =
  let inputParameters = map (\(index, _) -> (TPointer (TIdent "token"), "input_" ++ show index)) (zip [1 :: Int ..] inputSignals)
      outputParameters = map (\(index, _) -> (TPointer (TIdent "token"), "output_" ++ show index)) (zip [1 :: Int ..] outputSignals)
      parametersList = inputParameters ++ outputParameters
      inputIdList = zip (map snd inputParameters) inputTypes
      outputIdList = zip (map snd outputParameters) outputTypes
      context1 = context {inputIds = inputIdList, outputIds = outputIdList}
   in (parametersList, context1)
getParameterList context (IRDelay _ _ _) _types = ([], context)

getFunctionDeclaration :: [(Type, String)] -> IRId -> ([DataType], [DataType]) -> Global
getFunctionDeclaration parameterList functionId typeDef =
  let functionDeclareGlobal = GFuncDeclare (Just Static) TVoid (show functionId) parameterList typeDef
   in functionDeclareGlobal

getFunctionDefinition :: TranslationContext -> [(Type, String)] -> IRId -> ([DataType], [DataType]) -> Global
getFunctionDefinition context parametersList functionId typeDef =
  let stmtScope = scope context
      functionDefinitionGlobal = GFuncDef (Just Static) TVoid (show functionId) parametersList typeDef stmtScope
   in functionDefinitionGlobal

-- | Translates the `CoreExpr` associated with an `IRFunction` into a Procedural
-- IR scope. The input id argument represents the current input being pattern
-- matched. The input index argument represents index of the input the pattern
-- matched binder represents.
translateFunctionExpr :: TranslationContext -> Int -> Int -> CoreExpr -> TranslationContext
translateFunctionExpr context inputId inputIndex expr = case expr of
  -- Explicitly ignore lambdas refering to type-level binders
  Lam b e | (isTyCoVar b || (isPredTy . varType) b) -> translateFunctionExpr context inputId inputIndex e
  -- Function with 4 inputs
  Lam b1 (Lam b2 (Lam b3 (Lam b4 e))) ->
    let context1 = context {functionInputs = [(IRVar b1), (IRVar b2), (IRVar b3), (IRVar b4)]}
     in translateFunctionExpr context1 inputId inputIndex e
  -- Function with 3 inputs
  Lam b1 (Lam b2 (Lam b3 e)) ->
    let context1 = context {functionInputs = [(IRVar b1), (IRVar b2), (IRVar b3)]}
     in translateFunctionExpr context1 inputId inputIndex e
  -- Function with 2 inputs
  Lam b1 (Lam b2 e) ->
    let context1 = context {functionInputs = [(IRVar b1), (IRVar b2)]}
     in translateFunctionExpr context1 inputId inputIndex e
  -- Function with 1 input
  Lam b e ->
    let context1 = context {functionInputs = [(IRVar b)]}
     in translateFunctionExpr context1 inputId inputIndex e
  Let _ e -> translateFunctionExpr context inputId inputIndex e
  -- The Core IR `Case` data type is used in function expressions to pattern
  -- match on a functions inputs. Checks if the current binder being pattern
  -- matched is a function input and get its index.
  Case (Var i) _ _ty alts -> case elemIndex (IRVar i) (functionInputs context) of
    -- The index of an input in the function inputs list is used to id the input
    -- in Procedural IR.
    Just newInputId ->
      -- Pattern matching on a new function input pass new input id and reset
      -- index to zero
      let context1 = translateAltList context alts (Just (Var i)) newInputId 0
       in context1
    Nothing ->
      -- Still pattern maching on previous function input continue passing
      -- current input id and increment input index
      let context1 = translateAltList context alts (Just (Var i)) inputId (inputIndex + 1)
       in context1
  -- Reached the main content of the function
  _ ->
    let functionContentList = translateOutputExprToFunctionContentList context expr []
        scopeStmt = translateFunctionContent context functionContentList
        context1 = context {scope = scopeStmt}
     in context1

-- | Helper function which identifies a binder as a function input and updates
-- the translation context with its associated Procedural IR expression
translateAltList :: TranslationContext -> [Alt CoreBndr] -> Maybe CoreExpr -> Int -> Int -> TranslationContext
translateAltList context alts cond inputId inputIndex = case alts of
  [] -> context
  _ | any isLitAlt alts ->
        translateBranch context alts cond inputId inputIndex
  (Alt (DataAlt dc) ids e) : altsTail -> case showPpr (flags context) dc of
    -- An empty list pattern match indicates the end of function input
    "[]" ->
      let context1 = translateFunctionExpr context inputId inputIndex e
       in translateAltList context1 altsTail cond inputId inputIndex
    -- A cons pattern match indicates a new function input, the following
    -- builds the associated variable expression for the input and updates the
    -- translation context
    ":" ->
      let variableId = IRVar (ids !! 0)
          inputType = snd (inputIds context !! inputId)
          typeTranslation = cUnionField inputType
          variableExpression = EMemberAccess (EArrayAccess (EVar ("input_" ++ show (inputId + 1))) (ELit (LInt inputIndex))) (typeTranslation)
          context1 = context {functionVariables = (variableId, variableExpression) : functionVariables context}
          context2 = translateFunctionExpr context1 inputId inputIndex e
       in translateAltList context2 altsTail cond inputId inputIndex
    -- A unboxed Int constructor: DataAlt I#
    -- Aliases the unboxed binder to the same expression as Int conditional
    "I#" ->
      let context1 = case (ids, cond) of
            (unboxedVar : _, Just (Var scrVar)) ->
              case lookup (IRVar scrVar) (functionVariables context) of
                Just expr -> context {functionVariables = (IRVar unboxedVar, expr) : functionVariables context}
                Nothing -> context
            _ -> context
          context2 = translateFunctionExpr context1 inputId inputIndex e
       in translateAltList context2 altsTail cond inputId inputIndex
    _ -> error ("translateAltList - unsupported data alt: " ++ showPpr (flags context) dc)
  -- Skip DEFAULT pattern match, ignoring base case since not translating errors
  (Alt DEFAULT _ _) : altsTail -> translateAltList context altsTail cond inputId inputIndex
  alt : _ -> error ("translateAltList - unsupported alt: " ++ prettyCoreAlt (flags context) alt)
  where
    isLitAlt (Alt (LitAlt _) _ _) = True
    isLitAlt _ = False

-- | Translates branching on an unboxed literal into an SIf statement
-- (Currently Int only)
translateBranch :: TranslationContext -> [Alt CoreBndr] -> Maybe CoreExpr -> Int -> Int -> TranslationContext
translateBranch context alts cond inputId inputIndex =
  let 
      -- Find THEN (literal) branch
      (lit, thenExpr) = case [ (l, e) | Alt (LitAlt l) _ e <- alts ] of
        ((l, e) : _) -> (l, e)
        [] -> error "translateBranch - missing LitAlt branch"

      -- Find ELSE (DEFAULT) branch
      elseExpr = case [e | Alt DEFAULT _ e <- alts] of
        (e : _) -> e
        [] -> error "translateBranch - missing DEFAULT branch for if statement"

      -- Resolve literal integer value
      litVal = case lit of
        LitNumber _ n -> fromIntegral n
        _ -> error $ "translateBranch - unsupported literal: " ++ showPpr (flags context) lit

      -- Resolve the target expression to compare against
      targetExpr = case cond of
        Just (Var scrVar) ->
          case lookup (IRVar scrVar) (functionVariables context) of
            Just expr -> expr
            Nothing -> EVar (showPpr (flags context) scrVar)
        _ -> case functionVariables context of
          ((_, expr) : _) -> expr
          [] -> error "translateBranch - no target expression found for condition"

      -- Procedural condition: targetExpr == litVal
      condExpr = EBinOp Equal targetExpr (EInt litVal)

      -- Translate THEN branch scope
      contextThen = translateFunctionExpr (context {scope = SScope []}) inputId inputIndex thenExpr
      thenScope = scope contextThen

      contextElse = translateFunctionExpr (context {scope = SScope []}) inputId inputIndex elseExpr
      elseScope = scope contextElse

      -- Construct SIf with the else branch
      ifStmt = SIf condExpr thenScope (Just elseScope)

      existingStmts = case scope context of
        SScope stmts -> stmts
        other -> [other]
   in context {scope = SScope (existingStmts ++ [ifStmt])}

-- | Translates the main `CoreExpr` of an `IRFunction` into a list of
-- `FunctionContent`. The returned `FunctionContent` list is in reverse polish
-- notation.
translateOutputExprToFunctionContentList :: TranslationContext -> CoreExpr -> [FunctionContent] -> [FunctionContent]
translateOutputExprToFunctionContentList context expr acc = case expr of
  App e a ->
    let acc1 = translateOutputExprToFunctionContentList context e acc
     in translateOutputExprToFunctionContentList context a acc1
  (Var varId) -> case (showPpr (flags context)) varId of
    ":" -> FCons : acc
    "(,)" -> FTuple : acc
    "(,,)" -> FTuple : acc
    "(,,,)" -> FTuple : acc
    "[]" -> FEmptyList : acc
    "+" -> FPlus : acc
    "-" -> FMinus : acc
    "*" -> FMultiply : acc
    "div" -> FDiv : acc
    "negate" -> FNegate : acc
    -- Only adds current variable id if its associated to a current variable expression
    _ -> case lookup (IRVar varId) (functionVariables context) of
      Just _ -> FVar varId : acc
      Nothing -> acc
  Lit i -> case coreLiteralToIR i of
    Just irLit -> FLit irLit : acc
    Nothing -> acc
  _ -> acc

-- | Translates a `FunctionContent` list into a Procedural IR scope.
translateFunctionContent :: TranslationContext -> [FunctionContent] -> Statement
translateFunctionContent context contentList =
  let finalExprsStack = foldl' functionContentToStack emptyStack contentList
      stmtList = foldl' stackToScope [] (zip (outputIds context) (stackToList finalExprsStack))
   in SScope stmtList
  where
    -- Helper function which translates an associated output signal id and a
    -- list of expressions into a list of statements. Each statement in the list
    -- represents an array assign of the output at an incrementing index equal
    -- to an expression.
    stackToScope :: [Statement] -> ((String, DataType), [Expression]) -> [Statement]
    stackToScope acc ((outputSignalId, outputSignalType), exprList) =
      let typeTranslation = cUnionField outputSignalType
          outputStmts = map (\(idx, expr) -> SArrayAssign outputSignalId (ELit (LInt idx)) (Just typeTranslation) expr) (zip [0 ..] exprList)
       in (acc ++ outputStmts)
    -- Helper function to handle the case when the `FunctionContent` represents
    -- a `BinaryOperator`
    binOpContentToStack :: BinaryOperator -> Stack [Expression] -> Stack [Expression]
    binOpContentToStack binOp exprsStack =
      let (exprList, exprsStack1) = case (pop exprsStack) of
            Nothing -> error "translateFunctionContent - empty expression stack when popping"
            Just (elist, stack1) -> (elist, stack1)
          (expr1, expr2, exprTail) = case exprList of
            (e1 : e2 : eTail) -> (EParen e1, EParen e2, eTail)
            _ -> error ("translateFunctionContent - insufficient operands for " ++ show binOp)
          newExpr = EBinOp binOp expr1 expr2
          newExprList = newExpr : exprTail
          exprsStack2 = push newExprList exprsStack1
       in exprsStack2
    -- Help function which uses a stack to convert a `FunctionContent` the
    -- Procedural IR `Expression` it represents
    functionContentToStack :: (Stack [Expression]) -> FunctionContent -> (Stack [Expression])
    functionContentToStack exprsStack content = case content of
      FEmptyList ->
        let newExprList = []
            exprsStack1 = push newExprList exprsStack
         in exprsStack1
      (FLit lit) ->
        let (exprList, exprsStack1) = case (pop exprsStack) of
              Nothing -> error "translateFunctionContent - empty expression stack when popping"
              Just (elist, stack1) -> (elist, stack1)
            expr = ELit lit
            newExprList = expr : exprList
            exprsStack2 = push newExprList exprsStack1
         in exprsStack2
      -- TODO: Need to decide how floats should be handeled
      (FBool b) ->
        let (exprList, exprsStack1) = case (pop exprsStack) of
              Nothing -> error "translateFunctionContent - empty expression stack when popping"
              Just (elist, stack1) -> (elist, stack1)
            expr = EBool b
            newExprList = expr : exprList
            exprsStack2 = push newExprList exprsStack1
         in exprsStack2
      (FVar varId) ->
        let (exprList, exprsStack1) = case (pop exprsStack) of
              Nothing -> error "translateFunctionContent - empty expression stack when popping"
              Just (elist, stack1) -> (elist, stack1)
            variableExpr = case lookup (IRVar varId) (functionVariables context) of
              Just expr -> expr
              Nothing -> error ("translateFunctionContent - variable not found: " ++ showPpr (flags context) varId)
            newExprList = variableExpr : exprList
            exprsStack2 = push newExprList exprsStack1
         in exprsStack2
      FPlus -> binOpContentToStack Plus exprsStack
      FMinus -> binOpContentToStack Minus exprsStack
      FMultiply -> binOpContentToStack Multiply exprsStack
      FDiv -> binOpContentToStack Divide exprsStack
      FNegate ->
        let (exprList, exprsStack1) = case (pop exprsStack) of
              Nothing -> error "translateFunctionContent - empty expression stack when popping"
              Just (elist, stack1) -> (elist, stack1)
            (expr1, exprTail) = case exprList of
              (e1 : eTail) -> (EParen e1, eTail)
              _ -> error "translateFunctionContent - insufficient operands for Negate"
            newExpr = EUnOp Negate expr1
            newExprList = newExpr : exprTail
            exprsStack2 = push newExprList exprsStack1
         in exprsStack2
      FCons -> exprsStack
      FTuple -> exprsStack
