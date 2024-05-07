module TypeChecking.TypeChecker (runTypeChecking) where

import Control.Monad (unless)
import Core.ErrorState
import Core.Errors
import Core.FilePositions
import Core.SyntaxTree
import Core.Type
import Data.Foldable (fold)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import IdentifierBinding.SyntaxTree
import TypeChecking.SyntaxTree
import TypeChecking.TypeChecking

runTypeChecking :: IBModule -> WithErrors TCModule
runTypeChecking m = snd $ runErrorState (typeCheckModule m) initialTypeCheckingState

typeCheckModule :: IBModule -> TypeChecker TCModule
typeCheckModule (Module () (IBModuleContent mainFunction subFunctions)) = do
  mapM_ (uncurry prepareSubFunction) (Seq.zip (Seq.fromList [1 .. Seq.length subFunctions]) subFunctions)
  checkedMainFunction <- typeCheckMainFunction mainFunction
  typeCheckSubFunctions subFunctions
  checkedSubFunctions <- getCheckedFunctions (Seq.length subFunctions)
  return $ Module () (TCModuleContent checkedMainFunction checkedSubFunctions)

prepareSubFunction :: FunctionIndex -> IBSubFunction -> TypeChecker ()
prepareSubFunction functionIndex (SubFunction subFunctionRange _ (FunctionDefinition parameters (WithTypeAnnotation _ returnTypeAnnotation))) = do
  parameterTypes <- mapM getParameterType parameters
  returnType <- case returnTypeAnnotation of
    Just returnType -> return $ fromTypeExpression returnType
    Nothing -> throwError $ FunctionMissingReturnTypeAnnotation subFunctionRange
  let functionType = FunctionType parameterTypes returnType
  setFunctionType functionIndex functionType
  where
    getParameterType :: IBWithTypeAnnotation IBIdentifier -> TypeChecker Type
    getParameterType (WithTypeAnnotation parameter parameterTypeAnnotation) = case parameterTypeAnnotation of
      Just parameterTypeExpression -> do
        let parameterType = fromTypeExpression parameterTypeExpression
        setIdentifierType (getIdentifierName parameter) parameterType
        return parameterType
      Nothing -> throwError $ FunctionMissingParameterTypeAnnotation (getRange parameter)

typeCheckMainFunction :: IBMainFunction -> TypeChecker TCMainFunction
typeCheckMainFunction (MainFunction range statements) = do
  checkedStatements <- mapM typeCheckStatement statements
  return $ MainFunction range checkedStatements

typeCheckSubFunctions :: Seq IBSubFunction -> TypeChecker ()
typeCheckSubFunctions subFunctions = do
  popResult <- popCapturedIdentifierTypes
  case popResult of
    Nothing -> return ()
    Just (functionIndex, capturedIdentifierTypes) -> case Seq.lookup (functionIndex - 1) subFunctions of
      Nothing -> throwError $ ShouldNotGetHereError "Got out of range function index in typeCheckSubFunctions"
      Just subFunction -> do
        checkedSubFunction <- typeCheckSubFunction capturedIdentifierTypes subFunction
        addCheckedFunction functionIndex checkedSubFunction
        typeCheckSubFunctions subFunctions

typeCheckSubFunction :: Seq Type -> IBSubFunction -> TypeChecker TCSubFunction
typeCheckSubFunction capturedIdentifierTypes (SubFunction subFunctionRange capturedIdentifiers (FunctionDefinition parameters (WithTypeAnnotation body returnTypeAnnotation))) = do
  unless (Seq.length capturedIdentifierTypes == Seq.length capturedIdentifiers) $
    throwError (ShouldNotGetHereError "The number of captured identifier types did not match the number of captured identifiers when type checking function definition")
  checkedCapturedIdentifiers <- mapM (uncurry typeCheckCapturedIdentifier) (Seq.zip capturedIdentifiers capturedIdentifierTypes)
  checkedParameters <- mapM checkParameterType parameters
  (returnType, returnTypeRange) <- case returnTypeAnnotation of
    Just returnTypeExpression -> do
      let returnType = fromTypeExpression returnTypeExpression
      setFunctionContext
        FunctionContext
          { contextReturnType = returnType,
            contextReturnTypeRange = getRange returnTypeExpression
          }
      return (returnType, getRange returnTypeExpression)
    Nothing -> throwError $ FunctionMissingReturnTypeAnnotation subFunctionRange
  checkedBody <- typeCheckExpression body
  {- If the body of a function always runs a return statement when evaluated, the type of the body itself doesn't need
    to match the function return type. This enables writing functions whose body is a scope expression, but with return
    type other than Nil.

    Ex: []: Int -> { return 5; }
  -}
  let bodyReturnInfo = expressionReturnInfo . getExpressionData $ checkedBody
  let bodyType = expressionType . getExpressionData $ checkedBody
  unless (bodyReturnInfo == AlwaysReturns || bodyType == returnType) $
    throwError (FunctionReturnTypeError returnTypeRange returnType (getRange body) bodyType)
  return $ SubFunction subFunctionRange checkedCapturedIdentifiers (FunctionDefinition checkedParameters (WithTypeAnnotation checkedBody ()))
  where
    typeCheckCapturedIdentifier :: IBIdentifier -> Type -> TypeChecker TCIdentifier
    typeCheckCapturedIdentifier identifier identifierType = do
      checkedIdentifier <- typeCheckIdentifier identifier
      setIdentifierType (getIdentifierName checkedIdentifier) identifierType
      return checkedIdentifier
    checkParameterType :: IBWithTypeAnnotation IBIdentifier -> TypeChecker (TCWithTypeAnnotation TCIdentifier)
    checkParameterType (WithTypeAnnotation parameter _) = do
      checkedParameter <- typeCheckIdentifier parameter
      return $ WithTypeAnnotation checkedParameter ()

typeCheckStatement :: IBStatement -> TypeChecker TCStatement
typeCheckStatement (PrintStatement statementRange expression) = do
  checkedExpression <- typeCheckExpression expression
  let statementReturnInfo = expressionReturnInfo . getExpressionData $ checkedExpression
  return $ PrintStatement (TCStatementData statementRange statementReturnInfo) checkedExpression
typeCheckStatement (VariableDeclarationStatement statementRange mutability (WithTypeAnnotation variableName typeAnnotation) value) = do
  checkedValue <- typeCheckExpression value
  let valueType = expressionType . getExpressionData $ checkedValue
  case typeAnnotation of
    Just expectedType ->
      unless (fromTypeExpression expectedType == valueType) $
        throwError (VariableDeclarationTypeError statementRange (fromTypeExpression expectedType) valueType)
    Nothing -> return ()
  checkedVariableName <- typeCheckIdentifier variableName
  setIdentifierType (getIdentifierName checkedVariableName) valueType
  let statementReturnInfo = expressionReturnInfo . getExpressionData $ checkedValue
  return $ VariableDeclarationStatement (TCStatementData statementRange statementReturnInfo) mutability (WithTypeAnnotation checkedVariableName ()) checkedValue
typeCheckStatement (VariableMutationStatement statementRange variableName value) = do
  checkedValue <- typeCheckExpression value
  let valueType = expressionType . getExpressionData $ checkedValue
  variableType <- getIdentifierType (getIdentifierName variableName)
  unless (variableType == valueType) $
    throwError (VariableMutationTypeError statementRange variableType valueType)
  checkedVariableName <- typeCheckIdentifier variableName
  let statementReturnInfo = expressionReturnInfo . getExpressionData $ checkedValue
  return $ VariableMutationStatement (TCStatementData statementRange statementReturnInfo) checkedVariableName checkedValue
typeCheckStatement (ExpressionStatement statementRange expression) = do
  checkedExpression <- typeCheckExpression expression
  let statementReturnInfo = expressionReturnInfo . getExpressionData $ checkedExpression
  return $ ExpressionStatement (TCStatementData statementRange statementReturnInfo) checkedExpression
typeCheckStatement (WhileLoopStatement statementRange condition body) = do
  checkedCondition <- typeCheckExpression condition
  let conditionType = expressionType . getExpressionData $ checkedCondition
  checkedBody <- typeCheckExpression body
  unless (conditionType == BoolType) $
    throwError (WhileLoopConditionTypeError (getRange condition) conditionType)
  let statementReturnInfo = (expressionReturnInfo . getExpressionData $ checkedCondition) `riAnd` (expressionReturnInfo . getExpressionData $ checkedBody)
  return $ WhileLoopStatement (TCStatementData statementRange statementReturnInfo) checkedCondition checkedBody
typeCheckStatement (ReturnStatement statementRange returnValue) = do
  checkedReturnValue <- mapM typeCheckExpression returnValue
  let returnValueType = case checkedReturnValue of
        Just expression -> expressionType . getExpressionData $ expression
        Nothing -> NilType
  functionContext <- getFunctionContext
  case functionContext of
    MainFunctionContext ->
      unless (returnValueType == NilType) $
        throwError (MainFunctionReturnTypeError statementRange returnValueType)
    FunctionContext {contextReturnType, contextReturnTypeRange} ->
      unless (returnValueType == contextReturnType) $
        throwError (FunctionReturnTypeError contextReturnTypeRange contextReturnType statementRange returnValueType)
  return $ ReturnStatement (TCStatementData statementRange AlwaysReturns) checkedReturnValue

typeCheckExpression :: IBExpression -> TypeChecker TCExpression
typeCheckExpression (IntLiteralExpression expressionRange value) =
  return $ IntLiteralExpression (TCExpresionData expressionRange IntType NeverReturns) value
typeCheckExpression (FloatLiteralExpression expressionRange value) =
  return $ FloatLiteralExpression (TCExpresionData expressionRange FloatType NeverReturns) value
typeCheckExpression (CharLiteralExpression expressionRange value) =
  return $ CharLiteralExpression (TCExpresionData expressionRange CharType NeverReturns) value
typeCheckExpression (StringLiteralExpression expressionRange value) =
  return $ StringLiteralExpression (TCExpresionData expressionRange StringType NeverReturns) value
typeCheckExpression (BoolLiteralExpression expressionRange value) =
  return $ BoolLiteralExpression (TCExpresionData expressionRange BoolType NeverReturns) value
typeCheckExpression (NilExpression expressionRange) =
  return $ NilExpression (TCExpresionData expressionRange NilType NeverReturns)
typeCheckExpression (VariableExpression expressionRange identifier) = do
  checkedIdentifier <- typeCheckIdentifier identifier
  expressionType <- getIdentifierType (getIdentifierName checkedIdentifier)
  return $ VariableExpression (TCExpresionData expressionRange expressionType NeverReturns) checkedIdentifier
typeCheckExpression (NegateExpression expressionRange inner) = do
  checkedInner <- typeCheckExpression inner
  expressionType <- case expressionType . getExpressionData $ checkedInner of
    IntType -> return IntType
    FloatType -> return FloatType
    unexpectedType -> throwError $ NegateExpressionTypeError expressionRange unexpectedType
  let returnInfo = expressionReturnInfo . getExpressionData $ checkedInner
  return $ NegateExpression (TCExpresionData expressionRange expressionType returnInfo) checkedInner
typeCheckExpression (AddExpression expressionRange left right) = do
  checkedLeft <- typeCheckExpression left
  checkedRight <- typeCheckExpression right
  expressionType <- case (expressionType . getExpressionData $ checkedLeft, expressionType . getExpressionData $ checkedRight) of
    (IntType, IntType) -> return IntType
    (FloatType, FloatType) -> return FloatType
    (StringType, StringType) -> return StringType
    (CharType, StringType) -> return StringType
    (StringType, CharType) -> return StringType
    (leftType, rightType) -> throwError $ AddExpressionTypeError expressionRange leftType rightType
  let returnInfo = (expressionReturnInfo . getExpressionData $ checkedLeft) `riAnd` (expressionReturnInfo . getExpressionData $ checkedRight)
  return $ AddExpression (TCExpresionData expressionRange expressionType returnInfo) checkedLeft checkedRight
typeCheckExpression (SubtractExpression expressionRange left right) = do
  checkedLeft <- typeCheckExpression left
  checkedRight <- typeCheckExpression right
  expressionType <- case (expressionType . getExpressionData $ checkedLeft, expressionType . getExpressionData $ checkedRight) of
    (IntType, IntType) -> return IntType
    (FloatType, FloatType) -> return FloatType
    (leftType, rightType) -> throwError $ SubtractExpressionTypeError expressionRange leftType rightType
  let returnInfo = (expressionReturnInfo . getExpressionData $ checkedLeft) `riAnd` (expressionReturnInfo . getExpressionData $ checkedRight)
  return $ SubtractExpression (TCExpresionData expressionRange expressionType returnInfo) checkedLeft checkedRight
typeCheckExpression (MultiplyExpression expressionRange left right) = do
  checkedLeft <- typeCheckExpression left
  checkedRight <- typeCheckExpression right
  expressionType <- case (expressionType . getExpressionData $ checkedLeft, expressionType . getExpressionData $ checkedRight) of
    (IntType, IntType) -> return IntType
    (FloatType, FloatType) -> return FloatType
    (leftType, rightType) -> throwError $ MultiplyExpressionTypeError expressionRange leftType rightType
  let returnInfo = (expressionReturnInfo . getExpressionData $ checkedLeft) `riAnd` (expressionReturnInfo . getExpressionData $ checkedRight)
  return $ MultiplyExpression (TCExpresionData expressionRange expressionType returnInfo) checkedLeft checkedRight
typeCheckExpression (DivideExpression expressionRange left right) = do
  checkedLeft <- typeCheckExpression left
  checkedRight <- typeCheckExpression right
  expressionType <- case (expressionType . getExpressionData $ checkedLeft, expressionType . getExpressionData $ checkedRight) of
    (IntType, IntType) -> return IntType
    (FloatType, FloatType) -> return FloatType
    (leftType, rightType) -> throwError $ DivideExpressionTypeError expressionRange leftType rightType
  let returnInfo = (expressionReturnInfo . getExpressionData $ checkedLeft) `riAnd` (expressionReturnInfo . getExpressionData $ checkedRight)
  return $ DivideExpression (TCExpresionData expressionRange expressionType returnInfo) checkedLeft checkedRight
typeCheckExpression (ModuloExpression expressionRange left right) = do
  checkedLeft <- typeCheckExpression left
  checkedRight <- typeCheckExpression right
  expressionType <- case (expressionType . getExpressionData $ checkedLeft, expressionType . getExpressionData $ checkedRight) of
    (IntType, IntType) -> return IntType
    (FloatType, FloatType) -> return FloatType
    (leftType, rightType) -> throwError $ ModuloExpressionTypeError expressionRange leftType rightType
  let returnInfo = (expressionReturnInfo . getExpressionData $ checkedLeft) `riAnd` (expressionReturnInfo . getExpressionData $ checkedRight)
  return $ ModuloExpression (TCExpresionData expressionRange expressionType returnInfo) checkedLeft checkedRight
typeCheckExpression (NotExpression expressionRange inner) = do
  checkedInner <- typeCheckExpression inner
  expressionType <- case expressionType . getExpressionData $ checkedInner of
    BoolType -> return BoolType
    unexpectedType -> throwError $ NotExpressionTypeError expressionRange unexpectedType
  let returnInfo = expressionReturnInfo . getExpressionData $ checkedInner
  return $ NotExpression (TCExpresionData expressionRange expressionType returnInfo) checkedInner
typeCheckExpression (AndExpression expressionRange left right) = do
  checkedLeft <- typeCheckExpression left
  checkedRight <- typeCheckExpression right
  expressionType <- case (expressionType . getExpressionData $ checkedLeft, expressionType . getExpressionData $ checkedRight) of
    (BoolType, BoolType) -> return BoolType
    (leftType, rightType) -> throwError $ AndExpressionTypeError expressionRange leftType rightType
  let returnInfo = (expressionReturnInfo . getExpressionData $ checkedLeft) `riAnd` (expressionReturnInfo . getExpressionData $ checkedRight)
  return $ AndExpression (TCExpresionData expressionRange expressionType returnInfo) checkedLeft checkedRight
typeCheckExpression (OrExpression expressionRange left right) = do
  checkedLeft <- typeCheckExpression left
  checkedRight <- typeCheckExpression right
  expressionType <- case (expressionType . getExpressionData $ checkedLeft, expressionType . getExpressionData $ checkedRight) of
    (BoolType, BoolType) -> return BoolType
    (leftType, rightType) -> throwError $ OrExpressionTypeError expressionRange leftType rightType
  let returnInfo = (expressionReturnInfo . getExpressionData $ checkedLeft) `riAnd` (expressionReturnInfo . getExpressionData $ checkedRight)
  return $ OrExpression (TCExpresionData expressionRange expressionType returnInfo) checkedLeft checkedRight
typeCheckExpression (EqualExpression expressionRange left right) = do
  checkedLeft <- typeCheckExpression left
  checkedRight <- typeCheckExpression right
  expressionType <- case (expressionType . getExpressionData $ checkedLeft, expressionType . getExpressionData $ checkedRight) of
    (IntType, IntType) -> return BoolType
    (FloatType, FloatType) -> return BoolType
    (StringType, StringType) -> return BoolType
    (CharType, CharType) -> return BoolType
    (BoolType, BoolType) -> return BoolType
    (NilType, NilType) -> return BoolType
    (leftType, rightType) -> throwError $ EqualExpressionTypeError expressionRange leftType rightType
  let returnInfo = (expressionReturnInfo . getExpressionData $ checkedLeft) `riAnd` (expressionReturnInfo . getExpressionData $ checkedRight)
  return $ EqualExpression (TCExpresionData expressionRange expressionType returnInfo) checkedLeft checkedRight
typeCheckExpression (NotEqualExpression expressionRange left right) = do
  checkedLeft <- typeCheckExpression left
  checkedRight <- typeCheckExpression right
  expressionType <- case (expressionType . getExpressionData $ checkedLeft, expressionType . getExpressionData $ checkedRight) of
    (IntType, IntType) -> return BoolType
    (FloatType, FloatType) -> return BoolType
    (StringType, StringType) -> return BoolType
    (CharType, CharType) -> return BoolType
    (BoolType, BoolType) -> return BoolType
    (NilType, NilType) -> return BoolType
    (leftType, rightType) -> throwError $ NotEqualExpressionTypeError expressionRange leftType rightType
  let returnInfo = (expressionReturnInfo . getExpressionData $ checkedLeft) `riAnd` (expressionReturnInfo . getExpressionData $ checkedRight)
  return $ NotEqualExpression (TCExpresionData expressionRange expressionType returnInfo) checkedLeft checkedRight
typeCheckExpression (GreaterExpression expressionRange left right) = do
  checkedLeft <- typeCheckExpression left
  checkedRight <- typeCheckExpression right
  expressionType <- case (expressionType . getExpressionData $ checkedLeft, expressionType . getExpressionData $ checkedRight) of
    (IntType, IntType) -> return BoolType
    (FloatType, FloatType) -> return BoolType
    (leftType, rightType) -> throwError $ GreaterExpressionTypeError expressionRange leftType rightType
  let returnInfo = (expressionReturnInfo . getExpressionData $ checkedLeft) `riAnd` (expressionReturnInfo . getExpressionData $ checkedRight)
  return $ GreaterExpression (TCExpresionData expressionRange expressionType returnInfo) checkedLeft checkedRight
typeCheckExpression (LessExpression expressionRange left right) = do
  checkedLeft <- typeCheckExpression left
  checkedRight <- typeCheckExpression right
  expressionType <- case (expressionType . getExpressionData $ checkedLeft, expressionType . getExpressionData $ checkedRight) of
    (IntType, IntType) -> return BoolType
    (FloatType, FloatType) -> return BoolType
    (leftType, rightType) -> throwError $ LessExpressionTypeError expressionRange leftType rightType
  let returnInfo = (expressionReturnInfo . getExpressionData $ checkedLeft) `riAnd` (expressionReturnInfo . getExpressionData $ checkedRight)
  return $ LessExpression (TCExpresionData expressionRange expressionType returnInfo) checkedLeft checkedRight
typeCheckExpression (GreaterEqualExpression expressionRange left right) = do
  checkedLeft <- typeCheckExpression left
  checkedRight <- typeCheckExpression right
  expressionType <- case (expressionType . getExpressionData $ checkedLeft, expressionType . getExpressionData $ checkedRight) of
    (IntType, IntType) -> return BoolType
    (FloatType, FloatType) -> return BoolType
    (leftType, rightType) -> throwError $ GreaterEqualExpressionTypeError expressionRange leftType rightType
  let returnInfo = (expressionReturnInfo . getExpressionData $ checkedLeft) `riAnd` (expressionReturnInfo . getExpressionData $ checkedRight)
  return $ GreaterEqualExpression (TCExpresionData expressionRange expressionType returnInfo) checkedLeft checkedRight
typeCheckExpression (LessEqualExpression expressionRange left right) = do
  checkedLeft <- typeCheckExpression left
  checkedRight <- typeCheckExpression right
  expressionType <- case (expressionType . getExpressionData $ checkedLeft, expressionType . getExpressionData $ checkedRight) of
    (IntType, IntType) -> return BoolType
    (FloatType, FloatType) -> return BoolType
    (leftType, rightType) -> throwError $ LessEqualExpressionTypeError expressionRange leftType rightType
  let returnInfo = (expressionReturnInfo . getExpressionData $ checkedLeft) `riAnd` (expressionReturnInfo . getExpressionData $ checkedRight)
  return $ LessEqualExpression (TCExpresionData expressionRange expressionType returnInfo) checkedLeft checkedRight
typeCheckExpression (IfThenElseExpression expressionRange condition trueBranch falseBranch) = do
  checkedCondition <- typeCheckExpression condition
  let conditionType = expressionType . getExpressionData $ checkedCondition
  let conditionReturnInfo = expressionReturnInfo . getExpressionData $ checkedCondition
  checkedTrueBranch <- typeCheckExpression trueBranch
  let trueBranchType = expressionType . getExpressionData $ checkedTrueBranch
  let trueBranchReturnInfo = expressionReturnInfo . getExpressionData $ checkedTrueBranch
  checkedFalseBranch <- mapM typeCheckExpression falseBranch
  let maybeFalseBranchType = expressionType . getExpressionData <$> checkedFalseBranch
  let falseBranchReturnInfo = case checkedFalseBranch of
        Just e -> expressionReturnInfo . getExpressionData $ e
        Nothing -> NeverReturns
  unless (conditionType == BoolType) $
    throwError (IfThenElseExpressionConditionTypeError expressionRange conditionType)
  expressionType <- case maybeFalseBranchType of
    Nothing ->
      if trueBranchType == NilType
        then return NilType
        else throwError $ IfThenExpressionBranchesTypeError expressionRange trueBranchType
    Just falseBranchType ->
      if trueBranchType == falseBranchType
        then return trueBranchType
        else throwError $ IfThenElseExpressionBranchesTypeError expressionRange trueBranchType falseBranchType
  let returnInfo = conditionReturnInfo `riAnd` (trueBranchReturnInfo `riOr` falseBranchReturnInfo)
  return $ IfThenElseExpression (TCExpresionData expressionRange expressionType returnInfo) checkedCondition checkedTrueBranch checkedFalseBranch
typeCheckExpression (ScopeExpression expressionRange statements) = do
  checkedStatements <- mapM typeCheckStatement statements
  let returnInfo = fold $ statementReturnInfo . getStatementData <$> checkedStatements
  return $ ScopeExpression (TCExpresionData expressionRange NilType returnInfo) checkedStatements
typeCheckExpression (FunctionExpression expressionRange (IBFunctionExpressionContent functionIndex capturedIdentifiers)) = do
  checkedCapturedIdentifiers <- mapM typeCheckIdentifier capturedIdentifiers
  capturedIdentifierTypes <- mapM (getIdentifierType . getIdentifierName) checkedCapturedIdentifiers
  pushCapturedIdentifierTypes functionIndex capturedIdentifierTypes
  expressionType <- getFunctionType functionIndex
  return $ FunctionExpression (TCExpresionData expressionRange expressionType NeverReturns) (TCFunctionExpressionContent functionIndex checkedCapturedIdentifiers)
typeCheckExpression (FunctionCallExpression expressionRange function arguments) = do
  checkedFunction <- typeCheckExpression function
  checkedArguments <- mapM typeCheckExpression arguments
  (parameterTypes, returnType) <- case expressionType . getExpressionData $ checkedFunction of
    FunctionType parameterTypes returnType -> return (parameterTypes, returnType)
    unexpectedType -> throwError $ FunctionCallExpressionNotAFunctionTypeError expressionRange unexpectedType
  unless (Seq.length parameterTypes == Seq.length checkedArguments) $
    throwError (FunctionCallExpressionArityError expressionRange (Seq.length parameterTypes) (Seq.length checkedArguments))
  _ <- traverse' checkArgumentType (Seq.zip parameterTypes checkedArguments)
  let returnInfo = (expressionReturnInfo . getExpressionData $ checkedFunction) `riAnd` (fold $ expressionReturnInfo . getExpressionData <$> checkedArguments)
  return $ FunctionCallExpression (TCExpresionData expressionRange returnType returnInfo) checkedFunction checkedArguments

checkArgumentType :: (Type, TCExpression) -> TypeChecker ()
checkArgumentType (parameterType, argument) = do
  let argumentType = expressionType . getExpressionData $ argument
  unless (argumentType == parameterType) $
    throwError (FunctionCallExpressionArgumentTypeError (getRange argument) parameterType argumentType)

typeCheckIdentifier :: IBIdentifier -> TypeChecker TCIdentifier
typeCheckIdentifier (Identifier range identifier) = return $ Identifier range identifier