module IdentifierBinding.IdentifierBinder
  ( runIdentifierBinding,
  )
where

import Core.ErrorState
import Core.Errors
import Core.SyntaxTree
import Data.Sequence (Seq)
import IdentifierBinding.IdentifierBinding
import IdentifierBinding.SyntaxTree
import Parsing.SyntaxTree

runIdentifierBinding :: PModule -> WithErrors IBModule
runIdentifierBinding m = snd $ runErrorState (moduleBinder m) initialBindingState

moduleBinder :: PModule -> IdentifierBinder IBModule
moduleBinder (Module _ (MainFunction _ statements)) = withNewExpressionScope $ do
  boundStatements <- bindScope statements
  boundSubFunctions <- getBoundFunctions
  return $ Module () $ IBModuleContent (MainFunction () boundStatements) boundSubFunctions

{- We preemtively add declared variables to the current scope in a before declaration state. This lets us catch and throw
  errors in situations where a variable is used in a scope before it is later shadowed. This could be allowed, but it is
  confusing enough that it's probably worth just giving a compilation error.

  Code example:
  let x = 1;
  {
    print x;
    let x = 2;
  };

  BindingReadyStatement is just a wrapper around statement that shows that we've completed this pre-binding step.
  -}
newtype BindingReadyStatement = BindingReadyStatement PStatement

prepareStatementForBinding :: PStatement -> IdentifierBinder BindingReadyStatement
prepareStatementForBinding statement = case statement of
  (VariableDeclarationStatement range mutability (WithTypeAnnotation (Identifier _ variableName) _) _) -> do
    _ <- addVariable range mutability variableName
    return $ BindingReadyStatement statement
  _ -> return $ BindingReadyStatement statement

statementBinder :: BindingReadyStatement -> IdentifierBinder IBStatement
statementBinder
  ( BindingReadyStatement
      ( VariableDeclarationStatement
          declarationRange
          mutability
          (WithTypeAnnotation (Identifier identifierRange identifier) typeAnnotation)
          expression
        )
    ) =
    do
      IdentifierInfo {boundIdentifier} <- setVariableUsability identifier InDeclaration
      boundExpression <- expressionBinder expression
      boundTypeAnnotation <- mapM typeExpressionBinder typeAnnotation
      return $ VariableDeclarationStatement declarationRange mutability (WithTypeAnnotation (Identifier identifierRange boundIdentifier) boundTypeAnnotation) boundExpression
      `andFinally` setVariableUsability identifier Usable
statementBinder (BindingReadyStatement (VariableMutationStatement statementRange (Identifier identifierRange identifier) expression)) = do
  IdentifierInfo {boundIdentifier} <- getIdentifierBinding identifierRange True identifier
  boundExpression <- expressionBinder expression
  return $ VariableMutationStatement statementRange (Identifier identifierRange boundIdentifier) boundExpression
-- Standard cases
statementBinder (BindingReadyStatement (PrintStatement range expression)) = do
  boundExpression <- expressionBinder expression
  return $ PrintStatement range boundExpression
statementBinder (BindingReadyStatement (ExpressionStatement range expression)) = do
  boundExpression <- expressionBinder expression
  return $ ExpressionStatement range boundExpression
statementBinder (BindingReadyStatement (WhileLoopStatement range condition body)) = do
  boundCondition <- expressionBinder condition
  boundBody <- expressionBinder body
  return $ WhileLoopStatement range boundCondition boundBody
statementBinder (BindingReadyStatement (ReturnStatement range (Just expression))) = do
  boundExpression <- expressionBinder expression
  return $ ReturnStatement range (Just boundExpression)
statementBinder (BindingReadyStatement (ReturnStatement range Nothing)) =
  return $ ReturnStatement range Nothing

expressionBinder :: PExpression -> IdentifierBinder IBExpression
expressionBinder (VariableExpression expressionRange (Identifier identifierRange identifier)) = do
  IdentifierInfo {boundIdentifier} <- getIdentifierBinding expressionRange False identifier
  return $ VariableExpression expressionRange (Identifier identifierRange boundIdentifier)
expressionBinder (ScopeExpression d statements) = withNewExpressionScope $ do
  boundStatements <- bindScope statements
  return $ ScopeExpression d boundStatements
expressionBinder (FunctionExpression d (FunctionDefinition parameters (WithTypeAnnotation body returnTypeAnnotation))) = withNewFunctionScope $ do
  boundParameters <- traverse' bindParameter parameters
  boundReturnTypeAnnotation <- mapM typeExpressionBinder returnTypeAnnotation
  boundBody <- expressionBinder body
  capturedVariables <- getCapturedIdentifiers
  let functionDefinition = FunctionDefinition boundParameters (WithTypeAnnotation boundBody boundReturnTypeAnnotation)
  let subFunction = SubFunction d (toAstIdentifier . insideIdentifier <$> capturedVariables) functionDefinition
  functionIndex <- addBoundSubFunction subFunction
  return $ FunctionExpression d $ IBFunctionExpressionContent functionIndex (toAstIdentifier . outsideIdentifier <$> capturedVariables)
-- Standard cases
expressionBinder (IntLiteralExpression d value) = return $ IntLiteralExpression d value
expressionBinder (FloatLiteralExpression d value) = return $ FloatLiteralExpression d value
expressionBinder (CharLiteralExpression d value) = return $ CharLiteralExpression d value
expressionBinder (StringLiteralExpression d value) = return $ StringLiteralExpression d value
expressionBinder (BoolLiteralExpression d value) = return $ BoolLiteralExpression d value
expressionBinder (NilExpression d) = return $ NilExpression d
expressionBinder (NegateExpression d inner) = do
  boundInner <- expressionBinder inner
  return $ NegateExpression d boundInner
expressionBinder (AddExpression d left right) = do
  boundLeft <- expressionBinder left
  boundRight <- expressionBinder right
  return $ AddExpression d boundLeft boundRight
expressionBinder (SubtractExpression d left right) = do
  boundLeft <- expressionBinder left
  boundRight <- expressionBinder right
  return $ SubtractExpression d boundLeft boundRight
expressionBinder (MultiplyExpression d left right) = do
  boundLeft <- expressionBinder left
  boundRight <- expressionBinder right
  return $ MultiplyExpression d boundLeft boundRight
expressionBinder (DivideExpression d left right) = do
  boundLeft <- expressionBinder left
  boundRight <- expressionBinder right
  return $ DivideExpression d boundLeft boundRight
expressionBinder (ModuloExpression d left right) = do
  boundLeft <- expressionBinder left
  boundRight <- expressionBinder right
  return $ ModuloExpression d boundLeft boundRight
expressionBinder (NotExpression d inner) = do
  boundInner <- expressionBinder inner
  return $ NotExpression d boundInner
expressionBinder (AndExpression d left right) = do
  boundLeft <- expressionBinder left
  boundRight <- expressionBinder right
  return $ AndExpression d boundLeft boundRight
expressionBinder (OrExpression d left right) = do
  boundLeft <- expressionBinder left
  boundRight <- expressionBinder right
  return $ OrExpression d boundLeft boundRight
expressionBinder (EqualExpression d left right) = do
  boundLeft <- expressionBinder left
  boundRight <- expressionBinder right
  return $ EqualExpression d boundLeft boundRight
expressionBinder (NotEqualExpression d left right) = do
  boundLeft <- expressionBinder left
  boundRight <- expressionBinder right
  return $ NotEqualExpression d boundLeft boundRight
expressionBinder (GreaterExpression d left right) = do
  boundLeft <- expressionBinder left
  boundRight <- expressionBinder right
  return $ GreaterExpression d boundLeft boundRight
expressionBinder (LessExpression d left right) = do
  boundLeft <- expressionBinder left
  boundRight <- expressionBinder right
  return $ LessExpression d boundLeft boundRight
expressionBinder (GreaterEqualExpression d left right) = do
  boundLeft <- expressionBinder left
  boundRight <- expressionBinder right
  return $ GreaterEqualExpression d boundLeft boundRight
expressionBinder (LessEqualExpression d left right) = do
  boundLeft <- expressionBinder left
  boundRight <- expressionBinder right
  return $ LessEqualExpression d boundLeft boundRight
expressionBinder (IfThenElseExpression d condition trueExpression maybeFalseExpression) = do
  boundCondition <- expressionBinder condition
  boundTrueExpression <- expressionBinder trueExpression
  boundFalseExpression <- case maybeFalseExpression of
    Just falseExpression -> do
      boundFalseExpression <- expressionBinder falseExpression
      return $ Just boundFalseExpression
    Nothing -> return Nothing
  return $ IfThenElseExpression d boundCondition boundTrueExpression boundFalseExpression
expressionBinder (FunctionCallExpression d function arguments) = do
  boundFunction <- expressionBinder function
  boundArguments <- traverse' expressionBinder arguments
  return $ FunctionCallExpression d boundFunction boundArguments

bindScope :: Seq PStatement -> IdentifierBinder (Seq IBStatement)
bindScope statements = do
  readyStatements <- traverse' prepareStatementForBinding statements
  traverse' statementBinder readyStatements

bindParameter :: PWithTypeAnnotation PIdentifier -> IdentifierBinder (IBWithTypeAnnotation IBIdentifier)
bindParameter (WithTypeAnnotation (Identifier identifierRange identifier) typeAnnotation) = do
  IdentifierInfo {boundIdentifier} <- addParameter identifierRange identifier
  boundTypeAnnotation <- mapM typeExpressionBinder typeAnnotation
  return $ WithTypeAnnotation (Identifier identifierRange boundIdentifier) boundTypeAnnotation

-- Note that this function uses the identifier declaration range as its range, so this should not be used for most identifier uses
toAstIdentifier :: IdentifierInfo -> IBIdentifier
toAstIdentifier (IdentifierInfo {boundIdentifier, declarationRange}) = Identifier declarationRange boundIdentifier

typeExpressionBinder :: PTypeExpression -> IdentifierBinder IBTypeExpression
typeExpressionBinder (IntTypeExpression range) = return $ IntTypeExpression range
typeExpressionBinder (FloatTypeExpression range) = return $ FloatTypeExpression range
typeExpressionBinder (CharTypeExpression range) = return $ CharTypeExpression range
typeExpressionBinder (StringTypeExpression range) = return $ StringTypeExpression range
typeExpressionBinder (BoolTypeExpression range) = return $ BoolTypeExpression range
typeExpressionBinder (NilTypeExpression range) = return $ NilTypeExpression range
typeExpressionBinder (FunctionTypeExpression range parameterTypes returnType) = do
  boundParameterTypes <- mapM typeExpressionBinder parameterTypes
  boundReturnType <- typeExpressionBinder returnType
  return $ FunctionTypeExpression range boundParameterTypes boundReturnType