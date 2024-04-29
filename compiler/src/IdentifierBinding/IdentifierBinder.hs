module IdentifierBinding.IdentifierBinder
  ( runIdentifierBinding,
  )
where

import Core.Errors
import Core.SyntaxTree
import Data.Sequence (Seq)
import IdentifierBinding.IdentifierBinding
import IdentifierBinding.SyntaxTree
import Parsing.SyntaxTree

runIdentifierBinding :: PModule -> WithErrors IBModule
runIdentifierBinding m = snd $ runBinder (moduleBinder m) initialBindingState

moduleBinder :: PModule -> IdentifierBinder IBModule
moduleBinder (Module _ (MainFunctionDefinition _ statements)) = withNewExpressionScope $ do
  boundStatements <- bindScope statements
  boundFunctionDefinitions <- getBoundFunctions
  return $ Module () $ IBModuleContent (MainFunctionDefinition () boundStatements) boundFunctionDefinitions

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
  (VariableDeclarationStatement range (Identifier _ identifierName) _) -> do
    _ <- addVariable range identifierName
    return $ BindingReadyStatement statement
  _ -> return $ BindingReadyStatement statement

statementBinder :: BindingReadyStatement -> IdentifierBinder IBStatement
statementBinder (BindingReadyStatement (VariableDeclarationStatement declarationRange (Identifier identifierRange identifier) expression)) =
  do
    IdentifierInfo {boundIdentifier} <- setVariableUsability identifier InDeclaration
    boundExpression <- expressionBinder expression
    return $ VariableDeclarationStatement declarationRange (Identifier identifierRange boundIdentifier) boundExpression
    `andFinally` setVariableUsability identifier Usable
statementBinder (BindingReadyStatement (VariableMutationStatement statementRange (Identifier identifierRange identifier) expression)) = do
  IdentifierInfo {boundIdentifier} <- getIdentifierBinding identifierRange identifier
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

expressionBinder :: PExpression -> IdentifierBinder IBExpression
expressionBinder (VariableExpression expressionRange (Identifier identifierRange identifier)) = do
  IdentifierInfo {boundIdentifier} <- getIdentifierBinding expressionRange identifier
  return $ VariableExpression expressionRange (Identifier identifierRange boundIdentifier)
expressionBinder (ScopeExpression d statements) = withNewExpressionScope $ do
  boundStatements <- bindScope statements
  return $ ScopeExpression d boundStatements
expressionBinder (FunctionExpression d (PFunctionExpressionContent parameters body)) = withNewFunctionScope $ do
  boundParameters <- traverse' bindParameter parameters
  boundBody <- expressionBinder body
  capturedVariables <- getCapturedIdentifiers
  let functionDefinition = FunctionDefinition d boundParameters (toAstIdentifier . insideIdentifier <$> capturedVariables) boundBody
  functionIndex <- addBoundFunctionDefinition functionDefinition
  return $ FunctionExpression d $ IBFunctionExpressionContent functionIndex (toAstIdentifier . outsideIdentifier <$> capturedVariables)
-- Standard cases
expressionBinder (IntLiteralExpression d value) = return $ IntLiteralExpression d value
expressionBinder (DoubleLiteralExpression d value) = return $ DoubleLiteralExpression d value
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

bindParameter :: PIdentifier -> IdentifierBinder IBIdentifier
bindParameter (Identifier range identifier) = do
  IdentifierInfo {boundIdentifier} <- addParameter range identifier
  return $ Identifier range boundIdentifier

-- Note that this function uses the identifier declaration range as its range, so this should not be used for most identifier uses
toAstIdentifier :: IdentifierInfo -> IBIdentifier
toAstIdentifier (IdentifierInfo {boundIdentifier, declarationRange}) = Identifier declarationRange boundIdentifier