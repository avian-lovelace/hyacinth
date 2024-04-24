module VariableBinding.VariableBinder (runVariableBinding) where

import Core.Errors
import Core.SyntaxTree
import Parsing.SyntaxTree
import VariableBinding.SyntaxTree
import VariableBinding.VariableBinding

runVariableBinding :: PFileScope -> WithErrors VBFileScope
runVariableBinding fileScope = snd $ runBinder (fileBinder fileScope) initialBindingState

fileBinder :: PFileScope -> VariableBinder VBFileScope
fileBinder (FileScope _ statements) = withNewScope $ do
  boundStatements <- traverse' statementBinder statements
  return $ FileScope () boundStatements

statementBinder :: PStatement -> VariableBinder VBStatement
statementBinder (VariableDeclarationStatement declarationRange (VariableName variableNameRange identifier) expression) =
  do
    assertVariableIsNotBeingDeclared declarationRange identifier
    boundIdentifier <- addVariable identifier declarationRange
    setVariableIsBeingDeclared identifier
    boundExpression <- expressionBinder expression
    return $ VariableDeclarationStatement declarationRange (VariableName variableNameRange boundIdentifier) boundExpression
    `andFinally` setVariableIsDoneBeingDeclared identifier
statementBinder (VariableMutationStatement statementRange (VariableName variableNameRange identifier) expression) = do
  assertVariableIsNotBeingDeclared statementRange identifier
  let variableNotDefinedError = VariableNotDefinedBeforeMutationError identifier statementRange
  VariableInfo {boundIdentifier} <- assertHasValue variableNotDefinedError $ getVariableInfo identifier
  boundExpression <- expressionBinder expression
  return $ VariableMutationStatement statementRange (VariableName variableNameRange boundIdentifier) boundExpression
statementBinder (PrintStatement range expression) = do
  boundExpression <- expressionBinder expression
  return $ PrintStatement range boundExpression
statementBinder (ExpressionStatement range expression) = do
  boundExpression <- expressionBinder expression
  return $ ExpressionStatement range boundExpression

expressionBinder :: PExpression -> VariableBinder VBExpression
expressionBinder (VariableExpression expressionRange (VariableName variableNameRange identifier)) = do
  assertVariableIsNotBeingDeclared expressionRange identifier
  let variableNotDefinedError = VariableNotDefinedBeforeUsageError identifier expressionRange
  VariableInfo {boundIdentifier} <- assertHasValue variableNotDefinedError $ getVariableInfo identifier
  return $ VariableExpression expressionRange (VariableName variableNameRange boundIdentifier)
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
expressionBinder (ScopeExpression d statements) = withNewScope $ do
  boundStatements <- traverse' statementBinder statements
  return $ ScopeExpression d boundStatements
