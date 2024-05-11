module FunctionLifting.FunctionLifter (runFunctionLifting) where

import Control.Monad (foldM, unless)
import Core.ErrorState
import Core.Errors
import Core.Graph
import Core.SyntaxTree
import Data.Foldable (toList)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import FunctionLifting.FunctionLifting
import FunctionLifting.SyntaxTree
import IdentifierBinding.SyntaxTree
import TypeChecking.SyntaxTree

runFunctionLifting :: Int -> Int -> TCModule -> WithErrors FLModule
runFunctionLifting boundValueIdentifierCounter boundFunctionIdentifierCounter tcModule = liftingResult
  where
    initialState = initialFunctionLiftingState boundValueIdentifierCounter boundFunctionIdentifierCounter
    (_, liftingResult) = runErrorState (moduleLifter tcModule) initialState

moduleLifter :: TCModule -> FunctionLifter FLModule
moduleLifter (Module () (MainFunction () mainFunctionScope)) = do
  liftedMainFunctionScope <- scopeLifter mainFunctionScope
  subFunctions <- getLiftedFunctions
  return $ Module () (MainFunction () liftedMainFunctionScope, subFunctions)

scopeLifter :: TCScope -> FunctionLifter FLScope
scopeLifter (Scope _ nonPositionalStatements statements) = do
  functionGraph <- makeScopeFunctionGraph nonPositionalStatements
  mapM_ (initializeFunction functionGraph) nonPositionalStatements
  mapM_ nonPositionalStatementLifter nonPositionalStatements
  liftedStatements <- mapM statementLifter statements
  return $ Scope () Seq.Empty liftedStatements
  where
    getCapturedIdentifiers :: Graph BoundFunctionIdentifier (Set BoundValueIdentifier) -> BoundFunctionIdentifier -> Set BoundValueIdentifier
    getCapturedIdentifiers graph functionName = foldDepthFirst Set.empty Set.union functionName graph
    initializeFunction :: Graph BoundFunctionIdentifier (Set BoundValueIdentifier) -> TCNonPositionalStatement -> FunctionLifter ()
    initializeFunction graph (FunctionStatement _ functionName _) = do
      let capturedIdentifiers = getCapturedIdentifiers graph functionName
      addFunctionCapturedIdentifiers functionName capturedIdentifiers

makeScopeFunctionGraph :: Seq TCNonPositionalStatement -> FunctionLifter (Graph BoundFunctionIdentifier (Set BoundValueIdentifier))
makeScopeFunctionGraph nonPositionalStatements = do
  graphNodes <- mapM toNamedGraphNode nonPositionalStatements
  return $ Graph $ Map.fromList . toList $ graphNodes
  where
    toNamedGraphNode :: TCNonPositionalStatement -> FunctionLifter (BoundFunctionIdentifier, GraphNode BoundFunctionIdentifier (Set BoundValueIdentifier))
    toNamedGraphNode (FunctionStatement _ functionName functionDefinition) = do
      graphNode <- toGraphNode functionDefinition
      return (functionName, graphNode)
    toGraphNode :: TCFunctionDefinition -> FunctionLifter (GraphNode BoundFunctionIdentifier (Set BoundValueIdentifier))
    toGraphNode (FunctionDefinition TCFunctionDefinitionData {tcFunctionDefinitionCapturedIdentifiers} _ _) = do
      (capturedValueIdentifiers, capturedFunctionIdentifiers) <- consolidateCapturedIdentifiers tcFunctionDefinitionCapturedIdentifiers
      return $ GraphNode capturedValueIdentifiers capturedFunctionIdentifiers

consolidateCapturedIdentifiers :: Set TCIdentifier -> FunctionLifter (Set BoundValueIdentifier, Set BoundFunctionIdentifier)
consolidateCapturedIdentifiers = foldM combine (Set.empty, Set.empty)
  where
    combine :: (Set BoundValueIdentifier, Set BoundFunctionIdentifier) -> TCIdentifier -> FunctionLifter (Set BoundValueIdentifier, Set BoundFunctionIdentifier)
    combine (capturedValueIdentifiers, capturedFunctionIdentifiers) (Left valueIdentifier) =
      return (Set.insert valueIdentifier capturedValueIdentifiers, capturedFunctionIdentifiers)
    combine (capturedValueIdentifiers, capturedFunctionIdentifiers) (Right functionIdentifier) = do
      maybeFunctionCapturedValueIdentifiers <- getFunctionCapturedIdentifiers functionIdentifier
      case maybeFunctionCapturedValueIdentifiers of
        Just functionCapturedValueIdentifiers -> return (Set.union capturedValueIdentifiers functionCapturedValueIdentifiers, capturedFunctionIdentifiers)
        Nothing -> return (capturedValueIdentifiers, Set.insert functionIdentifier capturedFunctionIdentifiers)

nonPositionalStatementLifter :: TCNonPositionalStatement -> FunctionLifter ()
nonPositionalStatementLifter (FunctionStatement statementRange functionName functionDefinition) = do
  maybeCapturedIdentifiers <- getFunctionCapturedIdentifiers functionName
  capturedIdentifiers <- case maybeCapturedIdentifiers of
    Just capturedIdentifiers -> return capturedIdentifiers
    Nothing -> throwError $ ShouldNotGetHereError "Captured identifiers were not recorded before calling nonPositionalStatementLifter"
  (liftedFunctionDefinition, capturedIdentifierInnerValues) <- withCapturedIdentifiers capturedIdentifiers (functionDefinitionLifter functionDefinition)
  let (BoundFunctionIdentifier functionIndex _) = functionName
  let subFunction = SubFunction statementRange capturedIdentifierInnerValues liftedFunctionDefinition
  addLiftedFunction functionIndex subFunction

statementLifter :: TCStatement -> FunctionLifter FLStatement
statementLifter (VariableDeclarationStatement TCStatementData {statementRange} mutability (WithTypeAnnotation variableName ()) variableValue) = do
  liftedVariableValue <- expressionLifter variableValue
  setIdentifierIsUsable variableName
  return $ VariableDeclarationStatement statementRange mutability (WithTypeAnnotation variableName ()) liftedVariableValue
statementLifter (VariableMutationStatement TCStatementData {statementRange} variableName variableValue) = do
  let BoundValueIdentifier _ variableTextName = variableName
  let mutatedCapturedIdentifierError = MutatedCapturedIdentifierError variableTextName statementRange
  assertIdentifierIsNotCaptured mutatedCapturedIdentifierError variableName
  liftedVariableValue <- expressionLifter variableValue
  return $ VariableMutationStatement statementRange variableName liftedVariableValue
statementLifter (PrintStatement TCStatementData {statementRange} expression) = do
  liftedExpression <- expressionLifter expression
  return $ PrintStatement statementRange liftedExpression
statementLifter (ExpressionStatement TCStatementData {statementRange} expression) = do
  liftedExpression <- expressionLifter expression
  return $ ExpressionStatement statementRange liftedExpression
statementLifter (WhileLoopStatement TCStatementData {statementRange} condition body) = do
  liftedCondition <- expressionLifter condition
  liftedBody <- expressionLifter body
  return $ WhileLoopStatement statementRange liftedCondition liftedBody
statementLifter (ReturnStatement TCStatementData {statementRange} (Just expression)) = do
  liftedExpression <- expressionLifter expression
  return $ ReturnStatement statementRange (Just liftedExpression)
statementLifter (ReturnStatement TCStatementData {statementRange} Nothing) =
  return $ ReturnStatement statementRange Nothing

functionDefinitionLifter :: TCFunctionDefinition -> FunctionLifter FLFunctionDefinition
functionDefinitionLifter (FunctionDefinition TCFunctionDefinitionData {tcFunctionDefinitionRange} parameters (WithTypeAnnotation body ())) = do
  liftedParameters <- mapM liftParameter parameters
  liftedBody <- expressionLifter body
  return $ FunctionDefinition tcFunctionDefinitionRange liftedParameters (WithTypeAnnotation liftedBody ())
  where
    liftParameter :: TCWithTypeAnnotation BoundValueIdentifier -> FunctionLifter (FLWithTypeAnnotation BoundValueIdentifier)
    liftParameter (WithTypeAnnotation parameter ()) = do
      setIdentifierIsUsable parameter
      return $ WithTypeAnnotation parameter ()

expressionLifter :: TCExpression -> FunctionLifter FLExpression
expressionLifter (IdentifierExpression TCExpresionData {expressionRange} identifier) = case identifier of
  Left valueIdentifier -> do
    let identifierUnusableError = ShouldNotGetHereError "Found unusable value identifier in expressionLifter. This should have already been caught in identifier binding."
    liftedIdentifier <- getIdentifierInContext identifierUnusableError valueIdentifier
    return $ IdentifierExpression expressionRange liftedIdentifier
  Right functionIdentifier -> do
    capturedIdentifiers <- getFunctionCapturedIdentifiersInContext expressionRange functionIdentifier
    let (BoundFunctionIdentifier functionIndex _) = functionIdentifier
    return $ FunctionExpression expressionRange $ FunctionReference functionIndex capturedIdentifiers
expressionLifter (FunctionExpression TCExpresionData {expressionRange} functionDefinition) = do
  let combinedCapturedIdentifiers = tcFunctionDefinitionCapturedIdentifiers . getFunctionDefinitionData $ functionDefinition
  (capturedValueIdentifiers, capturedFunctionIdentifiers) <- consolidateCapturedIdentifiers combinedCapturedIdentifiers
  unless (Set.size capturedFunctionIdentifiers == 0) $
    throwError (ShouldNotGetHereError "Some captured functions were not initialized in expressionLifter")
  let identifierUnusableError = ShouldNotGetHereError "Found unusable captured identifier in expressionLifter. This should have already been caught in identifier binding."
  capturedFunctionIdentifierContextValues <- mapM (getIdentifierInContext identifierUnusableError) $ Set.toAscList capturedValueIdentifiers
  (liftedFunctionDefinition, capturedIdentifierInnerValues) <- withCapturedIdentifiers capturedValueIdentifiers (functionDefinitionLifter functionDefinition)
  functionIndex <- getNewFunctionIndex
  let subFunction = SubFunction expressionRange capturedIdentifierInnerValues liftedFunctionDefinition
  addLiftedFunction functionIndex subFunction
  return $ FunctionExpression expressionRange (FunctionReference functionIndex (Seq.fromList capturedFunctionIdentifierContextValues))
-- Standard cases
expressionLifter (IntLiteralExpression TCExpresionData {expressionRange} value) = return $ IntLiteralExpression expressionRange value
expressionLifter (FloatLiteralExpression TCExpresionData {expressionRange} value) = return $ FloatLiteralExpression expressionRange value
expressionLifter (CharLiteralExpression TCExpresionData {expressionRange} value) = return $ CharLiteralExpression expressionRange value
expressionLifter (StringLiteralExpression TCExpresionData {expressionRange} value) = return $ StringLiteralExpression expressionRange value
expressionLifter (BoolLiteralExpression TCExpresionData {expressionRange} value) = return $ BoolLiteralExpression expressionRange value
expressionLifter (NilExpression TCExpresionData {expressionRange}) = return $ NilExpression expressionRange
expressionLifter (NegateExpression TCExpresionData {expressionRange} inner) = do
  liftedInner <- expressionLifter inner
  return $ NegateExpression expressionRange liftedInner
expressionLifter (AddExpression TCExpresionData {expressionRange} left right) = do
  liftedLeft <- expressionLifter left
  liftedRight <- expressionLifter right
  return $ AddExpression expressionRange liftedLeft liftedRight
expressionLifter (SubtractExpression TCExpresionData {expressionRange} left right) = do
  liftedLeft <- expressionLifter left
  liftedRight <- expressionLifter right
  return $ SubtractExpression expressionRange liftedLeft liftedRight
expressionLifter (MultiplyExpression TCExpresionData {expressionRange} left right) = do
  liftedLeft <- expressionLifter left
  liftedRight <- expressionLifter right
  return $ MultiplyExpression expressionRange liftedLeft liftedRight
expressionLifter (DivideExpression TCExpresionData {expressionRange} left right) = do
  liftedLeft <- expressionLifter left
  liftedRight <- expressionLifter right
  return $ DivideExpression expressionRange liftedLeft liftedRight
expressionLifter (ModuloExpression TCExpresionData {expressionRange} left right) = do
  liftedLeft <- expressionLifter left
  liftedRight <- expressionLifter right
  return $ ModuloExpression expressionRange liftedLeft liftedRight
expressionLifter (NotExpression TCExpresionData {expressionRange} inner) = do
  liftedInner <- expressionLifter inner
  return $ NotExpression expressionRange liftedInner
expressionLifter (AndExpression TCExpresionData {expressionRange} left right) = do
  liftedLeft <- expressionLifter left
  liftedRight <- expressionLifter right
  return $ AndExpression expressionRange liftedLeft liftedRight
expressionLifter (OrExpression TCExpresionData {expressionRange} left right) = do
  liftedLeft <- expressionLifter left
  liftedRight <- expressionLifter right
  return $ OrExpression expressionRange liftedLeft liftedRight
expressionLifter (EqualExpression TCExpresionData {expressionRange} left right) = do
  liftedLeft <- expressionLifter left
  liftedRight <- expressionLifter right
  return $ EqualExpression expressionRange liftedLeft liftedRight
expressionLifter (NotEqualExpression TCExpresionData {expressionRange} left right) = do
  liftedLeft <- expressionLifter left
  liftedRight <- expressionLifter right
  return $ NotEqualExpression expressionRange liftedLeft liftedRight
expressionLifter (GreaterExpression TCExpresionData {expressionRange} left right) = do
  liftedLeft <- expressionLifter left
  liftedRight <- expressionLifter right
  return $ GreaterExpression expressionRange liftedLeft liftedRight
expressionLifter (LessExpression TCExpresionData {expressionRange} left right) = do
  liftedLeft <- expressionLifter left
  liftedRight <- expressionLifter right
  return $ LessExpression expressionRange liftedLeft liftedRight
expressionLifter (GreaterEqualExpression TCExpresionData {expressionRange} left right) = do
  liftedLeft <- expressionLifter left
  liftedRight <- expressionLifter right
  return $ GreaterEqualExpression expressionRange liftedLeft liftedRight
expressionLifter (LessEqualExpression TCExpresionData {expressionRange} left right) = do
  liftedLeft <- expressionLifter left
  liftedRight <- expressionLifter right
  return $ LessEqualExpression expressionRange liftedLeft liftedRight
expressionLifter (IfThenElseExpression TCExpresionData {expressionRange} condition trueExpression maybeFalseExpression) = do
  liftedCondition <- expressionLifter condition
  liftedTrueExpression <- expressionLifter trueExpression
  liftedFalseExpression <- case maybeFalseExpression of
    Just falseExpression -> do
      boundFalseExpression <- expressionLifter falseExpression
      return $ Just boundFalseExpression
    Nothing -> return Nothing
  return $ IfThenElseExpression expressionRange liftedCondition liftedTrueExpression liftedFalseExpression
expressionLifter (ScopeExpression TCExpresionData {expressionRange} scope) = do
  liftedScope <- scopeLifter scope
  return $ ScopeExpression expressionRange liftedScope
expressionLifter (FunctionCallExpression TCExpresionData {expressionRange} function arguments) = do
  liftedFunction <- expressionLifter function
  liftedArguments <- traverse' expressionLifter arguments
  return $ FunctionCallExpression expressionRange liftedFunction liftedArguments