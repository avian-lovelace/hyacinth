module IntermediateCodeGeneration.IntermediateCodeGenerator (runFunctionLifting) where

import Control.Monad (foldM, unless)
import Core.ErrorState
import Core.Errors
import Core.Graph
import Core.SyntaxTree
import Core.Type
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import IdentifierBinding.SyntaxTree
import IntermediateCodeGeneration.IntermediateCode
import IntermediateCodeGeneration.IntermediateCodeGeneration
import Parsing.SyntaxTree
import TypeChecking.SyntaxTree

runFunctionLifting :: Int -> Int -> Map BoundRecordIdentifier (Seq UnboundIdentifier) -> TCModule -> WithErrors Mod
runFunctionLifting boundValueIdentifierCounter boundFunctionIdentifierCounter recordFieldOrders tcModule = liftingResult
  where
    initialState = initialFunctionLiftingState boundValueIdentifierCounter boundFunctionIdentifierCounter recordFieldOrders
    (_, liftingResult) = runErrorState (moduleLifter tcModule) initialState

moduleLifter :: TCModule -> FunctionLifter Mod
moduleLifter (Module () (MainFunction () mainFunctionScope)) = do
  liftedMainFunctionScope <- scopeLifter mainFunctionScope
  subFunctions <- getLiftedFunctions
  return $ Mod (MainFunc liftedMainFunctionScope) subFunctions

scopeLifter :: TCScope -> FunctionLifter (Seq Stmt)
scopeLifter (Scope _ nonPositionalStatements statements) = do
  functionGraph <- makeScopeFunctionGraph nonPositionalStatements
  mapM_ (initializeFunction functionGraph) nonPositionalStatements
  mapM_ nonPositionalStatementLifter nonPositionalStatements
  mapM statementLifter statements
  where
    getCapturedIdentifiers :: Graph BoundFunctionIdentifier (Set BoundValueIdentifier) -> BoundFunctionIdentifier -> Set BoundValueIdentifier
    getCapturedIdentifiers graph functionName = foldDepthFirst Set.empty Set.union functionName graph
    initializeFunction :: Graph BoundFunctionIdentifier (Set BoundValueIdentifier) -> TCNonPositionalStatement -> FunctionLifter ()
    initializeFunction graph (FunctionStatement _ functionName _) = do
      let capturedIdentifiers = getCapturedIdentifiers graph functionName
      addFunctionCapturedIdentifiers functionName capturedIdentifiers
    initializeFunction _ (RecordStatement {}) = throwError $ ShouldNotGetHereError "Got record statement in initializeFunction"

makeScopeFunctionGraph :: Seq TCNonPositionalStatement -> FunctionLifter (Graph BoundFunctionIdentifier (Set BoundValueIdentifier))
makeScopeFunctionGraph nonPositionalStatements = do
  graphNodes <- mapM toNamedGraphNode nonPositionalStatements
  return $ Graph $ Map.fromList . toList $ graphNodes
  where
    toNamedGraphNode :: TCNonPositionalStatement -> FunctionLifter (BoundFunctionIdentifier, GraphNode BoundFunctionIdentifier (Set BoundValueIdentifier))
    toNamedGraphNode (FunctionStatement _ functionName functionDefinition) = do
      graphNode <- toGraphNode functionDefinition
      return (functionName, graphNode)
    toNamedGraphNode (RecordStatement {}) = throwError $ ShouldNotGetHereError "Got record statement in toNamedGraphNode"
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
  (makeSubFunc, capturedIdentifierInnerValues) <- withCapturedIdentifiers capturedIdentifiers (functionDefinitionLifter functionDefinition)
  let (BoundFunctionIdentifier functionIndex _) = functionName
  let subFunction = makeSubFunc (getValueIdentifierIndex <$> capturedIdentifierInnerValues)
  addLiftedFunction functionIndex subFunction
nonPositionalStatementLifter (RecordStatement {}) = throwError $ ShouldNotGetHereError "Got record statement in nonPositionalStatementLifter"

statementLifter :: TCStatement -> FunctionLifter Stmt
statementLifter (VariableDeclarationStatement _ _ (WithTypeAnnotation variableName ()) variableValue) = do
  liftedVariableValue <- expressionLifter variableValue
  setIdentifierIsUsable variableName
  return $ VariableDeclarationStmt (getValueIdentifierIndex variableName) liftedVariableValue
statementLifter (VariableMutationStatement TCStatementData {statementRange} variableName variableValue) = do
  let BoundValueIdentifier _ variableTextName = variableName
  let mutatedCapturedIdentifierError = MutatedCapturedIdentifierError variableTextName statementRange
  assertIdentifierIsNotCaptured mutatedCapturedIdentifierError variableName
  liftedVariableValue <- expressionLifter variableValue
  return $ VariableMutationStmt (getValueIdentifierIndex variableName) liftedVariableValue
statementLifter (PrintStatement _ expression) = do
  liftedExpression <- expressionLifter expression
  return $ ExpressionStmt (BuiltInFunctionExpr PrintFn $ Seq.singleton liftedExpression)
statementLifter (ExpressionStatement _ expression) = do
  liftedExpression <- expressionLifter expression
  return $ ExpressionStmt liftedExpression
statementLifter (WhileLoopStatement _ condition body) = do
  liftedCondition <- expressionLifter condition
  liftedBody <- expressionLifter body
  return $ WhileLoopStmt liftedCondition liftedBody
statementLifter (ReturnStatement _ (Just expression)) = do
  liftedExpression <- expressionLifter expression
  return $ ReturnStmt liftedExpression
statementLifter (ReturnStatement _ Nothing) =
  return $ ReturnStmt (LiteralExpr NilLiteral)

functionDefinitionLifter :: TCFunctionDefinition -> FunctionLifter (Seq ValueIdentifierIndex -> SubFunc)
functionDefinitionLifter (FunctionDefinition TCFunctionDefinitionData {tcFunctionDefinitionRange} parameters (WithTypeAnnotation body ())) = do
  liftedParameters <- mapM liftParameter parameters
  liftedBody <- expressionLifter body
  return $ \capturedIdentifiers -> SubFunc (liftedParameters >< capturedIdentifiers) liftedBody
  where
    liftParameter :: TCWithTypeAnnotation BoundValueIdentifier -> FunctionLifter ValueIdentifierIndex
    liftParameter (WithTypeAnnotation parameter ()) = do
      setIdentifierIsUsable parameter
      return $ getValueIdentifierIndex parameter

expressionLifter :: TCExpression -> FunctionLifter Expr
expressionLifter (IdentifierExpression TCExpresionData {expressionRange} identifier) = case identifier of
  Left valueIdentifier -> do
    let identifierUnusableError = ShouldNotGetHereError "Found unusable value identifier in expressionLifter. This should have already been caught in identifier binding."
    liftedIdentifier <- getIdentifierInContext identifierUnusableError valueIdentifier
    return $ IdentifierExpr (getValueIdentifierIndex liftedIdentifier)
  Right functionIdentifier -> do
    capturedIdentifiers <- getFunctionCapturedIdentifiersInContext expressionRange functionIdentifier
    let (BoundFunctionIdentifier functionIndex _) = functionIdentifier
    return $ FunctionExpr functionIndex (getValueIdentifierIndex <$> capturedIdentifiers)
expressionLifter (FunctionExpression TCExpresionData {expressionRange} functionDefinition) = do
  let combinedCapturedIdentifiers = tcFunctionDefinitionCapturedIdentifiers . getFunctionDefinitionData $ functionDefinition
  (capturedValueIdentifiers, capturedFunctionIdentifiers) <- consolidateCapturedIdentifiers combinedCapturedIdentifiers
  unless (Set.size capturedFunctionIdentifiers == 0) $
    throwError (ShouldNotGetHereError "Some captured functions were not initialized in expressionLifter")
  let identifierUnusableError = ShouldNotGetHereError "Found unusable captured identifier in expressionLifter. This should have already been caught in identifier binding."
  capturedFunctionIdentifierContextValues <- mapM (getIdentifierInContext identifierUnusableError) $ Set.toAscList capturedValueIdentifiers
  (makeSubFunc, capturedIdentifierInnerValues) <- withCapturedIdentifiers capturedValueIdentifiers (functionDefinitionLifter functionDefinition)
  functionIndex <- getNewFunctionIndex
  let subFunction = makeSubFunc (getValueIdentifierIndex <$> capturedIdentifierInnerValues)
  addLiftedFunction functionIndex subFunction
  return $ FunctionExpr functionIndex (getValueIdentifierIndex <$> Seq.fromList capturedFunctionIdentifierContextValues)
expressionLifter (RecordExpression TCExpresionData {expressionRange} recordName fieldValueMap) = do
  recordFieldOrder <- getRecordFieldOrder recordName
  let recordFields = (\fieldName -> fromJust $ Map.lookup fieldName fieldValueMap) <$> recordFieldOrder
  liftedRecordFields <- mapM expressionLifter recordFields
  return $ RecordExpr (getRecordIdentifierIndex recordName) liftedRecordFields
expressionLifter (FieldAccessExpression TCExpresionData {expressionRange} inner fieldName) = do
  liftedInner <- expressionLifter inner
  let innerType = expressionType . getExpressionData $ inner
  recordName <- case innerType of
    RecordType recordName -> return recordName
    _ -> throwError $ ShouldNotGetHereError "Inner expression of FieldAccessExpression was not a record in expressionLifter"
  fieldIndex <- getRecordFieldIndex recordName fieldName
  return $ FieldExpr liftedInner fieldIndex
-- Standard cases
expressionLifter (IntLiteralExpression TCExpresionData {expressionRange} value) = return $ LiteralExpr (IntLiteral value)
expressionLifter (FloatLiteralExpression TCExpresionData {expressionRange} value) = return $ LiteralExpr (FloatLiteral value)
expressionLifter (CharLiteralExpression TCExpresionData {expressionRange} value) = return $ LiteralExpr (CharLiteral value)
expressionLifter (StringLiteralExpression TCExpresionData {expressionRange} value) = return $ LiteralExpr (StringLiteral value)
expressionLifter (BoolLiteralExpression TCExpresionData {expressionRange} value) = return $ LiteralExpr (BoolLiteral value)
expressionLifter (NilExpression TCExpresionData {expressionRange}) = return $ LiteralExpr NilLiteral
expressionLifter (NegateExpression TCExpresionData {expressionRange} inner) = do
  liftedInner <- expressionLifter inner
  return $ BuiltInFunctionExpr NegateFn (Seq.singleton liftedInner)
expressionLifter (AddExpression TCExpresionData {expressionRange} left right) = do
  liftedLeft <- expressionLifter left
  liftedRight <- expressionLifter right
  return $ BuiltInFunctionExpr AddFn (Seq.fromList [liftedLeft, liftedRight])
expressionLifter (SubtractExpression TCExpresionData {expressionRange} left right) = do
  liftedLeft <- expressionLifter left
  liftedRight <- expressionLifter right
  return $ BuiltInFunctionExpr SubtractFn (Seq.fromList [liftedLeft, liftedRight])
expressionLifter (MultiplyExpression TCExpresionData {expressionRange} left right) = do
  liftedLeft <- expressionLifter left
  liftedRight <- expressionLifter right
  return $ BuiltInFunctionExpr MultiplyFn (Seq.fromList [liftedLeft, liftedRight])
expressionLifter (DivideExpression TCExpresionData {expressionRange} left right) = do
  liftedLeft <- expressionLifter left
  liftedRight <- expressionLifter right
  return $ BuiltInFunctionExpr DivideFn (Seq.fromList [liftedLeft, liftedRight])
expressionLifter (ModuloExpression TCExpresionData {expressionRange} left right) = do
  liftedLeft <- expressionLifter left
  liftedRight <- expressionLifter right
  return $ BuiltInFunctionExpr ModuloFn (Seq.fromList [liftedLeft, liftedRight])
expressionLifter (NotExpression TCExpresionData {expressionRange} inner) = do
  liftedInner <- expressionLifter inner
  return $ BuiltInFunctionExpr NotFn (Seq.singleton liftedInner)
expressionLifter (AndExpression TCExpresionData {expressionRange} left right) = do
  liftedLeft <- expressionLifter left
  liftedRight <- expressionLifter right
  return $ IfThenElseExpr liftedLeft liftedRight (LiteralExpr (BoolLiteral False))
expressionLifter (OrExpression TCExpresionData {expressionRange} left right) = do
  liftedLeft <- expressionLifter left
  liftedRight <- expressionLifter right
  return $ IfThenElseExpr liftedLeft (LiteralExpr (BoolLiteral True)) liftedRight
expressionLifter (EqualExpression TCExpresionData {expressionRange} left right) = do
  liftedLeft <- expressionLifter left
  liftedRight <- expressionLifter right
  return $ BuiltInFunctionExpr EqualFn (Seq.fromList [liftedLeft, liftedRight])
expressionLifter (NotEqualExpression TCExpresionData {expressionRange} left right) = do
  liftedLeft <- expressionLifter left
  liftedRight <- expressionLifter right
  return $ BuiltInFunctionExpr NotEqualFn (Seq.fromList [liftedLeft, liftedRight])
expressionLifter (GreaterExpression TCExpresionData {expressionRange} left right) = do
  liftedLeft <- expressionLifter left
  liftedRight <- expressionLifter right
  return $ BuiltInFunctionExpr GreaterFn (Seq.fromList [liftedLeft, liftedRight])
expressionLifter (LessExpression TCExpresionData {expressionRange} left right) = do
  liftedLeft <- expressionLifter left
  liftedRight <- expressionLifter right
  return $ BuiltInFunctionExpr LessFn (Seq.fromList [liftedLeft, liftedRight])
expressionLifter (GreaterEqualExpression TCExpresionData {expressionRange} left right) = do
  liftedLeft <- expressionLifter left
  liftedRight <- expressionLifter right
  return $ BuiltInFunctionExpr GreaterEqualFn (Seq.fromList [liftedLeft, liftedRight])
expressionLifter (LessEqualExpression TCExpresionData {expressionRange} left right) = do
  liftedLeft <- expressionLifter left
  liftedRight <- expressionLifter right
  return $ BuiltInFunctionExpr LessEqualFn (Seq.fromList [liftedLeft, liftedRight])
expressionLifter (IfThenElseExpression TCExpresionData {expressionRange} condition trueExpression maybeFalseExpression) = do
  liftedCondition <- expressionLifter condition
  liftedTrueExpression <- expressionLifter trueExpression
  liftedFalseExpression <- case maybeFalseExpression of
    Just falseExpression -> expressionLifter falseExpression
    Nothing -> return (LiteralExpr NilLiteral)
  return $ IfThenElseExpr liftedCondition liftedTrueExpression liftedFalseExpression
expressionLifter (ScopeExpression TCExpresionData {expressionRange} scope) = do
  liftedScope <- scopeLifter scope
  return $ ScopeExpr liftedScope
expressionLifter (FunctionCallExpression TCExpresionData {expressionRange} function arguments) = do
  liftedFunction <- expressionLifter function
  liftedArguments <- traverse' expressionLifter arguments
  return $ CallExpr liftedFunction liftedArguments