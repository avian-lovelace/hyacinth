module IntermediateCodeGeneration.IntermediateCodeGenerator (runIntermediateCodeGeneration) where

import Control.Monad (foldM, forM, unless)
import Core.ErrorState
import Core.Errors
import Core.Graph
import Core.SyntaxTree
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
import TypeChecking.Type

runIntermediateCodeGeneration :: Int -> Int -> Map BoundRecordIdentifier (Seq UnboundIdentifier) -> TCModule -> WithErrors Mod
runIntermediateCodeGeneration boundValueIdentifierCounter boundFunctionIdentifierCounter recordFieldOrders tcModule = intermediateCode
  where
    initialState = initialIntermediateCodeGenerationState boundValueIdentifierCounter boundFunctionIdentifierCounter recordFieldOrders
    (_, intermediateCode) = runErrorState (moduleGenerator tcModule) initialState

moduleGenerator :: TCModule -> IntermediateCodeGenerator Mod
moduleGenerator (Module () (MainFunction () mainFunctionScope)) = do
  encodedMainFunctionScope <- scopeGenerator mainFunctionScope
  subFunctions <- getSubFunctions
  return $ Mod (MainFunc encodedMainFunctionScope) subFunctions

scopeGenerator :: TCScope -> IntermediateCodeGenerator (Seq Stmt)
scopeGenerator (Scope _ nonPositionalStatements statements) = do
  functionGraph <- makeScopeFunctionGraph nonPositionalStatements
  mapM_ (initializeFunction functionGraph) nonPositionalStatements
  mapM_ nonPositionalStatementGenerator nonPositionalStatements
  mapM statementGenerator statements
  where
    getCapturedIdentifiers :: Graph BoundFunctionIdentifier (Set BoundValueIdentifier) -> BoundFunctionIdentifier -> Set BoundValueIdentifier
    getCapturedIdentifiers graph functionName = foldDepthFirst Set.empty Set.union functionName graph
    initializeFunction :: Graph BoundFunctionIdentifier (Set BoundValueIdentifier) -> TCNonPositionalStatement -> IntermediateCodeGenerator ()
    initializeFunction graph (FunctionStatement _ functionName _) = do
      let capturedIdentifiers = getCapturedIdentifiers graph functionName
      addFunctionCapturedIdentifiers functionName capturedIdentifiers
    initializeFunction _ (RecordStatement {}) = throwError $ ShouldNotGetHereError "Got record statement in initializeFunction"

makeScopeFunctionGraph :: Seq TCNonPositionalStatement -> IntermediateCodeGenerator (Graph BoundFunctionIdentifier (Set BoundValueIdentifier))
makeScopeFunctionGraph nonPositionalStatements = do
  graphNodes <- mapM toNamedGraphNode nonPositionalStatements
  return $ Graph $ Map.fromList . toList $ graphNodes
  where
    toNamedGraphNode :: TCNonPositionalStatement -> IntermediateCodeGenerator (BoundFunctionIdentifier, GraphNode BoundFunctionIdentifier (Set BoundValueIdentifier))
    toNamedGraphNode (FunctionStatement _ functionName functionDefinition) = do
      graphNode <- toGraphNode functionDefinition
      return (functionName, graphNode)
    toNamedGraphNode (RecordStatement {}) = throwError $ ShouldNotGetHereError "Got record statement in toNamedGraphNode"
    toGraphNode :: TCFunctionDefinition -> IntermediateCodeGenerator (GraphNode BoundFunctionIdentifier (Set BoundValueIdentifier))
    toGraphNode (FunctionDefinition TCFunctionDefinitionData {tcFunctionDefinitionCapturedIdentifiers} _ _) = do
      (capturedValueIdentifiers, capturedFunctionIdentifiers) <- consolidateCapturedIdentifiers tcFunctionDefinitionCapturedIdentifiers
      return $ GraphNode capturedValueIdentifiers capturedFunctionIdentifiers

consolidateCapturedIdentifiers :: Set TCIdentifier -> IntermediateCodeGenerator (Set BoundValueIdentifier, Set BoundFunctionIdentifier)
consolidateCapturedIdentifiers = foldM combine (Set.empty, Set.empty)
  where
    combine :: (Set BoundValueIdentifier, Set BoundFunctionIdentifier) -> TCIdentifier -> IntermediateCodeGenerator (Set BoundValueIdentifier, Set BoundFunctionIdentifier)
    combine (capturedValueIdentifiers, capturedFunctionIdentifiers) (Left valueIdentifier) =
      return (Set.insert valueIdentifier capturedValueIdentifiers, capturedFunctionIdentifiers)
    combine (capturedValueIdentifiers, capturedFunctionIdentifiers) (Right functionIdentifier) = do
      maybeFunctionCapturedValueIdentifiers <- getFunctionCapturedIdentifiers functionIdentifier
      case maybeFunctionCapturedValueIdentifiers of
        Just functionCapturedValueIdentifiers -> return (Set.union capturedValueIdentifiers functionCapturedValueIdentifiers, capturedFunctionIdentifiers)
        Nothing -> return (capturedValueIdentifiers, Set.insert functionIdentifier capturedFunctionIdentifiers)

nonPositionalStatementGenerator :: TCNonPositionalStatement -> IntermediateCodeGenerator ()
nonPositionalStatementGenerator (FunctionStatement _ functionName functionDefinition) = do
  maybeCapturedIdentifiers <- getFunctionCapturedIdentifiers functionName
  capturedIdentifiers <- case maybeCapturedIdentifiers of
    Just capturedIdentifiers -> return capturedIdentifiers
    Nothing -> throwError $ ShouldNotGetHereError "Captured identifiers were not recorded before calling nonPositionalStatementGenerator"
  (makeSubFunc, capturedIdentifierInnerValues) <- withCapturedIdentifiers capturedIdentifiers (functionDefinitionGenerator functionDefinition)
  let (BoundFunctionIdentifier functionIndex _) = functionName
  let subFunction = makeSubFunc (getValueIdentifierIndex <$> capturedIdentifierInnerValues)
  addSubFunction functionIndex subFunction
nonPositionalStatementGenerator (RecordStatement {}) = throwError $ ShouldNotGetHereError "Got record statement in nonPositionalStatementGenerator"

statementGenerator :: TCStatement -> IntermediateCodeGenerator Stmt
statementGenerator (VariableDeclarationStatement _ _ (WithTypeAnnotation variableName ()) variableValue) = do
  encodedVariableValue <- expressionGenerator variableValue
  setIdentifierIsUsable variableName
  return $ VariableDeclarationStmt (getValueIdentifierIndex variableName) encodedVariableValue
statementGenerator (VariableMutationStatement TCStatementData {statementRange} variableName variableValue) = do
  let BoundValueIdentifier _ variableTextName = variableName
  let mutatedCapturedIdentifierError = MutatedCapturedIdentifierError variableTextName statementRange
  assertIdentifierIsNotCaptured mutatedCapturedIdentifierError variableName
  encodedVariableValue <- expressionGenerator variableValue
  return $ VariableMutationStmt (getValueIdentifierIndex variableName) encodedVariableValue
statementGenerator (FieldMutationStatement _ record field value) = do
  encodedRecord <- expressionGenerator record
  encodedValue <- expressionGenerator value
  let recordType = expressionType . getExpressionData $ record
  recordNames <- case recordType of
    RecordUnionType _ recordNames -> return recordNames
    _ -> throwError $ ShouldNotGetHereError "Inner expression of FieldAccessExpression was not a record in statementGenerator"
  recordIndexPairs <- forM (Map.keys recordNames) $ \recordName -> do
    fieldIndex <- getRecordFieldIndex recordName field
    return (recordName, fieldIndex)
  let firstFieldIndex = snd . head $ recordIndexPairs
  if all (\(_, index) -> index == firstFieldIndex) recordIndexPairs
    then return $ FieldMutationStmt encodedRecord firstFieldIndex encodedValue
    else do
      encodedCases <- forM (Seq.fromList recordIndexPairs) $ \(recordName, fieldIndex) -> do
        caseParameter <- getNewValueIdentifierIndex
        let fieldMutationStatement = FieldMutationStmt (IdentifierExpr caseParameter) fieldIndex encodedValue
        return (getRecordIdentifierIndex recordName, caseParameter, ScopeExpr $ Seq.singleton fieldMutationStatement)
      return $ ExpressionStmt $ CaseExpr encodedRecord encodedCases
statementGenerator (PrintStatement _ expression) = do
  encodedExpression <- expressionGenerator expression
  return $ ExpressionStmt (BuiltInFunctionExpr PrintFn $ Seq.singleton encodedExpression)
statementGenerator (ExpressionStatement _ expression) = do
  encodedExpression <- expressionGenerator expression
  return $ ExpressionStmt encodedExpression
statementGenerator (WhileLoopStatement _ condition body) = do
  encodedCondition <- expressionGenerator condition
  encodedBody <- expressionGenerator body
  return $ WhileLoopStmt encodedCondition encodedBody
statementGenerator (ReturnStatement _ (Just expression)) = do
  encodedExpression <- expressionGenerator expression
  return $ ReturnStmt encodedExpression
statementGenerator (ReturnStatement _ Nothing) =
  return $ ReturnStmt (LiteralExpr NilLiteral)

functionDefinitionGenerator :: TCFunctionDefinition -> IntermediateCodeGenerator (Seq ValueIdentifierIndex -> SubFunc)
functionDefinitionGenerator (FunctionDefinition _ parameters (WithTypeAnnotation body ())) = do
  encodedParameters <- mapM parameterGenerator parameters
  encodedBody <- expressionGenerator body
  return $ \capturedIdentifiers -> SubFunc (encodedParameters >< capturedIdentifiers) encodedBody
  where
    parameterGenerator :: TCWithTypeAnnotation BoundValueIdentifier -> IntermediateCodeGenerator ValueIdentifierIndex
    parameterGenerator (WithTypeAnnotation parameter ()) = do
      setIdentifierIsUsable parameter
      return $ getValueIdentifierIndex parameter

expressionGenerator :: TCExpression -> IntermediateCodeGenerator Expr
expressionGenerator (IdentifierExpression TCExpresionData {expressionRange} identifier) = case identifier of
  Left valueIdentifier -> do
    let identifierUnusableError = ShouldNotGetHereError "Found unusable value identifier in expressionGenerator. This should have already been caught in identifier binding."
    encodedIdentifier <- getIdentifierInContext identifierUnusableError valueIdentifier
    return $ IdentifierExpr (getValueIdentifierIndex encodedIdentifier)
  Right functionIdentifier -> do
    capturedIdentifiers <- getFunctionCapturedIdentifiersInContext expressionRange functionIdentifier
    let (BoundFunctionIdentifier functionIndex _) = functionIdentifier
    return $ FunctionExpr functionIndex (getValueIdentifierIndex <$> capturedIdentifiers)
expressionGenerator (FunctionExpression _ functionDefinition) = do
  let combinedCapturedIdentifiers = tcFunctionDefinitionCapturedIdentifiers . getFunctionDefinitionData $ functionDefinition
  (capturedValueIdentifiers, capturedFunctionIdentifiers) <- consolidateCapturedIdentifiers combinedCapturedIdentifiers
  unless (Set.size capturedFunctionIdentifiers == 0) $
    throwError (ShouldNotGetHereError "Some captured functions were not initialized in expressionGenerator")
  let identifierUnusableError = ShouldNotGetHereError "Found unusable captured identifier in expressionGenerator. This should have already been caught in identifier binding."
  capturedFunctionIdentifierContextValues <- mapM (getIdentifierInContext identifierUnusableError) $ Set.toAscList capturedValueIdentifiers
  (makeSubFunc, capturedIdentifierInnerValues) <- withCapturedIdentifiers capturedValueIdentifiers (functionDefinitionGenerator functionDefinition)
  functionIndex <- getNewFunctionIndex
  let subFunction = makeSubFunc (getValueIdentifierIndex <$> capturedIdentifierInnerValues)
  addSubFunction functionIndex subFunction
  return $ FunctionExpr functionIndex (getValueIdentifierIndex <$> Seq.fromList capturedFunctionIdentifierContextValues)
expressionGenerator (RecordExpression _ _ recordName _ fieldValueMap) = do
  recordFieldOrder <- getRecordFieldOrder recordName
  let recordFields = (\fieldName -> fromJust $ Map.lookup fieldName fieldValueMap) <$> recordFieldOrder
  encodedRecordFields <- mapM expressionGenerator recordFields
  return $ RecordExpr (getRecordIdentifierIndex recordName) encodedRecordFields
expressionGenerator (FieldAccessExpression _ inner fieldName) = do
  encodedInner <- expressionGenerator inner
  let innerType = expressionType . getExpressionData $ inner
  recordNames <- case innerType of
    RecordUnionType _ recordNames -> return recordNames
    _ -> throwError $ ShouldNotGetHereError "Inner expression of FieldAccessExpression was not a record in expressionGenerator"
  recordIndexPairs <- forM (Map.keys recordNames) $ \recordName -> do
    fieldIndex <- getRecordFieldIndex recordName fieldName
    return (recordName, fieldIndex)
  let firstFieldIndex = snd . head $ recordIndexPairs
  if all (\(_, index) -> index == firstFieldIndex) recordIndexPairs
    then return $ FieldExpr encodedInner firstFieldIndex
    else do
      encodedCases <- forM (Seq.fromList recordIndexPairs) $ \(recordName, fieldIndex) -> do
        caseParameter <- getNewValueIdentifierIndex
        return (getRecordIdentifierIndex recordName, caseParameter, FieldExpr (IdentifierExpr caseParameter) fieldIndex)
      return $ CaseExpr encodedInner encodedCases
expressionGenerator (CaseExpression _ switch cases) = do
  encodedSwitch <- expressionGenerator switch
  let caseGenerator (recordName, (caseParameter, caseValue)) = do
        setIdentifierIsUsable caseParameter
        encodedCaseValue <- expressionGenerator caseValue
        return (getRecordIdentifierIndex recordName, getValueIdentifierIndex caseParameter, encodedCaseValue)
  encodedCases <- mapM caseGenerator $ Seq.fromList . Map.toList $ cases
  return $ CaseExpr encodedSwitch encodedCases
-- Standard cases
expressionGenerator (IntLiteralExpression _ value) = return $ LiteralExpr (IntLiteral value)
expressionGenerator (FloatLiteralExpression _ value) = return $ LiteralExpr (FloatLiteral value)
expressionGenerator (CharLiteralExpression _ value) = return $ LiteralExpr (CharLiteral value)
expressionGenerator (StringLiteralExpression _ value) = return $ LiteralExpr (StringLiteral value)
expressionGenerator (BoolLiteralExpression _ value) = return $ LiteralExpr (BoolLiteral value)
expressionGenerator (NilExpression _) = return $ LiteralExpr NilLiteral
expressionGenerator (NegateExpression _ inner) = do
  encodedInner <- expressionGenerator inner
  return $ BuiltInFunctionExpr NegateFn (Seq.singleton encodedInner)
expressionGenerator (AddExpression _ left right) = do
  encodedLeft <- expressionGenerator left
  encodedRight <- expressionGenerator right
  return $ BuiltInFunctionExpr AddFn (Seq.fromList [encodedLeft, encodedRight])
expressionGenerator (SubtractExpression _ left right) = do
  encodedLeft <- expressionGenerator left
  encodedRight <- expressionGenerator right
  return $ BuiltInFunctionExpr SubtractFn (Seq.fromList [encodedLeft, encodedRight])
expressionGenerator (MultiplyExpression _ left right) = do
  encodedLeft <- expressionGenerator left
  encodedRight <- expressionGenerator right
  return $ BuiltInFunctionExpr MultiplyFn (Seq.fromList [encodedLeft, encodedRight])
expressionGenerator (DivideExpression _ left right) = do
  encodedLeft <- expressionGenerator left
  encodedRight <- expressionGenerator right
  return $ BuiltInFunctionExpr DivideFn (Seq.fromList [encodedLeft, encodedRight])
expressionGenerator (ModuloExpression _ left right) = do
  encodedLeft <- expressionGenerator left
  encodedRight <- expressionGenerator right
  return $ BuiltInFunctionExpr ModuloFn (Seq.fromList [encodedLeft, encodedRight])
expressionGenerator (NotExpression _ inner) = do
  encodedInner <- expressionGenerator inner
  return $ BuiltInFunctionExpr NotFn (Seq.singleton encodedInner)
expressionGenerator (AndExpression _ left right) = do
  encodedLeft <- expressionGenerator left
  encodedRight <- expressionGenerator right
  return $ IfThenElseExpr encodedLeft encodedRight (LiteralExpr (BoolLiteral False))
expressionGenerator (OrExpression _ left right) = do
  encodedLeft <- expressionGenerator left
  encodedRight <- expressionGenerator right
  return $ IfThenElseExpr encodedLeft (LiteralExpr (BoolLiteral True)) encodedRight
expressionGenerator (EqualExpression _ left right) = do
  encodedLeft <- expressionGenerator left
  encodedRight <- expressionGenerator right
  return $ BuiltInFunctionExpr EqualFn (Seq.fromList [encodedLeft, encodedRight])
expressionGenerator (NotEqualExpression _ left right) = do
  encodedLeft <- expressionGenerator left
  encodedRight <- expressionGenerator right
  return $ BuiltInFunctionExpr NotEqualFn (Seq.fromList [encodedLeft, encodedRight])
expressionGenerator (GreaterExpression _ left right) = do
  encodedLeft <- expressionGenerator left
  encodedRight <- expressionGenerator right
  return $ BuiltInFunctionExpr GreaterFn (Seq.fromList [encodedLeft, encodedRight])
expressionGenerator (LessExpression _ left right) = do
  encodedLeft <- expressionGenerator left
  encodedRight <- expressionGenerator right
  return $ BuiltInFunctionExpr LessFn (Seq.fromList [encodedLeft, encodedRight])
expressionGenerator (GreaterEqualExpression _ left right) = do
  encodedLeft <- expressionGenerator left
  encodedRight <- expressionGenerator right
  return $ BuiltInFunctionExpr GreaterEqualFn (Seq.fromList [encodedLeft, encodedRight])
expressionGenerator (LessEqualExpression _ left right) = do
  encodedLeft <- expressionGenerator left
  encodedRight <- expressionGenerator right
  return $ BuiltInFunctionExpr LessEqualFn (Seq.fromList [encodedLeft, encodedRight])
expressionGenerator (IfThenElseExpression _ condition trueExpression maybeFalseExpression) = do
  encodedCondition <- expressionGenerator condition
  encodedTrueExpression <- expressionGenerator trueExpression
  encodedFalseExpression <- case maybeFalseExpression of
    Just falseExpression -> expressionGenerator falseExpression
    Nothing -> return (LiteralExpr NilLiteral)
  return $ IfThenElseExpr encodedCondition encodedTrueExpression encodedFalseExpression
expressionGenerator (ScopeExpression _ scope) = do
  encodedScope <- scopeGenerator scope
  return $ ScopeExpr encodedScope
expressionGenerator (FunctionCallExpression _ function arguments) = do
  encodedFunction <- expressionGenerator function
  encodedArguments <- traverse' expressionGenerator arguments
  return $ CallExpr encodedFunction encodedArguments