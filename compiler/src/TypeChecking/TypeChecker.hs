module TypeChecking.TypeChecker (runTypeChecking) where

import Control.Monad (foldM, forM_, unless)
import Core.ErrorState
import Core.Errors
import Core.FilePositions
import Core.SyntaxTree
import Core.Type
import Core.Utils
import Data.Bifunctor (Bifunctor (first), second)
import Data.Foldable (fold)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import IdentifierBinding.SyntaxTree
import Parsing.SyntaxTree
import TypeChecking.SyntaxTree
import TypeChecking.TypeChecking

runTypeChecking :: IBModule -> WithErrors (Map BoundRecordIdentifier (Seq UnboundIdentifier), TCModule)
runTypeChecking m = do
  let (finalState, typeCheckingResult) = runErrorState (typeCheckModule m) initialTypeCheckingState
  checkedModule <- typeCheckingResult
  return (recordFieldOrders finalState, checkedModule)

typeCheckModule :: IBModule -> TypeChecker TCModule
typeCheckModule (Module () mainFunction) = do
  checkedMainFunction <- typeCheckMainFunction mainFunction
  return $ Module () checkedMainFunction

typeCheckMainFunction :: IBMainFunction -> TypeChecker TCMainFunction
typeCheckMainFunction (MainFunction () scope) = do
  checkedScope <- typeCheckScope scope
  return $ MainFunction () checkedScope

typeCheckScope :: IBScope -> TypeChecker TCScope
typeCheckScope (Scope () nonPositionalStatements statements) = do
  mapM_ initializeNonPositionalStatement nonPositionalStatements
  checkedStatements <- mapM typeCheckStatement statements
  maybeCheckedNonPositionalStatements <- mapM typeCheckNonPositionalStatement nonPositionalStatements
  let checkedNonPositionalStatements = seqFilterMap id maybeCheckedNonPositionalStatements
  let returnInfo = fold $ statementReturnInfo . getStatementData <$> checkedStatements
  return $ Scope returnInfo checkedNonPositionalStatements checkedStatements

initializeNonPositionalStatement :: IBNonPositionalStatement -> TypeChecker ()
initializeNonPositionalStatement (FunctionStatement statementRange functionName (FunctionDefinition _ parameters (WithTypeAnnotation _ returnTypeAnnotation))) = do
  parameterTypes <- mapM getParameterType parameters
  returnType <- case returnTypeAnnotation of
    Just returnType -> liftWithErrors $ fromTypeExpression returnType
    Nothing -> throwError $ FunctionMissingReturnTypeAnnotation statementRange
  setFunctionType functionName (FunctionType parameterTypes returnType)
  where
    getParameterType :: IBWithTypeAnnotation IBValueIdentifier -> TypeChecker Type
    getParameterType (WithTypeAnnotation _ parameterTypeAnnotation) = case parameterTypeAnnotation of
      Just typeExpression -> liftWithErrors $ fromTypeExpression typeExpression
      Nothing -> throwError $ FunctionMissingParameterTypeAnnotation statementRange
initializeNonPositionalStatement (RecordStatement _ recordName fieldTypePairs) = do
  fieldTypeExpressionMap <- foldM addFieldType Map.empty fieldTypePairs
  fieldTypeMap <- liftWithErrors $ mapM fromTypeExpression fieldTypeExpressionMap
  setRecordFieldTypes recordName fieldTypeMap
  addRecordFieldOrder recordName (fst <$> fieldTypePairs)
  where
    addFieldType :: Map IBFieldIdentifier IBTypeExpression -> (IBFieldIdentifier, IBTypeExpression) -> TypeChecker (Map IBFieldIdentifier IBTypeExpression)
    addFieldType recordMap (fieldName, fieldType) = do
      let (maybeConflictingType, updatedRecordMap) = Map.insertLookupWithKey (\_ a _ -> a) fieldName fieldType recordMap
      case maybeConflictingType of
        Just conflictingType ->
          throwError $ RecordStatementConflictingFieldsError (getTextName recordName) fieldName (getRange fieldType) (getRange conflictingType)
        Nothing -> return updatedRecordMap

typeCheckNonPositionalStatement :: IBNonPositionalStatement -> TypeChecker (Maybe TCNonPositionalStatement)
typeCheckNonPositionalStatement (FunctionStatement statementRange functionName functionDefinition) = do
  checkedFunctionDefinition <- typeCheckFunctionDefinition functionDefinition
  return $ Just $ FunctionStatement statementRange functionName checkedFunctionDefinition
typeCheckNonPositionalStatement (RecordStatement {}) = return Nothing

typeCheckFunctionDefinition :: IBFunctionDefinition -> TypeChecker TCFunctionDefinition
typeCheckFunctionDefinition
  ( FunctionDefinition
      IBFunctionDefinitionData {ibFunctionDefinitionRange, ibFunctionDefinitionCapturedIdentifiers}
      parameters
      (WithTypeAnnotation body returnTypeAnnotation)
    ) = do
    (checkedParameters, parameterTypes) <- Seq.unzip <$> mapM initializeParameter parameters
    (returnType, returnTypeRange) <- case returnTypeAnnotation of
      Just returnTypeExpression -> do
        returnType <- liftWithErrors $ fromTypeExpression returnTypeExpression
        return $ (returnType, getRange returnTypeExpression)
      Nothing -> throwError $ FunctionMissingReturnTypeAnnotation ibFunctionDefinitionRange
    let functionContext = FunctionContext {contextReturnType = returnType, contextReturnTypeRange = returnTypeRange}
    checkedBody <- withFunctionContext functionContext $ typeCheckExpression body
    --   {- If the body of a function always runs a return statement when evaluated, the type of the body itself doesn't need
    --     to match the function return type. This enables writing functions whose body is a scope expression, but with return
    --     type other than Nil.

    --     Ex: []: Int -> { return 5; }
    --   -}
    let bodyReturnInfo = expressionReturnInfo . getExpressionData $ checkedBody
    let bodyType = expressionType . getExpressionData $ checkedBody
    unless (bodyReturnInfo == AlwaysReturns || bodyType `subTypes` returnType) $
      throwError (FunctionReturnTypeError returnTypeRange returnType (getRange body) bodyType)
    let functionDefinitionType = FunctionType parameterTypes returnType
    let functionDefinitionData =
          TCFunctionDefinitionData
            { tcFunctionDefinitionRange = ibFunctionDefinitionRange,
              tcFunctionDefinitionType = functionDefinitionType,
              tcFunctionDefinitionCapturedIdentifiers = ibFunctionDefinitionCapturedIdentifiers
            }
    return $ FunctionDefinition functionDefinitionData checkedParameters (WithTypeAnnotation checkedBody ())
    where
      initializeParameter :: IBWithTypeAnnotation IBValueIdentifier -> TypeChecker (TCWithTypeAnnotation TCValueIdentifier, Type)
      initializeParameter (WithTypeAnnotation parameter parameterTypeAnnotation) = do
        parameterType <- case parameterTypeAnnotation of
          Just typeExpression -> liftWithErrors $ fromTypeExpression typeExpression
          Nothing -> throwError $ FunctionMissingParameterTypeAnnotation ibFunctionDefinitionRange
        setValueIdentifierType parameter parameterType
        return (WithTypeAnnotation parameter (), parameterType)

typeCheckStatement :: IBStatement -> TypeChecker TCStatement
typeCheckStatement (PrintStatement statementRange expression) = do
  checkedExpression <- typeCheckExpression expression
  let statementReturnInfo = expressionReturnInfo . getExpressionData $ checkedExpression
  return $ PrintStatement (TCStatementData statementRange statementReturnInfo) checkedExpression
typeCheckStatement (VariableDeclarationStatement statementRange mutability (WithTypeAnnotation variableName typeAnnotation) value) = do
  checkedValue <- typeCheckExpression value
  let valueType = expressionType . getExpressionData $ checkedValue
  variableType <- case typeAnnotation of
    Just expectedTypeExpression -> do
      expectedType <- liftWithErrors $ fromTypeExpression expectedTypeExpression
      unless (valueType `subTypes` expectedType) $
        throwError (VariableDeclarationTypeError statementRange expectedType valueType)
      return expectedType
    Nothing -> return valueType
  setValueIdentifierType variableName variableType
  let statementReturnInfo = expressionReturnInfo . getExpressionData $ checkedValue
  return $ VariableDeclarationStatement (TCStatementData statementRange statementReturnInfo) mutability (WithTypeAnnotation variableName ()) checkedValue
typeCheckStatement (VariableMutationStatement statementRange variableName value) = do
  checkedValue <- typeCheckExpression value
  let valueType = expressionType . getExpressionData $ checkedValue
  variableType <- getValueIdentifierType variableName
  unless (valueType `subTypes` variableType) $
    throwError (VariableMutationTypeError statementRange variableType valueType)
  let statementReturnInfo = expressionReturnInfo . getExpressionData $ checkedValue
  return $ VariableMutationStatement (TCStatementData statementRange statementReturnInfo) variableName checkedValue
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
    Nothing ->
      unless (returnValueType == NilType) $
        throwError (MainFunctionReturnTypeError statementRange returnValueType)
    Just (FunctionContext {contextReturnType, contextReturnTypeRange}) ->
      unless (returnValueType `subTypes` contextReturnType) $
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
typeCheckExpression (IdentifierExpression expressionRange identifier) = do
  expressionType <- case identifier of
    Left valueIdentifier -> getValueIdentifierType valueIdentifier
    Right functionIdentifier -> getFunctionType functionIdentifier
  return $ IdentifierExpression (TCExpresionData expressionRange expressionType NeverReturns) identifier
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
      case getCombinedType trueBranchType falseBranchType of
        Nothing -> throwError $ IfThenElseExpressionBranchesTypeError expressionRange trueBranchType falseBranchType
        Just combinedType -> return combinedType
  let returnInfo = conditionReturnInfo `riAnd` (trueBranchReturnInfo `riOr` falseBranchReturnInfo)
  return $ IfThenElseExpression (TCExpresionData expressionRange expressionType returnInfo) checkedCondition checkedTrueBranch checkedFalseBranch
typeCheckExpression (ScopeExpression expressionRange scope) = do
  checkedScope <- typeCheckScope scope
  let returnInfo = getScopeData checkedScope
  return $ ScopeExpression (TCExpresionData expressionRange NilType returnInfo) checkedScope
typeCheckExpression (FunctionExpression expressionRange functionDefinition) = do
  checkedFunctionDefinition <- typeCheckFunctionDefinition functionDefinition
  let expressionType = tcFunctionDefinitionType . getFunctionDefinitionData $ checkedFunctionDefinition
  return $ FunctionExpression (TCExpresionData expressionRange expressionType NeverReturns) checkedFunctionDefinition
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
typeCheckExpression (RecordExpression expressionRange recordName fieldValueMap) = do
  checkedFieldValueMap <- mapM typeCheckExpression fieldValueMap
  recordTypeMap <- getRecordFieldTypes recordName
  let typeCheckField (fieldName, fieldValue) = case Map.lookup fieldName recordTypeMap of
        Nothing -> throwError $ RecordExpressionExtraFieldError (getTextName recordName) fieldName expressionRange
        Just expectedType ->
          let fieldType = expressionType . getExpressionData $ fieldValue
           in unless (fieldType == expectedType) $
                throwError (RecordExpressionFieldTypeError (getTextName recordName) fieldName expectedType fieldType expressionRange)
  mapM_ typeCheckField (Map.toList checkedFieldValueMap)
  let checkRecordHasField fieldName =
        unless (Map.member fieldName checkedFieldValueMap) $
          throwError (RecordExpressionMissingFieldError (getTextName recordName) fieldName expressionRange)
  mapM_ checkRecordHasField (Map.keys recordTypeMap)
  let returnInfo = fold $ expressionReturnInfo . getExpressionData <$> checkedFieldValueMap
  return $ RecordExpression (TCExpresionData expressionRange (RecordUnionType (Set.singleton recordName)) returnInfo) recordName checkedFieldValueMap
typeCheckExpression (FieldAccessExpression expressionRange inner fieldName) = do
  checkedInner <- typeCheckExpression inner
  let innerExpressionType = expressionType . getExpressionData $ checkedInner
  innerExpressionRecordNames <- case innerExpressionType of
    RecordUnionType recordNames -> return recordNames
    otherType -> throwError $ AccessedFieldOfNonRecordValueError otherType expressionRange
  recordFieldTypePairs <- mapM (getRecordFieldType innerExpressionType) (Set.toList innerExpressionRecordNames)
  expressionType <- case getCombinedTypeF $ snd <$> recordFieldTypePairs of
    Nothing -> throwError $ FieldTypesAreNotCompatibleError fieldName (first getTextName <$> recordFieldTypePairs) expressionRange
    Just expressionType -> return expressionType
  let returnInfo = expressionReturnInfo . getExpressionData $ checkedInner
  return $ FieldAccessExpression (TCExpresionData expressionRange expressionType returnInfo) checkedInner fieldName
  where
    getRecordFieldType innerExpressionType recordName = do
      fieldTypeMap <- getRecordFieldTypes recordName
      case Map.lookup fieldName fieldTypeMap of
        Nothing -> throwError $ AccessedFieldNotInRecordError (getTextName recordName) fieldName innerExpressionType expressionRange
        Just fieldType -> return (recordName, fieldType)
typeCheckExpression (CaseExpression expressionRange switch cases) = do
  checkedSwitch <- typeCheckExpression switch
  let switchType = expressionType . getExpressionData $ checkedSwitch
  switchPossibleRecords <- case switchType of
    RecordUnionType recordNames -> return recordNames
    nonRecordType -> throwError $ CaseSwitchHasNonRecordTypeError (getRange switch) nonRecordType
  mapM_
    ( \recordName ->
        unless (Map.member recordName cases) $
          throwError (CaseExpressionMisingCaseError expressionRange (getTextName recordName))
    )
    switchPossibleRecords
  mapM_
    ( \recordName ->
        unless
          (Set.member recordName switchPossibleRecords)
          $ throwError (CaseExpressionExtraneousCaseError expressionRange (getTextName recordName))
    )
    (Map.keys cases)
  forM_ (Map.toList cases) $ \(recordName, (caseParameter, _)) -> setValueIdentifierType caseParameter (RecordUnionType $ Set.singleton recordName)
  checkedCases <- mapM (secondM typeCheckExpression) cases
  let caseRecordTypePairs = second (expressionType . getExpressionData . snd) <$> Map.toList checkedCases
  expressionType <- case getCombinedTypeF $ snd <$> caseRecordTypePairs of
    Nothing -> throwError $ CaseTypesAreNotCompatibleError (first getTextName <$> caseRecordTypePairs) expressionRange
    Just expressionType -> return expressionType
  let returnInfo = (expressionReturnInfo . getExpressionData $ checkedSwitch) `riAnd` foldl1 riOr (expressionReturnInfo . getExpressionData . snd <$> Map.elems checkedCases)
  return $ CaseExpression (TCExpresionData expressionRange expressionType returnInfo) checkedSwitch checkedCases

checkArgumentType :: (Type, TCExpression) -> TypeChecker ()
checkArgumentType (parameterType, argument) = do
  let argumentType = expressionType . getExpressionData $ argument
  unless (argumentType == parameterType) $
    throwError (FunctionCallExpressionArgumentTypeError (getRange argument) parameterType argumentType)