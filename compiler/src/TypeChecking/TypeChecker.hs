module TypeChecking.TypeChecker (runTypeChecking) where

import Control.Monad (foldM, forM, forM_, unless, when, (>=>))
import Core.ErrorState
import Core.Errors
import Core.FilePositions
import Core.SyntaxTree
import Core.Utils
import Data.Bifunctor (Bifunctor (first), second)
import Data.Foldable (fold, toList)
import Data.Functor ((<&>))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import IdentifierBinding.SyntaxTree
import Parsing.SyntaxTree
import TypeChecking.SyntaxTree
import TypeChecking.Type
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
  mapM_ initializeTypeSynonym nonPositionalStatements
  mapM_ initializeNonPositionalStatement nonPositionalStatements
  checkedStatements <- mapM typeCheckStatement statements
  maybeCheckedNonPositionalStatements <- mapM typeCheckNonPositionalStatement nonPositionalStatements
  let checkedNonPositionalStatements = seqFilterMap id maybeCheckedNonPositionalStatements
  let returnInfo = fold $ statementReturnInfo . getStatementData <$> checkedStatements
  return $ Scope returnInfo checkedNonPositionalStatements checkedStatements

initializeTypeSynonym :: IBNonPositionalStatement -> TypeChecker ()
initializeTypeSynonym (TypeStatement _ typeSynonym maybeMutabilityParameter typeParameters typeValue) =
  initializeTypeSynonymType typeSynonym maybeMutabilityParameter typeParameters typeValue
initializeTypeSynonym _ = return ()

initializeNonPositionalStatement :: IBNonPositionalStatement -> TypeChecker ()
initializeNonPositionalStatement (FunctionStatement statementRange functionName typeParameters (FunctionDefinition _ parameters (WithTypeAnnotation _ returnTypeAnnotation))) = do
  parameterTypes <- mapM (getParameterTypeExpression >=> getParametrizedTypeFunc Nothing typeParameters) parameters
  returnTypeFunc <- case returnTypeAnnotation of
    Just returnTypeExpression -> getParametrizedTypeFunc Nothing typeParameters returnTypeExpression
    Nothing -> throwError $ FunctionMissingReturnTypeAnnotation statementRange
  let functionTypeFunc typeArguments = FunctionType (parameterTypes <&> (\f -> f Immutable typeArguments)) (returnTypeFunc Immutable typeArguments)
  setFunctionType functionName (Seq.length typeParameters) functionTypeFunc
  where
    getParameterTypeExpression :: IBWithTypeAnnotation IBValueIdentifier -> TypeChecker IBTypeExpression
    getParameterTypeExpression (WithTypeAnnotation _ parameterTypeAnnotation) = case parameterTypeAnnotation of
      Just typeExpression -> return typeExpression
      Nothing -> throwError $ FunctionMissingParameterTypeAnnotation statementRange
initializeNonPositionalStatement (RecordStatement _ recordName mutabilityParameter typeParameters fieldTypePairs) = do
  fieldTypeExpressionMap <- foldM addFieldType Map.empty fieldTypePairs
  fieldTypeFuncMap <- mapM (getRecordFieldTypeFunc True mutabilityParameter typeParameters) fieldTypeExpressionMap
  setRecordFieldTypes recordName (Seq.length typeParameters) $ \mutability typeArguments -> fieldTypeFuncMap <&> \f -> f mutability typeArguments
  addRecordFieldOrder recordName (fst <$> fieldTypePairs)
  -- [BUG-4] Record type parameter variances should be properly calculated
  setRecordTypeParameterVariances recordName (typeParameters <&> const Covariant) (typeParameters <&> const Invariant)
  where
    addFieldType :: Map IBFieldIdentifier IBTypeExpression -> (IBFieldIdentifier, IBTypeExpression) -> TypeChecker (Map IBFieldIdentifier IBTypeExpression)
    addFieldType recordMap (fieldName, fieldType) = do
      let (maybeConflictingType, updatedRecordMap) = Map.insertLookupWithKey (\_ a _ -> a) fieldName fieldType recordMap
      case maybeConflictingType of
        Just conflictingType ->
          throwError $ RecordStatementConflictingFieldsError (getTextName recordName) fieldName (getRange fieldType) (getRange conflictingType)
        Nothing -> return updatedRecordMap
initializeNonPositionalStatement (TypeStatement statementRange typeSynonym _ typeParameters _) = do
  _ <- getTypeSynonymType statementRange (Seq.length typeParameters) typeSynonym
  return ()

typeCheckNonPositionalStatement :: IBNonPositionalStatement -> TypeChecker (Maybe TCNonPositionalStatement)
typeCheckNonPositionalStatement
  ( FunctionStatement
      statementRange
      functionName
      typeParameters
      ( FunctionDefinition
          IBFunctionDefinitionData {ibFunctionDefinitionRange, ibFunctionDefinitionCapturedIdentifiers}
          parameters
          (WithTypeAnnotation body _)
        )
    ) = do
    let typeParameterTypes = typeParameters <&> \(BoundTypeParameter typeParameterIndex typeParameterName) -> IdentifierType typeParameterIndex typeParameterName
    functionType <- getFunctionType statementRange (Left functionName) typeParameterTypes
    (parameterTypes, returnType) <- case functionType of
      FunctionType parameterTypes returnType -> return (parameterTypes, returnType)
      _ -> throwError $ ShouldNotGetHereError "Got non-function type for function in typeCheckNonPositionalStatement"
    let typedParameters = Seq.zip parameters parameterTypes <&> \(WithTypeAnnotation parameterName _, parameterType) -> (parameterName, parameterType)
    checkedBody <- typeCheckFunction statementRange typedParameters body returnType
    let functionDefinitionData =
          TCFunctionDefinitionData
            { tcFunctionDefinitionRange = ibFunctionDefinitionRange,
              tcFunctionDefinitionType = functionType,
              tcFunctionDefinitionCapturedIdentifiers = ibFunctionDefinitionCapturedIdentifiers
            }
    let checkedFunctionDefinition =
          FunctionDefinition
            functionDefinitionData
            (typedParameters <&> \(parameterName, _) -> WithTypeAnnotation parameterName ())
            (WithTypeAnnotation checkedBody ())
    return $ Just $ FunctionStatement statementRange functionName () checkedFunctionDefinition
typeCheckNonPositionalStatement (RecordStatement {}) = return Nothing
typeCheckNonPositionalStatement (TypeStatement {}) = return Nothing

typeCheckFunction :: Range -> Seq (IBValueIdentifier, Type) -> IBExpression -> Type -> TypeChecker TCExpression
typeCheckFunction _functionRange typedParameters body returnType = do
  forM_ typedParameters $ uncurry setValueIdentifierType
  let functionContext = FunctionContext {contextReturnType = returnType}
  withFunctionContext functionContext $ case body of
    ScopeExpression {} -> do
      --   {- If the body of a function always runs a return statement when evaluated, the type of the body itself doesn't need
      --     to match the function return type. This enables writing functions whose body is a scope expression, but with return
      --     type other than Nil.

      --     Ex: []: Int -> { return 5; }
      --   -}
      checkedBody <- typeSynthesizeExpression body
      let bodyReturnInfo = expressionReturnInfo . getExpressionData $ checkedBody
      let bodyType = expressionType . getExpressionData $ checkedBody
      bodyTypeIsCompatible <- bodyType `isCompatibleWith` returnType
      unless (bodyReturnInfo == AlwaysReturns || bodyTypeIsCompatible) $
        throwError (FunctionBodyTypeError (getRange body) returnType bodyType)
      return checkedBody
    _ -> typeCheckExpression returnType body

typeCheckStatement :: IBStatement -> TypeChecker TCStatement
typeCheckStatement (VariableDeclarationStatement statementRange mutability (WithTypeAnnotation variableName typeAnnotation) value) = do
  (checkedValue, variableType) <- case typeAnnotation of
    Just expectedTypeExpression -> do
      expectedType <- fromTypeExpression expectedTypeExpression
      checkedValue <- typeCheckExpression expectedType value
      return (checkedValue, expectedType)
    Nothing -> do
      checkedValue <- typeSynthesizeExpression value
      let variableType = expressionType . getExpressionData $ checkedValue
      return (checkedValue, variableType)
  setValueIdentifierType variableName variableType
  let statementReturnInfo = expressionReturnInfo . getExpressionData $ checkedValue
  return $ VariableDeclarationStatement (TCStatementData statementRange statementReturnInfo) mutability (WithTypeAnnotation variableName ()) checkedValue
typeCheckStatement (VariableMutationStatement statementRange variableName value) = do
  variableType <- getValueIdentifierType variableName
  checkedValue <- typeCheckExpression variableType value
  let statementReturnInfo = expressionReturnInfo . getExpressionData $ checkedValue
  return $ VariableMutationStatement (TCStatementData statementRange statementReturnInfo) variableName checkedValue
typeCheckStatement (FieldMutationStatement statementRange record field value) = do
  checkedRecord <- typeSynthesizeExpression record
  let recordType = expressionType . getExpressionData $ checkedRecord
  recordNames <- case recordType of
    RecordUnionType Immutable _ -> throwError $ MutatedFieldOfImmutableRecordError statementRange recordType
    RecordUnionType Mutable recordNames -> return recordNames
    nonRecordType -> throwError $ MutatedFieldOfNonRecordTypeError statementRange nonRecordType
  recordFieldTypePairs <- mapM (getRecordFieldTypePair Mutable recordType) (Map.toList recordNames)
  maybeFieldType <- typeIntersectionF . NonEmpty.fromList $ snd <$> recordFieldTypePairs
  fieldType <- case maybeFieldType of
    Nothing -> throwError $ FieldTypesHaveEmptyIntersectionError field (first getTextName <$> recordFieldTypePairs) statementRange
    Just expressionType -> return expressionType
  checkedValue <- typeCheckExpression fieldType value
  let statementReturnInfo = (expressionReturnInfo . getExpressionData $ checkedRecord) `riAnd` (expressionReturnInfo . getExpressionData $ checkedValue)
  return $ FieldMutationStatement (TCStatementData statementRange statementReturnInfo) checkedRecord field checkedValue
  where
    getRecordFieldTypePair mutability innerExpressionType (recordName, recordTypeParameters) = do
      fieldTypeMap <- getRecordFieldTypes statementRange recordName mutability recordTypeParameters
      case Map.lookup field fieldTypeMap of
        Nothing -> throwError $ MutatedFieldNotInRecordError (getTextName recordName) field innerExpressionType statementRange
        Just fieldType -> return (recordName, fieldType)
typeCheckStatement (IndexMutationStatement statementRange list index value) = do
  checkedList <- typeSynthesizeExpression list
  valueType <- case expressionType . getExpressionData $ checkedList of
    ListType Mutable valueType -> return valueType
    ListType Immutable valueType -> throwError $ MutatedImmutableListIndexError statementRange (ListType Immutable valueType)
    nonListType -> throwError $ MutatedNonListIndexError statementRange nonListType
  checkedIndex <- typeCheckExpression IntType index
  checkedValue <- typeCheckExpression valueType value
  let statementReturnInfo =
        (expressionReturnInfo . getExpressionData $ checkedList)
          `riAnd` (expressionReturnInfo . getExpressionData $ checkedIndex)
          `riAnd` (expressionReturnInfo . getExpressionData $ checkedValue)
  return $ IndexMutationStatement (TCStatementData statementRange statementReturnInfo) checkedList checkedIndex checkedValue
typeCheckStatement (ExpressionStatement statementRange expression) = do
  checkedExpression <- typeSynthesizeExpression expression
  let statementReturnInfo = expressionReturnInfo . getExpressionData $ checkedExpression
  return $ ExpressionStatement (TCStatementData statementRange statementReturnInfo) checkedExpression
typeCheckStatement (WhileLoopStatement statementRange condition body) = do
  checkedCondition <- typeCheckExpression BoolType condition
  checkedBody <- typeSynthesizeExpression body
  let statementReturnInfo = (expressionReturnInfo . getExpressionData $ checkedCondition) `riAnd` (expressionReturnInfo . getExpressionData $ checkedBody)
  return $ WhileLoopStatement (TCStatementData statementRange statementReturnInfo) checkedCondition checkedBody
typeCheckStatement (ReturnStatement statementRange returnValue) = do
  functionContext <- getFunctionContext
  let expectedReturnType = case functionContext of
        Nothing -> NilType
        Just (FunctionContext {contextReturnType}) -> contextReturnType
  checkedReturnValue <- case returnValue of
    Nothing -> do
      nilIsValidReturnType <- NilType `isCompatibleWith` expectedReturnType
      unless nilIsValidReturnType $ throwError (FunctionNilReturnTypeError statementRange expectedReturnType)
      return Nothing
    Just value -> do
      checkedValue <- typeCheckExpression expectedReturnType value
      return $ Just checkedValue
  return $ ReturnStatement (TCStatementData statementRange AlwaysReturns) checkedReturnValue

typeCheckExpression :: Type -> IBExpression -> TypeChecker TCExpression
typeCheckExpression expectedType expression = case expression of
  (NegateExpression expressionRange inner) -> do
    if expectedType == IntType || expectedType == FloatType
      then do
        checkedInner <- typeCheckExpression expectedType inner
        let returnInfo = expressionReturnInfo . getExpressionData $ checkedInner
        return $ NegateExpression (TCExpresionData expressionRange expectedType returnInfo) checkedInner
      else do
        typeCheckExpressionDefault
  (IfThenElseExpression expressionRange condition trueBranch maybeFalseBranch) -> do
    checkedCondition <- typeCheckExpression BoolType condition
    let conditionReturnInfo = expressionReturnInfo . getExpressionData $ checkedCondition
    checkedTrueBranch <- typeCheckExpression expectedType trueBranch
    let trueBranchReturnInfo = expressionReturnInfo . getExpressionData $ checkedTrueBranch
    checkedFalseBranch <- case maybeFalseBranch of
      Nothing -> do
        nilMeetsExpectation <- NilType `isCompatibleWith` expectedType
        if nilMeetsExpectation
          then return Nothing
          else throwError $ IfThenExpressionNilTypeError expressionRange expectedType
      Just falseBranch -> Just <$> typeCheckExpression expectedType falseBranch
    let falseBranchReturnInfo = case checkedFalseBranch of
          Just e -> expressionReturnInfo . getExpressionData $ e
          Nothing -> NeverReturns
    let returnInfo = conditionReturnInfo `riAnd` (trueBranchReturnInfo `riOr` falseBranchReturnInfo)
    return $ IfThenElseExpression (TCExpresionData expressionRange expectedType returnInfo) checkedCondition checkedTrueBranch checkedFalseBranch
  ( FunctionExpression
      expressionRange
      ( FunctionDefinition
          IBFunctionDefinitionData {ibFunctionDefinitionRange, ibFunctionDefinitionCapturedIdentifiers}
          parameters
          (WithTypeAnnotation body maybeReturnTypeAnnotation)
        )
    ) -> do
      (expectedParameterTypes, expectedBodyType) <- case expectedType of
        FunctionType parameterTypes returnType -> return (parameterTypes, returnType)
        _ -> throwError $ FunctionTypeError expressionRange expectedType
      unless (Seq.length expectedParameterTypes == Seq.length parameters) $
        throwError (FunctionArityError expressionRange expectedType $ Seq.length parameters)
      typedParameters <- forM (Seq.zip parameters expectedParameterTypes) $ \(WithTypeAnnotation parameterName maybeParameterTypeAnnotation, expectedParameterType) -> do
        parameterType <- case maybeParameterTypeAnnotation of
          Nothing -> return expectedParameterType
          Just parameterTypeAnnotation -> do
            parameterType <- fromTypeExpression parameterTypeAnnotation
            parameterCompatibleWithExpectedType <- parameterType `isCompatibleWith` expectedParameterType
            unless parameterCompatibleWithExpectedType $ throwError (FunctionParameterTypeError expressionRange (getTextName parameterName) expectedParameterType parameterType)
            return parameterType
        return $ (parameterName, parameterType)
      bodyType <- case maybeReturnTypeAnnotation of
        Nothing -> return expectedBodyType
        Just returnTypeAnnotation -> do
          bodyType <- fromTypeExpression returnTypeAnnotation
          bodyCompatibleWithExpectedType <- bodyType `isCompatibleWith` expectedBodyType
          unless bodyCompatibleWithExpectedType $ throwError (FunctionReturnTypeError expressionRange expectedBodyType bodyType)
          return bodyType
      checkedBody <- typeCheckFunction expressionRange typedParameters body bodyType
      let functionDefinitionData =
            TCFunctionDefinitionData
              { tcFunctionDefinitionRange = ibFunctionDefinitionRange,
                tcFunctionDefinitionType = expectedType,
                tcFunctionDefinitionCapturedIdentifiers = ibFunctionDefinitionCapturedIdentifiers
              }
      let checkedParameters = typedParameters <&> \(parameterName, _) -> WithTypeAnnotation parameterName ()
      return $
        FunctionExpression
          (TCExpresionData expressionRange expectedType NeverReturns)
          (FunctionDefinition functionDefinitionData checkedParameters (WithTypeAnnotation checkedBody ()))
  (RecordExpression expressionRange mutability recordName typeArgumentExpressions fieldValueMap) -> do
    typeArguments <-
      if Seq.length typeArgumentExpressions == 0
        -- If type arguments are not provided explicitly, we try to infer them from the expected type
        then case expectedType of
          RecordUnionType expectedMutability recordMap -> case Map.lookup recordName recordMap of
            Nothing -> throwError $ RecordExpresssionRecordNotInTypeError expressionRange expectedType (getTextName recordName)
            Just expectedTypeArguments -> case (expectedMutability, mutability) of
              (Mutable, Immutable) -> throwError $ RecordExpressionMutabilityTypeError expressionRange
              _ -> return expectedTypeArguments
          _ -> throwError $ RecordExpresssionRecordNotInTypeError expressionRange expectedType (getTextName recordName)
        else do
          expectedNumTypeParameters <- getRecordNumTypeParameters recordName
          unless (Seq.length typeArgumentExpressions == expectedNumTypeParameters) $
            throwError (RecordExpresssionNumTypeArgumentsError expressionRange (getTextName recordName) expectedNumTypeParameters (Seq.length typeArgumentExpressions))
          typeArguments <- mapM fromTypeExpression typeArgumentExpressions
          let actualType = RecordUnionType mutability (Map.singleton recordName typeArguments)
          typesAreCompatible <- actualType `isCompatibleWith` expectedType
          unless typesAreCompatible $ throwError (TypeExpectationError expressionRange expectedType actualType)
          return typeArguments
    fieldTypeMap <- getRecordFieldTypes expressionRange recordName mutability typeArguments
    checkedFieldValues <- forM (Map.toList fieldTypeMap) $ \(fieldName, fieldType) -> case Map.lookup fieldName fieldValueMap of
      Nothing -> throwError $ RecordExpressionMissingFieldError (getTextName recordName) fieldName expressionRange
      Just fieldValue -> do
        checkedFieldValue <- typeCheckExpression fieldType fieldValue
        return (fieldName, checkedFieldValue)
    forM_ (Map.keys fieldValueMap) $ \fieldName -> case Map.lookup fieldName fieldTypeMap of
      Nothing -> throwError $ RecordExpressionExtraFieldError (getTextName recordName) fieldName expressionRange
      Just _ -> return ()
    let checkedFieldValueMap = Map.fromList checkedFieldValues
    let returnInfo = fold $ expressionReturnInfo . getExpressionData <$> checkedFieldValueMap
    return $ RecordExpression (TCExpresionData expressionRange (RecordUnionType mutability (Map.singleton recordName typeArguments)) returnInfo) mutability recordName () checkedFieldValueMap
  (FieldAccessExpression expressionRange inner fieldName) -> do
    checkedInner <- typeSynthesizeExpression inner
    let innerExpressionType = expressionType . getExpressionData $ checkedInner
    (innerExpressionMutability, innerExpressionRecordNames) <- case innerExpressionType of
      RecordUnionType mutability recordNames -> return (mutability, recordNames)
      otherType -> throwError $ AccessedFieldOfNonRecordValueError otherType expressionRange
    forM_ (Map.toList innerExpressionRecordNames) $ \(recordName, recordTypeArguments) -> do
      fieldTypeMap <- getRecordFieldTypes expressionRange recordName innerExpressionMutability recordTypeArguments
      case Map.lookup fieldName fieldTypeMap of
        Nothing -> throwError $ AccessedFieldNotInRecordError (getTextName recordName) fieldName innerExpressionType expressionRange
        Just fieldType -> do
          fieldTypeIsCompatible <- fieldType `isCompatibleWith` expectedType
          unless fieldTypeIsCompatible $ throwError (TypeExpectationError expressionRange expectedType fieldType)
    let returnInfo = expressionReturnInfo . getExpressionData $ checkedInner
    return $ FieldAccessExpression (TCExpresionData expressionRange expectedType returnInfo) checkedInner fieldName
  (CaseExpression expressionRange switch cases) -> do
    checkedSwitch <- typeSynthesizeExpression switch
    let switchType = expressionType . getExpressionData $ checkedSwitch
    (switchMutability, switchRecordMap) <- case switchType of
      RecordUnionType mutability recordNames -> return (mutability, recordNames)
      nonRecordType -> throwError $ CaseSwitchHasNonRecordTypeError (getRange switch) nonRecordType
    forM_ (Map.keys switchRecordMap) $ \recordName ->
      unless (Map.member recordName cases) $
        throwError (CaseExpressionMisingCaseError expressionRange (getTextName recordName))
    forM_ (Map.keys cases) $ \recordName ->
      unless
        (Map.member recordName switchRecordMap)
        $ throwError (CaseExpressionExtraneousCaseError expressionRange (getTextName recordName))
    forM_ (Map.toList cases) $ \(recordName, (caseParameter, _)) ->
      setValueIdentifierType caseParameter (RecordUnionType switchMutability $ Map.singleton recordName (switchRecordMap ! recordName))
    checkedCases <- mapM (secondM $ typeCheckExpression expectedType) cases
    let returnInfo = (expressionReturnInfo . getExpressionData $ checkedSwitch) `riAnd` foldl1 riOr (expressionReturnInfo . getExpressionData . snd <$> Map.elems checkedCases)
    return $ CaseExpression (TCExpresionData expressionRange expectedType returnInfo) checkedSwitch checkedCases
  (ListExpression expressionRange mutability typeArgumentExpressions values) -> do
    expectedValueType <- case expectedType of
      ListType _ expectedValueType -> return expectedValueType
      _ -> throwError $ ListTypeError expressionRange expectedType
    typeArguments <- mapM fromTypeExpression typeArgumentExpressions
    valueType <- case typeArguments of
      Empty -> return expectedValueType
      valueType :<| Empty -> do
        let actualType = ListType mutability valueType
        typesAreCompatible <- actualType `isCompatibleWith` expectedType
        unless typesAreCompatible $ throwError (TypeExpectationError expressionRange expectedType actualType)
        return valueType
      _ -> throwError $ ListWrongNumberOfTypeArgumentsError expressionRange (Seq.length typeArguments)
    checkedValues <- forM values $ typeCheckExpression valueType
    let returnInfo = foldMap (expressionReturnInfo . getExpressionData) checkedValues
    return $ ListExpression (TCExpresionData expressionRange expectedType returnInfo) mutability () checkedValues
  (IndexExpression expressionRange innerExpression indexExpression) -> do
    checkedInnerExpression <- typeCheckExpression (ListType Immutable expectedType) innerExpression
    checkedIndexExpression <- typeCheckExpression IntType indexExpression
    let returnInfo =
          (expressionReturnInfo . getExpressionData $ checkedInnerExpression)
            `riAnd` (expressionReturnInfo . getExpressionData $ checkedIndexExpression)
    return $ IndexExpression (TCExpresionData expressionRange expectedType returnInfo) checkedInnerExpression checkedIndexExpression
  _ -> typeCheckExpressionDefault
  where
    typeCheckExpressionDefault :: TypeChecker TCExpression
    typeCheckExpressionDefault = do
      synthesizedExpression <- typeSynthesizeExpression expression
      let synthesizedType = expressionType . getExpressionData $ synthesizedExpression
      typeMeetsExpectation <- synthesizedType `isCompatibleWith` expectedType
      unless typeMeetsExpectation $ throwError (TypeExpectationError (getRange expression) expectedType synthesizedType)
      return synthesizedExpression

typeSynthesizeExpression :: IBExpression -> TypeChecker TCExpression
typeSynthesizeExpression (IntLiteralExpression expressionRange value) =
  return $ IntLiteralExpression (TCExpresionData expressionRange IntType NeverReturns) value
typeSynthesizeExpression (FloatLiteralExpression expressionRange value) =
  return $ FloatLiteralExpression (TCExpresionData expressionRange FloatType NeverReturns) value
typeSynthesizeExpression (CharLiteralExpression expressionRange value) =
  return $ CharLiteralExpression (TCExpresionData expressionRange CharType NeverReturns) value
typeSynthesizeExpression (StringLiteralExpression expressionRange value) =
  return $ StringLiteralExpression (TCExpresionData expressionRange StringType NeverReturns) value
typeSynthesizeExpression (BoolLiteralExpression expressionRange value) =
  return $ BoolLiteralExpression (TCExpresionData expressionRange BoolType NeverReturns) value
typeSynthesizeExpression (NilExpression expressionRange) =
  return $ NilExpression (TCExpresionData expressionRange NilType NeverReturns)
typeSynthesizeExpression (IdentifierExpression expressionRange identifier) = case identifier of
  SimpleValueIdentifier valueIdentifier -> do
    expressionType <- getValueIdentifierType valueIdentifier
    return $ IdentifierExpression (TCExpresionData expressionRange expressionType NeverReturns) (SimpleValueIdentifier valueIdentifier)
  FunctionValueIdentifier functionIdentifier typeArgumentExpressions -> do
    typeArguments <- mapM fromTypeExpression typeArgumentExpressions
    expressionType <- getFunctionType expressionRange functionIdentifier typeArguments
    return $ IdentifierExpression (TCExpresionData expressionRange expressionType NeverReturns) (FunctionValueIdentifier functionIdentifier ())
typeSynthesizeExpression (NegateExpression expressionRange inner) = do
  checkedInner <- typeSynthesizeExpression inner
  expressionType <- case expressionType . getExpressionData $ checkedInner of
    IntType -> return IntType
    FloatType -> return FloatType
    unexpectedType -> throwError $ NegateExpressionTypeError expressionRange unexpectedType
  let returnInfo = expressionReturnInfo . getExpressionData $ checkedInner
  return $ NegateExpression (TCExpresionData expressionRange expressionType returnInfo) checkedInner
typeSynthesizeExpression (AddExpression expressionRange left right) = do
  checkedLeft <- typeSynthesizeExpression left
  checkedRight <- typeSynthesizeExpression right
  expressionType <- case (expressionType . getExpressionData $ checkedLeft, expressionType . getExpressionData $ checkedRight) of
    (IntType, IntType) -> return IntType
    (FloatType, FloatType) -> return FloatType
    (StringType, StringType) -> return StringType
    (CharType, StringType) -> return StringType
    (StringType, CharType) -> return StringType
    (leftType, rightType) -> throwError $ AddExpressionTypeError expressionRange leftType rightType
  let returnInfo = (expressionReturnInfo . getExpressionData $ checkedLeft) `riAnd` (expressionReturnInfo . getExpressionData $ checkedRight)
  return $ AddExpression (TCExpresionData expressionRange expressionType returnInfo) checkedLeft checkedRight
typeSynthesizeExpression (SubtractExpression expressionRange left right) = do
  checkedLeft <- typeSynthesizeExpression left
  checkedRight <- typeSynthesizeExpression right
  expressionType <- case (expressionType . getExpressionData $ checkedLeft, expressionType . getExpressionData $ checkedRight) of
    (IntType, IntType) -> return IntType
    (FloatType, FloatType) -> return FloatType
    (leftType, rightType) -> throwError $ SubtractExpressionTypeError expressionRange leftType rightType
  let returnInfo = (expressionReturnInfo . getExpressionData $ checkedLeft) `riAnd` (expressionReturnInfo . getExpressionData $ checkedRight)
  return $ SubtractExpression (TCExpresionData expressionRange expressionType returnInfo) checkedLeft checkedRight
typeSynthesizeExpression (MultiplyExpression expressionRange left right) = do
  checkedLeft <- typeSynthesizeExpression left
  checkedRight <- typeSynthesizeExpression right
  expressionType <- case (expressionType . getExpressionData $ checkedLeft, expressionType . getExpressionData $ checkedRight) of
    (IntType, IntType) -> return IntType
    (FloatType, FloatType) -> return FloatType
    (leftType, rightType) -> throwError $ MultiplyExpressionTypeError expressionRange leftType rightType
  let returnInfo = (expressionReturnInfo . getExpressionData $ checkedLeft) `riAnd` (expressionReturnInfo . getExpressionData $ checkedRight)
  return $ MultiplyExpression (TCExpresionData expressionRange expressionType returnInfo) checkedLeft checkedRight
typeSynthesizeExpression (DivideExpression expressionRange left right) = do
  checkedLeft <- typeSynthesizeExpression left
  checkedRight <- typeSynthesizeExpression right
  expressionType <- case (expressionType . getExpressionData $ checkedLeft, expressionType . getExpressionData $ checkedRight) of
    (IntType, IntType) -> return IntType
    (FloatType, FloatType) -> return FloatType
    (leftType, rightType) -> throwError $ DivideExpressionTypeError expressionRange leftType rightType
  let returnInfo = (expressionReturnInfo . getExpressionData $ checkedLeft) `riAnd` (expressionReturnInfo . getExpressionData $ checkedRight)
  return $ DivideExpression (TCExpresionData expressionRange expressionType returnInfo) checkedLeft checkedRight
typeSynthesizeExpression (ModuloExpression expressionRange left right) = do
  checkedLeft <- typeSynthesizeExpression left
  checkedRight <- typeSynthesizeExpression right
  expressionType <- case (expressionType . getExpressionData $ checkedLeft, expressionType . getExpressionData $ checkedRight) of
    (IntType, IntType) -> return IntType
    (FloatType, FloatType) -> return FloatType
    (leftType, rightType) -> throwError $ ModuloExpressionTypeError expressionRange leftType rightType
  let returnInfo = (expressionReturnInfo . getExpressionData $ checkedLeft) `riAnd` (expressionReturnInfo . getExpressionData $ checkedRight)
  return $ ModuloExpression (TCExpresionData expressionRange expressionType returnInfo) checkedLeft checkedRight
typeSynthesizeExpression (NotExpression expressionRange inner) = do
  checkedInner <- typeCheckExpression BoolType inner
  let returnInfo = expressionReturnInfo . getExpressionData $ checkedInner
  return $ NotExpression (TCExpresionData expressionRange BoolType returnInfo) checkedInner
typeSynthesizeExpression (AndExpression expressionRange left right) = do
  checkedLeft <- typeCheckExpression BoolType left
  checkedRight <- typeCheckExpression BoolType right
  let returnInfo = (expressionReturnInfo . getExpressionData $ checkedLeft) `riAnd` (expressionReturnInfo . getExpressionData $ checkedRight)
  return $ AndExpression (TCExpresionData expressionRange BoolType returnInfo) checkedLeft checkedRight
typeSynthesizeExpression (OrExpression expressionRange left right) = do
  checkedLeft <- typeCheckExpression BoolType left
  checkedRight <- typeCheckExpression BoolType right
  let returnInfo = (expressionReturnInfo . getExpressionData $ checkedLeft) `riAnd` (expressionReturnInfo . getExpressionData $ checkedRight)
  return $ OrExpression (TCExpresionData expressionRange BoolType returnInfo) checkedLeft checkedRight
typeSynthesizeExpression (EqualExpression expressionRange left right) = do
  checkedLeft <- typeSynthesizeExpression left
  checkedRight <- typeSynthesizeExpression right
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
typeSynthesizeExpression (NotEqualExpression expressionRange left right) = do
  checkedLeft <- typeSynthesizeExpression left
  checkedRight <- typeSynthesizeExpression right
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
typeSynthesizeExpression (GreaterExpression expressionRange left right) = do
  checkedLeft <- typeSynthesizeExpression left
  checkedRight <- typeSynthesizeExpression right
  expressionType <- case (expressionType . getExpressionData $ checkedLeft, expressionType . getExpressionData $ checkedRight) of
    (IntType, IntType) -> return BoolType
    (FloatType, FloatType) -> return BoolType
    (leftType, rightType) -> throwError $ GreaterExpressionTypeError expressionRange leftType rightType
  let returnInfo = (expressionReturnInfo . getExpressionData $ checkedLeft) `riAnd` (expressionReturnInfo . getExpressionData $ checkedRight)
  return $ GreaterExpression (TCExpresionData expressionRange expressionType returnInfo) checkedLeft checkedRight
typeSynthesizeExpression (LessExpression expressionRange left right) = do
  checkedLeft <- typeSynthesizeExpression left
  checkedRight <- typeSynthesizeExpression right
  expressionType <- case (expressionType . getExpressionData $ checkedLeft, expressionType . getExpressionData $ checkedRight) of
    (IntType, IntType) -> return BoolType
    (FloatType, FloatType) -> return BoolType
    (leftType, rightType) -> throwError $ LessExpressionTypeError expressionRange leftType rightType
  let returnInfo = (expressionReturnInfo . getExpressionData $ checkedLeft) `riAnd` (expressionReturnInfo . getExpressionData $ checkedRight)
  return $ LessExpression (TCExpresionData expressionRange expressionType returnInfo) checkedLeft checkedRight
typeSynthesizeExpression (GreaterEqualExpression expressionRange left right) = do
  checkedLeft <- typeSynthesizeExpression left
  checkedRight <- typeSynthesizeExpression right
  expressionType <- case (expressionType . getExpressionData $ checkedLeft, expressionType . getExpressionData $ checkedRight) of
    (IntType, IntType) -> return BoolType
    (FloatType, FloatType) -> return BoolType
    (leftType, rightType) -> throwError $ GreaterEqualExpressionTypeError expressionRange leftType rightType
  let returnInfo = (expressionReturnInfo . getExpressionData $ checkedLeft) `riAnd` (expressionReturnInfo . getExpressionData $ checkedRight)
  return $ GreaterEqualExpression (TCExpresionData expressionRange expressionType returnInfo) checkedLeft checkedRight
typeSynthesizeExpression (LessEqualExpression expressionRange left right) = do
  checkedLeft <- typeSynthesizeExpression left
  checkedRight <- typeSynthesizeExpression right
  expressionType <- case (expressionType . getExpressionData $ checkedLeft, expressionType . getExpressionData $ checkedRight) of
    (IntType, IntType) -> return BoolType
    (FloatType, FloatType) -> return BoolType
    (leftType, rightType) -> throwError $ LessEqualExpressionTypeError expressionRange leftType rightType
  let returnInfo = (expressionReturnInfo . getExpressionData $ checkedLeft) `riAnd` (expressionReturnInfo . getExpressionData $ checkedRight)
  return $ LessEqualExpression (TCExpresionData expressionRange expressionType returnInfo) checkedLeft checkedRight
typeSynthesizeExpression (IfThenElseExpression expressionRange condition trueBranch falseBranch) = do
  checkedCondition <- typeCheckExpression BoolType condition
  let conditionReturnInfo = expressionReturnInfo . getExpressionData $ checkedCondition
  checkedTrueBranch <- typeSynthesizeExpression trueBranch
  let trueBranchType = expressionType . getExpressionData $ checkedTrueBranch
  let trueBranchReturnInfo = expressionReturnInfo . getExpressionData $ checkedTrueBranch
  checkedFalseBranch <- mapM typeSynthesizeExpression falseBranch
  let maybeFalseBranchType = expressionType . getExpressionData <$> checkedFalseBranch
  let falseBranchReturnInfo = case checkedFalseBranch of
        Just e -> expressionReturnInfo . getExpressionData $ e
        Nothing -> NeverReturns
  expressionType <- case maybeFalseBranchType of
    Nothing ->
      if trueBranchType == NilType
        then return NilType
        else throwError $ IfThenExpressionBranchesTypeError expressionRange trueBranchType
    Just falseBranchType -> do
      combinedBranchType <- typeUnion trueBranchType falseBranchType
      case combinedBranchType of
        Nothing -> throwError $ IfThenElseExpressionBranchesTypeError expressionRange trueBranchType falseBranchType
        Just combinedType -> return combinedType
  let returnInfo = conditionReturnInfo `riAnd` (trueBranchReturnInfo `riOr` falseBranchReturnInfo)
  return $ IfThenElseExpression (TCExpresionData expressionRange expressionType returnInfo) checkedCondition checkedTrueBranch checkedFalseBranch
typeSynthesizeExpression (ScopeExpression expressionRange scope) = do
  checkedScope <- typeCheckScope scope
  let returnInfo = getScopeData checkedScope
  return $ ScopeExpression (TCExpresionData expressionRange NilType returnInfo) checkedScope
typeSynthesizeExpression
  ( FunctionExpression
      expressionRange
      ( FunctionDefinition
          IBFunctionDefinitionData {ibFunctionDefinitionRange, ibFunctionDefinitionCapturedIdentifiers}
          parameters
          (WithTypeAnnotation body returnTypeAnnotation)
        )
    ) = do
    typedParameters <- forM parameters $ \(WithTypeAnnotation parameterName parameterTypeAnnotation) -> case parameterTypeAnnotation of
      Nothing -> throwError $ FunctionMissingParameterTypeAnnotation expressionRange
      Just parameterTypeExpression -> do
        parameterType <- fromTypeExpression parameterTypeExpression
        return (parameterName, parameterType)
    returnType <- case returnTypeAnnotation of
      Nothing -> throwError $ FunctionMissingReturnTypeAnnotation expressionRange
      Just returnTypeExpression -> fromTypeExpression returnTypeExpression
    checkedBody <- typeCheckFunction expressionRange typedParameters body returnType
    let functionType = FunctionType (snd <$> typedParameters) returnType
    let functionDefinitionData =
          TCFunctionDefinitionData
            { tcFunctionDefinitionType = functionType,
              tcFunctionDefinitionRange = ibFunctionDefinitionRange,
              tcFunctionDefinitionCapturedIdentifiers = ibFunctionDefinitionCapturedIdentifiers
            }
    let checkedFunctionDefinition =
          FunctionDefinition
            functionDefinitionData
            (typedParameters <&> \(parameterName, _) -> WithTypeAnnotation parameterName ())
            (WithTypeAnnotation checkedBody ())
    return $ FunctionExpression (TCExpresionData expressionRange functionType NeverReturns) checkedFunctionDefinition
typeSynthesizeExpression (FunctionCallExpression expressionRange function arguments) = do
  checkedFunction <- typeSynthesizeExpression function
  (parameterTypes, returnType) <- case expressionType . getExpressionData $ checkedFunction of
    FunctionType parameterTypes returnType -> return (parameterTypes, returnType)
    unexpectedType -> throwError $ FunctionCallExpressionNotAFunctionTypeError expressionRange unexpectedType
  unless (Seq.length parameterTypes == Seq.length arguments) $
    throwError (FunctionCallExpressionArityError expressionRange (Seq.length parameterTypes) (Seq.length arguments))
  checkedArguments <- forM (Seq.zip parameterTypes arguments) (uncurry typeCheckExpression)
  let returnInfo = (expressionReturnInfo . getExpressionData $ checkedFunction) `riAnd` (fold $ expressionReturnInfo . getExpressionData <$> checkedArguments)
  return $ FunctionCallExpression (TCExpresionData expressionRange returnType returnInfo) checkedFunction checkedArguments
typeSynthesizeExpression (RecordExpression expressionRange mutability recordName typeArgumentExpressions fieldValueMap) = do
  expectedNumArguments <- getRecordNumTypeParameters recordName
  unless (Seq.length typeArgumentExpressions == expectedNumArguments) $
    throwError (RecordExpresssionNumTypeArgumentsError expressionRange (getTextName recordName) expectedNumArguments (Seq.length typeArgumentExpressions))
  typeArguments <- mapM fromTypeExpression typeArgumentExpressions
  fieldTypeMap <- getRecordFieldTypes expressionRange recordName mutability typeArguments
  checkedFieldValues <- forM (Map.toList fieldTypeMap) $ \(fieldName, fieldType) -> case Map.lookup fieldName fieldValueMap of
    Nothing -> throwError $ RecordExpressionMissingFieldError (getTextName recordName) fieldName expressionRange
    Just fieldValue -> do
      checkedFieldValue <- typeCheckExpression fieldType fieldValue
      return (fieldName, checkedFieldValue)
  forM_ (Map.keys fieldValueMap) $ \fieldName -> case Map.lookup fieldName fieldTypeMap of
    Nothing -> throwError $ RecordExpressionExtraFieldError (getTextName recordName) fieldName expressionRange
    Just _ -> return ()
  let checkedFieldValueMap = Map.fromList checkedFieldValues
  let returnInfo = fold $ expressionReturnInfo . getExpressionData <$> checkedFieldValueMap
  return $ RecordExpression (TCExpresionData expressionRange (RecordUnionType mutability (Map.singleton recordName typeArguments)) returnInfo) mutability recordName () checkedFieldValueMap
typeSynthesizeExpression (FieldAccessExpression expressionRange inner fieldName) = do
  checkedInner <- typeSynthesizeExpression inner
  let innerExpressionType = expressionType . getExpressionData $ checkedInner
  (innerExpressionMutability, innerExpressionRecordNames) <- case innerExpressionType of
    RecordUnionType mutability recordNames -> return (mutability, recordNames)
    otherType -> throwError $ AccessedFieldOfNonRecordValueError otherType expressionRange
  recordFieldTypePairs <- forM (Map.toList innerExpressionRecordNames) $ \(recordName, recordTypeArguments) -> do
    fieldTypeMap <- getRecordFieldTypes expressionRange recordName innerExpressionMutability recordTypeArguments
    case Map.lookup fieldName fieldTypeMap of
      Nothing -> throwError $ AccessedFieldNotInRecordError (getTextName recordName) fieldName innerExpressionType expressionRange
      Just fieldType -> return (recordName, fieldType)
  combinedFieldType <- typeUnionF . NonEmpty.fromList $ snd <$> recordFieldTypePairs
  expressionType <- case combinedFieldType of
    Nothing -> throwError $ FieldTypesAreNotCompatibleError fieldName (first getTextName <$> recordFieldTypePairs) expressionRange
    Just expressionType -> return expressionType
  let returnInfo = expressionReturnInfo . getExpressionData $ checkedInner
  return $ FieldAccessExpression (TCExpresionData expressionRange expressionType returnInfo) checkedInner fieldName
typeSynthesizeExpression (CaseExpression expressionRange switch cases) = do
  checkedSwitch <- typeSynthesizeExpression switch
  let switchType = expressionType . getExpressionData $ checkedSwitch
  (switchMutability, switchRecordMap) <- case switchType of
    RecordUnionType mutability recordNames -> return (mutability, recordNames)
    nonRecordType -> throwError $ CaseSwitchHasNonRecordTypeError (getRange switch) nonRecordType
  forM_ (Map.keys switchRecordMap) $ \recordName ->
    unless (Map.member recordName cases) $
      throwError (CaseExpressionMisingCaseError expressionRange (getTextName recordName))
  forM_ (Map.keys cases) $ \recordName ->
    unless
      (Map.member recordName switchRecordMap)
      $ throwError (CaseExpressionExtraneousCaseError expressionRange (getTextName recordName))
  forM_ (Map.toList cases) $ \(recordName, (caseParameter, _)) ->
    setValueIdentifierType caseParameter (RecordUnionType switchMutability $ Map.singleton recordName (switchRecordMap ! recordName))
  checkedCases <- mapM (secondM typeSynthesizeExpression) cases
  let caseRecordTypePairs = second (expressionType . getExpressionData . snd) <$> Map.toList checkedCases
  combinedCaseType <- typeUnionF . NonEmpty.fromList $ snd <$> caseRecordTypePairs
  expressionType <- case combinedCaseType of
    Nothing -> throwError $ CaseTypesAreNotCompatibleError (first getTextName <$> caseRecordTypePairs) expressionRange
    Just expressionType -> return expressionType
  let returnInfo = (expressionReturnInfo . getExpressionData $ checkedSwitch) `riAnd` foldl1 riOr (expressionReturnInfo . getExpressionData . snd <$> Map.elems checkedCases)
  return $ CaseExpression (TCExpresionData expressionRange expressionType returnInfo) checkedSwitch checkedCases
typeSynthesizeExpression (ListExpression expressionRange mutability typeArgumentExpressions values) = do
  typeArguments <- mapM fromTypeExpression typeArgumentExpressions
  (valueType, checkedValues) <- case typeArguments of
    Empty -> do
      when (Seq.length values == 0) $
        throwError (ListValueTypeInferenceError expressionRange)
      checkedValues <- forM values typeSynthesizeExpression
      let valueTypes = expressionType . getExpressionData <$> checkedValues
      maybeCombinedValueType <- typeUnionF . NonEmpty.fromList . toList $ valueTypes
      combinedValueType <- liftWithErrors . expectJust (ListValueTypeInferenceError expressionRange) $ maybeCombinedValueType
      return (combinedValueType, checkedValues)
    valueType :<| Empty -> do
      checkedValues <- forM values $ typeCheckExpression valueType
      return (valueType, checkedValues)
    _ -> throwError $ ListWrongNumberOfTypeArgumentsError expressionRange (Seq.length typeArguments)
  let expressionType = ListType mutability valueType
  let returnInfo = foldMap (expressionReturnInfo . getExpressionData) checkedValues
  return $ ListExpression (TCExpresionData expressionRange expressionType returnInfo) mutability () checkedValues
typeSynthesizeExpression (IndexExpression expressionRange innerExpression indexExpression) = do
  checkedInnerExpression <- typeSynthesizeExpression innerExpression
  checkedIndexExpression <- typeCheckExpression IntType indexExpression
  expressionType <- case expressionType . getExpressionData $ checkedInnerExpression of
    ListType _ valueType -> return valueType
    nonListType -> throwError $ IndexTypeError expressionRange nonListType
  let returnInfo =
        (expressionReturnInfo . getExpressionData $ checkedInnerExpression)
          `riAnd` (expressionReturnInfo . getExpressionData $ checkedIndexExpression)
  return $ IndexExpression (TCExpresionData expressionRange expressionType returnInfo) checkedInnerExpression checkedIndexExpression