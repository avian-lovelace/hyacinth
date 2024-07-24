module IdentifierBinding.IdentifierBinder
  ( runIdentifierBinding,
  )
where

import Control.Monad (foldM, forM, unless)
import Core.ErrorState
import Core.Errors
import Core.FilePositions
import Core.SyntaxTree
import Core.Utils (secondM)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import IdentifierBinding.IdentifierBinding
import IdentifierBinding.SyntaxTree
import Parsing.SyntaxTree

runIdentifierBinding :: PModule -> WithErrors (Int, Int, IBModule)
runIdentifierBinding m = (boundValueIdentifierCounter,boundFunctionIdentifierCounter,) <$> bindingResult
  where
    (IdentifierBindingState {boundValueIdentifierCounter, boundFunctionIdentifierCounter}, bindingResult) =
      runErrorState (moduleBinder m) initialBindingState

moduleBinder :: PModule -> IdentifierBinder IBModule
moduleBinder (Module _ (MainFunction _ scope)) = withNewExpressionScope $ do
  boundScope <- bindScope scope
  return $ Module () (MainFunction () boundScope)

{- We preemtively add declared variables to the current scope in a before declaration state. This lets us catch and throw
  errors in situations where a variable is used in a scope before it is later shadowed. This could be allowed, but it is
  confusing enough that it's probably worth just giving a compilation error.

  Code example:
  let x = 1;
  {
    print x;
    let x = 2;
  };

  BindingReady is just a wrapper that shows that we've completed this pre-binding step.
  -}
newtype BindingReady a = BindingReady a

prepareNonPositionalStatementForBinding :: PNonPositionalStatement -> IdentifierBinder (BindingReady PNonPositionalStatement)
prepareNonPositionalStatementForBinding statement = case statement of
  (FunctionStatement statementRange unboundFunctionName _ _) -> do
    addFunction statementRange unboundFunctionName
    return $ BindingReady statement
  (RecordStatement statementRange recordName _ _ _) -> do
    addRecord statementRange recordName
    return $ BindingReady statement
  (TypeStatement statementRange typeSynonym _ _ _) -> do
    addTypeSynonym statementRange typeSynonym
    return $ BindingReady statement

nonPositionalStatementBinder :: BindingReady PNonPositionalStatement -> IdentifierBinder IBNonPositionalStatement
nonPositionalStatementBinder (BindingReady (FunctionStatement statementRange unboundFunctionName typeParameters (FunctionDefinition definitionRange parameters (WithTypeAnnotation body returnTypeAnnotation)))) = do
  boundFunctionName <- getFunctionNameBinding unboundFunctionName
  withNewFunctionScope $ do
    boundTypeParameters <- mapM (addFunctionTypeParameter statementRange unboundFunctionName) typeParameters
    boundParameters <- traverse' (bindParameter statementRange) parameters
    boundReturnTypeAnnotation <- mapM typeExpressionBinder returnTypeAnnotation
    boundBody <- expressionBinder body
    capturedIdentifiers <- getCapturedIdentifiers
    let functionDefinitionData = IBFunctionDefinitionData {ibFunctionDefinitionRange = definitionRange, ibFunctionDefinitionCapturedIdentifiers = capturedIdentifiers}
    let functionDefinition = FunctionDefinition functionDefinitionData boundParameters (WithTypeAnnotation boundBody boundReturnTypeAnnotation)
    return $ FunctionStatement statementRange boundFunctionName boundTypeParameters functionDefinition
nonPositionalStatementBinder (BindingReady (RecordStatement statementRange recordName mutabiltyParameter typeParameters fieldTypePairs)) = do
  boundRecordName <- getRecordNameBinding recordName
  (boundMutabilityParameter, (boundTypeParameters, boundFieldTypePairs)) <- withNewRecordScope statementRange mutabiltyParameter $ do
    boundTypeParameters <- mapM (addRecordTypeParameter statementRange recordName) typeParameters
    boundFieldTypePairs <- mapM (secondM typeExpressionBinder) fieldTypePairs
    return (boundTypeParameters, boundFieldTypePairs)
  return $ RecordStatement statementRange boundRecordName boundMutabilityParameter boundTypeParameters boundFieldTypePairs
nonPositionalStatementBinder (BindingReady (TypeStatement statementRange typeSynonym maybeMutabilityParameter typeParameters typeValue)) = do
  boundTypeSynonym <- getTypeSynonymBinding typeSynonym
  (boundMutabilityParameter, boundTypeParameters, boundTypeValue) <- withNewTypeSynonymScope $ do
    boundMutabilityParameter <- forM maybeMutabilityParameter $ addTypeSynonymMutabilityParameter statementRange
    boundTypeParameters <- forM typeParameters $ addTypeSynonymTypeParameter statementRange typeSynonym
    boundTypeValue <- typeExpressionBinder typeValue
    return (boundMutabilityParameter, boundTypeParameters, boundTypeValue)
  return $ TypeStatement statementRange boundTypeSynonym boundMutabilityParameter boundTypeParameters boundTypeValue

prepareStatementForBinding :: PStatement -> IdentifierBinder (BindingReady PStatement)
prepareStatementForBinding statement = case statement of
  (VariableDeclarationStatement statementRange mutability (WithTypeAnnotation unboundVariableName _) _) -> do
    addVariable statementRange mutability unboundVariableName
    return $ BindingReady statement
  _ -> return $ BindingReady statement

statementBinder :: BindingReady PStatement -> IdentifierBinder IBStatement
statementBinder (BindingReady (VariableDeclarationStatement declarationRange mutability (WithTypeAnnotation unboundVariableName typeAnnotation) expression)) =
  do
    boundVariableName <- setVariableUsability unboundVariableName InDeclaration
    boundExpression <- expressionBinder expression
    boundTypeAnnotation <- mapM typeExpressionBinder typeAnnotation
    return $ VariableDeclarationStatement declarationRange mutability (WithTypeAnnotation boundVariableName boundTypeAnnotation) boundExpression
    `andFinally` setVariableUsability unboundVariableName Usable
statementBinder (BindingReady (VariableMutationStatement statementRange unboundVariableName expression)) = do
  identifierInfo <- getIdentifierBinding statementRange unboundVariableName
  boundValueIdentifier <- case identifierInfo of
    BuiltInFunctionInfo _ ->
      throwError $ MutatedNonVariableIdentifierError unboundVariableName (getIdentifierInfoTextName identifierInfo) Nothing statementRange
    VariableIdentifierInfo declarationRange boundValueIdentifier mutability usability -> do
      case usability of
        BeforeDeclaration -> throwError $ VariableDefinedAfterReferenceError unboundVariableName statementRange declarationRange
        InDeclaration -> throwError $ VariableReferencedInDeclarationError unboundVariableName declarationRange statementRange
        Usable -> return ()
      case mutability of
        Immutable -> throwError $ MutatedImmutableVariableError unboundVariableName declarationRange statementRange
        Mutable -> return ()
      return boundValueIdentifier
    _ ->
      throwError $ MutatedNonVariableIdentifierError unboundVariableName (getIdentifierInfoTextName identifierInfo) (tryGetRange identifierInfo) statementRange
  boundExpression <- expressionBinder expression
  return $ VariableMutationStatement statementRange boundValueIdentifier boundExpression
-- Standard cases
statementBinder (BindingReady (FieldMutationStatement statementRange record field value)) = do
  boundRecord <- expressionBinder record
  boundValue <- expressionBinder value
  return $ FieldMutationStatement statementRange boundRecord field boundValue
statementBinder (BindingReady (ExpressionStatement range expression)) = do
  boundExpression <- expressionBinder expression
  return $ ExpressionStatement range boundExpression
statementBinder (BindingReady (WhileLoopStatement range condition body)) = do
  boundCondition <- expressionBinder condition
  boundBody <- expressionBinder body
  return $ WhileLoopStatement range boundCondition boundBody
statementBinder (BindingReady (ReturnStatement range (Just expression))) = do
  boundExpression <- expressionBinder expression
  return $ ReturnStatement range (Just boundExpression)
statementBinder (BindingReady (ReturnStatement range Nothing)) =
  return $ ReturnStatement range Nothing

expressionBinder :: PExpression -> IdentifierBinder IBExpression
expressionBinder (IdentifierExpression expressionRange (PIdentifier unboundIdentifier typeArguments)) = do
  identifierInfo <- getIdentifierBinding expressionRange unboundIdentifier
  case identifierInfo of
    ParameterIdentifierInfo _ boundValueIdentifier -> do
      unless (null typeArguments) $ throwError (TypeArgumentsAppliedToValueIdentifierError expressionRange unboundIdentifier)
      return $ IdentifierExpression expressionRange $ SimpleValueIdentifier boundValueIdentifier
    FunctionIdentifierInfo _ boundFunctionIdentifier -> do
      boundTypeArguments <- mapM typeExpressionBinder typeArguments
      return $ IdentifierExpression expressionRange $ FunctionValueIdentifier (Left boundFunctionIdentifier) boundTypeArguments
    BuiltInFunctionInfo builtInFunction -> do
      boundTypeArguments <- mapM typeExpressionBinder typeArguments
      return $ IdentifierExpression expressionRange $ FunctionValueIdentifier (Right builtInFunction) boundTypeArguments
    {- A record with no fields gets interpreted as a mutable, rather than immutable record. The mutability of a record
      with no fields doesn't matter directly. However, if the record ends up as part of a record union, being immutable
      would force the whole record union to be immutable. So, we make it mutable to improve flexibility.
    -}
    RecordIdentifierInfo _ boundRecordIdentifier -> return $ RecordExpression expressionRange Mutable boundRecordIdentifier Empty Map.empty
    VariableIdentifierInfo declarationRange boundValueIdentifier _ usability -> case usability of
      BeforeDeclaration -> throwError $ VariableDefinedAfterReferenceError unboundIdentifier expressionRange declarationRange
      InDeclaration -> throwError $ VariableReferencedInDeclarationError unboundIdentifier declarationRange expressionRange
      Usable -> do
        unless (null typeArguments) $ throwError (TypeArgumentsAppliedToValueIdentifierError expressionRange unboundIdentifier)
        return $ IdentifierExpression expressionRange $ SimpleValueIdentifier boundValueIdentifier
    CaseParameterInfo _ boundValueIdentifier -> do
      unless (null typeArguments) $ throwError (TypeArgumentsAppliedToValueIdentifierError expressionRange unboundIdentifier)
      return $ IdentifierExpression expressionRange $ SimpleValueIdentifier boundValueIdentifier
    TypeParameterInfo definitionRange _ ->
      throwError $ NonValueIdentifierUsedAsValueError unboundIdentifier (getIdentifierInfoTextName identifierInfo) expressionRange definitionRange
    MutabilityParameterInfo definitionRange _ ->
      throwError $ NonValueIdentifierUsedAsValueError unboundIdentifier (getIdentifierInfoTextName identifierInfo) expressionRange definitionRange
    TypeSynonymInfo definitionRange _ ->
      throwError $ NonValueIdentifierUsedAsValueError unboundIdentifier (getIdentifierInfoTextName identifierInfo) expressionRange definitionRange
expressionBinder (ScopeExpression d scope) = withNewExpressionScope $ do
  boundScope <- bindScope scope
  return $ ScopeExpression d boundScope
expressionBinder (FunctionExpression expressionRange (FunctionDefinition definitionRange parameters (WithTypeAnnotation body returnTypeAnnotation))) = withNewFunctionScope $ do
  boundParameters <- traverse' (bindParameter expressionRange) parameters
  boundReturnTypeAnnotation <- mapM typeExpressionBinder returnTypeAnnotation
  boundBody <- expressionBinder body
  capturedIdentifiers <- getCapturedIdentifiers
  let functionDefinitionData = IBFunctionDefinitionData {ibFunctionDefinitionRange = definitionRange, ibFunctionDefinitionCapturedIdentifiers = capturedIdentifiers}
  let functionDefinition = FunctionDefinition functionDefinitionData boundParameters (WithTypeAnnotation boundBody boundReturnTypeAnnotation)
  return $ FunctionExpression expressionRange functionDefinition
expressionBinder (RecordExpression expressionRange mutability recordName typeArguments fieldValueMap) = do
  identifierInfo <- getIdentifierBinding expressionRange recordName
  boundRecordName <- case identifierInfo of
    RecordIdentifierInfo _ boundRecordIdentifier -> return boundRecordIdentifier
    valueIdentifier -> throwError $ IdentifierUsedAsRecordNameError recordName (tryGetRange valueIdentifier) expressionRange
  boundTypeArguments <- mapM typeExpressionBinder typeArguments
  boundFieldValueMap <- mapM expressionBinder fieldValueMap
  return $ RecordExpression expressionRange mutability boundRecordName boundTypeArguments boundFieldValueMap
expressionBinder (CaseExpression expressionRange switch caseList) = do
  boundSwitch <- expressionBinder switch
  boundCaseList <- mapM (bindCase expressionRange) caseList
  boundCaseMap <- foldM addCase Map.empty boundCaseList
  return $ CaseExpression expressionRange boundSwitch boundCaseMap
  where
    bindCase :: Range -> (PRecordIdentifier, PValueIdentifier, PExpression) -> IdentifierBinder (IBRecordIdentifier, IBValueIdentifier, IBExpression)
    bindCase caseExpressionRange (recordName, caseParameter, caseValue) = do
      identifierInfo <- getIdentifierBinding caseExpressionRange recordName
      boundRecordName <- case identifierInfo of
        RecordIdentifierInfo _ boundRecordName -> return boundRecordName
        valueIdentifier -> throwError $ ValueIdentifierUsedAsCaseError recordName (tryGetRange valueIdentifier) caseExpressionRange
      (boundCaseParameter, boundCaseValue) <- withNewCaseScope caseExpressionRange caseParameter $ expressionBinder caseValue
      return (boundRecordName, boundCaseParameter, boundCaseValue)
    addCase ::
      Map IBRecordIdentifier (IBValueIdentifier, IBExpression) ->
      (IBRecordIdentifier, IBValueIdentifier, IBExpression) ->
      IdentifierBinder (Map IBRecordIdentifier (IBValueIdentifier, IBExpression))
    addCase caseMap (recordName, switchValueName, caseValue) = do
      let (maybeConflictingValue, updatedCaseMap) = Map.insertLookupWithKey (\_ a _ -> a) recordName (switchValueName, caseValue) caseMap
      case maybeConflictingValue of
        Just conflictingValue ->
          throwError $ CaseExpressionDuplicatedCasesError (getTextName recordName) (getRange caseValue) (getRange . snd $ conflictingValue)
        Nothing -> return updatedCaseMap
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
expressionBinder (FieldAccessExpression d inner field) = do
  boundInner <- expressionBinder inner
  return $ FieldAccessExpression d boundInner field

bindScope :: PScope -> IdentifierBinder IBScope
bindScope (Scope () nonPositionalStatements statements) = do
  readyNonPositionalStatements <- traverse prepareNonPositionalStatementForBinding nonPositionalStatements
  readyStatements <- traverse prepareStatementForBinding statements
  boundStatements <- traverse statementBinder readyStatements
  boundNonPositionalStatements <- traverse nonPositionalStatementBinder readyNonPositionalStatements
  return $ Scope () boundNonPositionalStatements boundStatements

bindParameter :: Range -> PWithTypeAnnotation PValueIdentifier -> IdentifierBinder (IBWithTypeAnnotation IBValueIdentifier)
bindParameter definitionRange (WithTypeAnnotation unboundParameter typeAnnotation) = do
  boundParameter <- addParameter definitionRange unboundParameter
  boundTypeAnnotation <- mapM typeExpressionBinder typeAnnotation
  return $ WithTypeAnnotation boundParameter boundTypeAnnotation

typeExpressionBinder :: PTypeExpression -> IdentifierBinder IBTypeExpression
typeExpressionBinder (IntTypeExpression typeExpressionRange) = return $ IntTypeExpression typeExpressionRange
typeExpressionBinder (FloatTypeExpression typeExpressionRange) = return $ FloatTypeExpression typeExpressionRange
typeExpressionBinder (CharTypeExpression typeExpressionRange) = return $ CharTypeExpression typeExpressionRange
typeExpressionBinder (StringTypeExpression typeExpressionRange) = return $ StringTypeExpression typeExpressionRange
typeExpressionBinder (BoolTypeExpression typeExpressionRange) = return $ BoolTypeExpression typeExpressionRange
typeExpressionBinder (NilTypeExpression typeExpressionRange) = return $ NilTypeExpression typeExpressionRange
typeExpressionBinder (FunctionTypeExpression typeExpressionRange parameterTypes returnType) = do
  boundParameterTypes <- mapM typeExpressionBinder parameterTypes
  boundReturnType <- typeExpressionBinder returnType
  return $ FunctionTypeExpression typeExpressionRange boundParameterTypes boundReturnType
typeExpressionBinder (IdentifierTypeExpression typeExpressionRange mutability typeIdentifier typeArguments) = do
  boundMutability <- case mutability of
    Left m -> return $ Left m
    Right mutabilityIdentifier -> do
      identifierInfo <- getIdentifierBinding typeExpressionRange mutabilityIdentifier
      case identifierInfo of
        MutabilityParameterInfo _ boundMutabilityIdentifier -> return $ Right boundMutabilityIdentifier
        nonMutabilityIdentifier -> throwError $ IdentifierUsedAsMutabilityParameterError mutabilityIdentifier (tryGetRange nonMutabilityIdentifier) typeExpressionRange
  boundTypeArguments <- mapM typeExpressionBinder typeArguments
  identifierInfo <- getIdentifierBinding typeExpressionRange typeIdentifier
  case identifierInfo of
    TypeParameterInfo _ boundTypeParameter -> return $ IdentifierTypeExpression typeExpressionRange boundMutability (Left boundTypeParameter) boundTypeArguments
    TypeSynonymInfo _ boundTypeSynonym ->
      return $ IdentifierTypeExpression typeExpressionRange boundMutability (Right boundTypeSynonym) boundTypeArguments
    RecordIdentifierInfo _ boundRecordName ->
      return $ RecordUnionTypeExpression typeExpressionRange boundMutability (Seq.singleton (boundRecordName, boundTypeArguments))
    valueIdentifier -> throwError $ ValueIdentifierUsedAsTypeError typeIdentifier (tryGetRange valueIdentifier) typeExpressionRange
typeExpressionBinder (RecordUnionTypeExpression typeExpressionRange mutability records) = do
  boundMutability <- case mutability of
    Left m -> return $ Left m
    Right mutabilityIdentifier -> do
      identifierInfo <- getIdentifierBinding typeExpressionRange mutabilityIdentifier
      case identifierInfo of
        MutabilityParameterInfo _ boundMutabilityIdentifier -> return $ Right boundMutabilityIdentifier
        nonMutabilityIdentifier -> throwError $ IdentifierUsedAsMutabilityParameterError mutabilityIdentifier (tryGetRange nonMutabilityIdentifier) typeExpressionRange
  boundRecords <- forM records $ \(recordName, recordTypeArguments) -> do
    boundRecordName <- do
      identifierInfo <- getIdentifierBinding typeExpressionRange recordName
      case identifierInfo of
        RecordIdentifierInfo _ boundRecordName -> return boundRecordName
        nonRecordIdentifier -> throwError $ IdentifierUsedAsRecordNameError recordName (tryGetRange nonRecordIdentifier) typeExpressionRange
    boundRecordTypeArguments <- mapM typeExpressionBinder recordTypeArguments
    return (boundRecordName, boundRecordTypeArguments)
  return $ RecordUnionTypeExpression typeExpressionRange boundMutability boundRecords