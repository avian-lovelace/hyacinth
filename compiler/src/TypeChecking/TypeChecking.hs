module TypeChecking.TypeChecking
  ( TypeChecker,
    TypeCheckingState,
    FunctionContext (FunctionContext, contextReturnType),
    Variance (Covariant, Contravariant, Invariant),
    MutabilityContext (NormalContext),
    VarianceInfo,
    recordContext,
    setValueIdentifierType,
    getValueIdentifierType,
    getFunctionContext,
    initialTypeCheckingState,
    setFunctionTypeInfo,
    getFunctionType,
    setRecordTypeInfo,
    getRecordFieldTypes,
    withFunctionContext,
    getRecordFieldOrders,
    isCompatibleWith,
    typeUnion,
    typeUnionF,
    typeIntersection,
    typeIntersectionF,
    getRecordNumTypeParameters,
    fromTypeExpression,
    getParametrizedTypeFunc,
    initializeTypeSynonymTypeInfo,
    initialVarianceFunc,
    getTypeSynonymTypeInfo,
    calculateVariances,
  )
where

import Control.Monad (foldM, forM_, unless)
import Core.ErrorState
import Core.Errors
import Core.FilePositions
import Core.SyntaxTree
import Core.Utils
import Data.Bifunctor (Bifunctor (bimap))
import Data.Foldable (Foldable (fold), toList)
import Data.Foldable1 (Foldable1, foldlM1)
import Data.Functor (($>), (<&>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Traversable (forM)
import IdentifierBinding.SyntaxTree
import Parsing.SyntaxTree
import TypeChecking.Type
import TypeChecking.Variance

data TypeCheckingState = TypeCheckingState
  { valueIdentifierTypes :: Map BoundValueIdentifier Type,
    functionTypeInfos :: Map BoundFunctionIdentifier FunctionTypeInfo,
    recordTypeInfos :: Map BoundRecordIdentifier RecordTypeInfo,
    typeSynonymTypeInfos ::
      Map
        BoundTypeSynonym
        ( Either
            (Maybe IBMutabilityParameter, Seq IBTypeParameter, IBTypeExpression)
            TypeSynonymTypeInfo
        ),
    functionContextStack :: [FunctionContext],
    inProgressTypeSynonyms :: Seq BoundTypeSynonym
  }

data FunctionTypeInfo = FunctionTypeInfo
  { functionTypeArity :: Int,
    functionTypeFunc :: Seq Type -> Type
  }

data TypeSynonymTypeInfo = TypeSynonymTypeInfo
  { typeSynonymArity :: Int,
    typeSynonymTypeFunc :: Mutability -> Seq Type -> Type,
    typeSynonymVarianceFunc :: Mutability -> Seq Variance
  }

data RecordTypeInfo = RecordTypeInfo
  { recordArity :: Int,
    recordFieldTypesFunc :: Mutability -> Seq Type -> Map UnboundIdentifier Type,
    recordVarianceFunc :: Mutability -> Seq Variance,
    recordFieldOrder :: Seq UnboundIdentifier
  }

newtype FunctionContext = FunctionContext {contextReturnType :: Type}

data MutabilityContext
  = RecordContext (Maybe IBMutabilityParameter) Bool
  | NormalContext (Maybe IBMutabilityParameter)

type TypeChecker = ErrorState TypeCheckingState

initialTypeCheckingState :: TypeCheckingState
initialTypeCheckingState =
  TypeCheckingState
    { valueIdentifierTypes = Map.empty,
      functionTypeInfos = Map.empty,
      recordTypeInfos = Map.empty,
      typeSynonymTypeInfos = Map.empty,
      functionContextStack = [],
      inProgressTypeSynonyms = []
    }

-- Identifier info getters/setters

setValueIdentifierType :: BoundValueIdentifier -> Type -> TypeChecker ()
setValueIdentifierType identifier identifierType = do
  state <- getState
  let updatedValueIdentifierTypes = Map.insert identifier identifierType $ valueIdentifierTypes state
  setState state {valueIdentifierTypes = updatedValueIdentifierTypes}

getValueIdentifierType :: BoundValueIdentifier -> TypeChecker Type
getValueIdentifierType identifier = do
  valueIdentifierTypes <- valueIdentifierTypes <$> getState
  case Map.lookup identifier valueIdentifierTypes of
    Just identifierType -> return identifierType
    Nothing -> do
      throwError $ ShouldNotGetHereError "Called getValueIdentifierType before identifier was initialized"

setFunctionTypeInfo :: BoundFunctionIdentifier -> Int -> (Seq Type -> Type) -> TypeChecker ()
setFunctionTypeInfo functionName functionTypeArity functionTypeFunc = do
  state <- getState
  let updatedFunctionTypeInfo = Map.insert functionName FunctionTypeInfo {functionTypeArity, functionTypeFunc} $ functionTypeInfos state
  setState state {functionTypeInfos = updatedFunctionTypeInfo}

getFunctionType :: Range -> Either BoundFunctionIdentifier BuiltInFunction -> Seq Type -> TypeChecker Type
getFunctionType usageRange (Left functionName) typeArguments = do
  functionTypes <- functionTypeInfos <$> getState
  case Map.lookup functionName functionTypes of
    Just FunctionTypeInfo {functionTypeArity, functionTypeFunc} -> do
      unless (Seq.length typeArguments == functionTypeArity) $
        throwError (FunctionWrongNumberOfTypeArgumentsError usageRange (getTextName functionName) functionTypeArity (Seq.length typeArguments))
      return $ functionTypeFunc typeArguments
    Nothing -> throwError $ ShouldNotGetHereError "Called getFunctionType before function was initialized"
getFunctionType usageRange (Right builtInFunction) typeArguments = do
  let functionTypeArity = case builtInFunction of
        PrintFunction -> 1
        PrintLineFunction -> 1
        ReadLineFunction -> 0
  unless (Seq.length typeArguments == functionTypeArity) $
    throwError (FunctionWrongNumberOfTypeArgumentsError usageRange (getTextName builtInFunction) functionTypeArity (Seq.length typeArguments))
  return $ case builtInFunction of
    PrintFunction -> FunctionType (Seq.singleton $ typeArguments `Seq.index` 0) NilType
    PrintLineFunction -> FunctionType (Seq.singleton $ typeArguments `Seq.index` 0) NilType
    ReadLineFunction -> FunctionType Empty StringType

setRecordTypeInfo ::
  BoundRecordIdentifier ->
  Int ->
  (Mutability -> Seq Type -> Map UnboundIdentifier Type) ->
  (Mutability -> Seq Variance) ->
  Seq UnboundIdentifier ->
  TypeChecker ()
setRecordTypeInfo recordName recordArity recordFieldTypesFunc recordVarianceFunc recordFieldOrder = do
  state <- getState
  let newInfo = RecordTypeInfo {recordArity, recordFieldTypesFunc, recordVarianceFunc, recordFieldOrder}
  let updatedRecordTypeInfo = Map.insert recordName newInfo $ recordTypeInfos state
  setState $ state {recordTypeInfos = updatedRecordTypeInfo}

setRecordVariancesFunc :: BoundRecordIdentifier -> (Mutability -> Seq Variance) -> TypeChecker ()
setRecordVariancesFunc recordName recordVarianceFunc = do
  state <- getState
  let currentRecordTypeInfos = recordTypeInfos state
  let updatedRecordTypeInfo = (currentRecordTypeInfos Map.! recordName) {recordVarianceFunc}
  let updatedRecordTypeInfos = Map.insert recordName updatedRecordTypeInfo $ currentRecordTypeInfos
  setState $ state {recordTypeInfos = updatedRecordTypeInfos}

getRecordTypeInfo :: BoundRecordIdentifier -> TypeChecker RecordTypeInfo
getRecordTypeInfo recordName = do
  recordTypeInfos <- recordTypeInfos <$> getState
  case Map.lookup recordName recordTypeInfos of
    Nothing -> throwError $ ShouldNotGetHereError "Called getRecordTypeInfo before record was initialized"
    Just recordTypeInfo -> return recordTypeInfo

getRecordFieldTypes :: Range -> BoundRecordIdentifier -> Mutability -> Seq Type -> TypeChecker (Map UnboundIdentifier Type)
getRecordFieldTypes usageRange recordName mutability typeArguments = do
  RecordTypeInfo {recordArity, recordFieldTypesFunc} <- getRecordTypeInfo recordName
  unless (Seq.length typeArguments == recordArity) $
    throwError (RecordExpresssionNumTypeArgumentsError usageRange (getTextName recordName) recordArity (Seq.length typeArguments))
  return $ recordFieldTypesFunc mutability typeArguments

getRecordVariances :: BoundRecordIdentifier -> Mutability -> TypeChecker (Seq Variance)
getRecordVariances recordName mutability = do
  RecordTypeInfo {recordVarianceFunc} <- getRecordTypeInfo recordName
  return $ recordVarianceFunc mutability

getRecordFieldOrders :: TypeCheckingState -> Map BoundRecordIdentifier (Seq UnboundIdentifier)
getRecordFieldOrders = (recordFieldOrder <$>) . recordTypeInfos

getRecordNumTypeParameters :: BoundRecordIdentifier -> TypeChecker Int
getRecordNumTypeParameters recordName = Seq.length <$> getRecordVariances recordName Mutable

initializeTypeSynonymTypeInfo :: IBTypeSynonym -> Maybe IBMutabilityParameter -> Seq IBTypeParameter -> IBTypeExpression -> TypeChecker ()
initializeTypeSynonymTypeInfo typeSynonym maybeMutabilityParameter typeParameters typeValueExpression = do
  state <- getState
  let updatedTypeSynonymTypeInfo = Map.insert typeSynonym (Left (maybeMutabilityParameter, typeParameters, typeValueExpression)) $ typeSynonymTypeInfos state
  setState state {typeSynonymTypeInfos = updatedTypeSynonymTypeInfo}

setTypeSynonymTypeInfo :: BoundTypeSynonym -> TypeSynonymTypeInfo -> TypeChecker ()
setTypeSynonymTypeInfo typeSynonym typeSynonymTypeInfo = do
  state <- getState
  let updatedTypeSynonymTypeInfos = Map.insert typeSynonym (Right typeSynonymTypeInfo) $ typeSynonymTypeInfos state
  setState state {typeSynonymTypeInfos = updatedTypeSynonymTypeInfos}

setTypeSynonymVariancesFunc :: BoundTypeSynonym -> (Mutability -> Seq Variance) -> TypeChecker ()
setTypeSynonymVariancesFunc typeSynonym typeSynonymVarianceFunc = do
  currentTypeSynonymTypeInfo <- getTypeSynonymTypeInfo typeSynonym
  setTypeSynonymTypeInfo typeSynonym currentTypeSynonymTypeInfo {typeSynonymVarianceFunc}

getTypeSynonymTypeInfo :: BoundTypeSynonym -> TypeChecker TypeSynonymTypeInfo
getTypeSynonymTypeInfo typeSynonym = do
  typeSynonymTypes <- typeSynonymTypeInfos <$> getState
  case Map.lookup typeSynonym typeSynonymTypes of
    Nothing -> throwError $ ShouldNotGetHereError "Called getTypeSynonymType before type synonym was initialized"
    Just (Right typeSynonymTypeInfo) -> return typeSynonymTypeInfo
    Just (Left (maybeMutabilityParameter, typeParameters, typeValueExpression)) -> do
      inProgressTypeSynonyms <- inProgressTypeSynonyms <$> getState
      case Seq.elemIndexL typeSynonym inProgressTypeSynonyms of
        Nothing -> return ()
        Just index -> throwError $ TypeSynonymCyclicReferencesError (toList $ Seq.take (index + 1) $ getTextName <$> inProgressTypeSynonyms)
      setTypeSynonymInProgress
      typeSynonymTypeFunc <- getParametrizedTypeFunc (NormalContext maybeMutabilityParameter) typeParameters typeValueExpression
      typeSynonymVarianceFunc <- getTypeVarianceFunc (NormalContext maybeMutabilityParameter) typeParameters typeValueExpression
      let typeSynonymTypeInfo = TypeSynonymTypeInfo {typeSynonymArity = Seq.length typeParameters, typeSynonymTypeFunc, typeSynonymVarianceFunc}
      setTypeSynonymTypeInfo typeSynonym typeSynonymTypeInfo
      setTypeSynonymNotInProgress
      return typeSynonymTypeInfo
  where
    setTypeSynonymInProgress = do
      state <- getState
      setState state {inProgressTypeSynonyms = typeSynonym :<| inProgressTypeSynonyms state}
    setTypeSynonymNotInProgress = do
      state <- getState
      case inProgressTypeSynonyms state of
        headInProgress :<| tailInProgress | headInProgress == typeSynonym -> setState state {inProgressTypeSynonyms = tailInProgress}
        _ -> throwError $ ShouldNotGetHereError "setTypeSynonymNotInProgress error"

getTypeSynonymTypeFunc :: Range -> Int -> BoundTypeSynonym -> TypeChecker (Mutability -> Seq Type -> Type)
getTypeSynonymTypeFunc usageRange numArguments typeSynonym = do
  TypeSynonymTypeInfo {typeSynonymArity, typeSynonymTypeFunc} <- getTypeSynonymTypeInfo typeSynonym
  unless (numArguments == typeSynonymArity) $
    throwError (TypeSynonymWrongNumberOfTypeArgumentsError usageRange (getTextName typeSynonym) typeSynonymArity numArguments)
  return $ typeSynonymTypeFunc

getTypeSynonymVariances :: BoundTypeSynonym -> Mutability -> TypeChecker (Seq Variance)
getTypeSynonymVariances typeSynonym mutability = do
  TypeSynonymTypeInfo {typeSynonymVarianceFunc} <- getTypeSynonymTypeInfo typeSynonym
  return $ typeSynonymVarianceFunc mutability

getListVariance :: Mutability -> Variance
getListVariance Immutable = Covariant
getListVariance Mutable = Invariant

-- Mutability context

getMutabilityFunc :: Range -> MutabilityContext -> IBMutabilityExpression -> TypeChecker (Mutability -> Mutability)
getMutabilityFunc usageRange mutabilityContext mutabilityExpression = case mutabilityContext of
  (RecordContext Nothing True) -> case mutabilityExpression of
    Left Immutable -> return id
    Left Mutable -> throwError $ RecordFieldExplicitMutabilityError usageRange
    Right _ -> throwError (ShouldNotGetHereError "Encountered out-of-scope mutability parameter in getMutabilityFunc")
  (RecordContext maybeMutabilityParameter _) -> case mutabilityExpression of
    Left mutability -> return $ const mutability
    Right mutabilityIdentifier -> do
      unless (maybeMutabilityParameter == Just mutabilityIdentifier) $
        throwError (ShouldNotGetHereError "Encountered out-of-scope mutability parameter in getMutabilityFunc")
      return id
  (NormalContext maybeMutabilityParameter) -> case mutabilityExpression of
    Left mutability -> return $ const mutability
    Right mutabilityIdentifier -> do
      unless (maybeMutabilityParameter == Just mutabilityIdentifier) $
        throwError (ShouldNotGetHereError "Encountered out-of-scope mutability parameter in getMutabilityFunc")
      return id

recordContext :: Maybe IBMutabilityParameter -> MutabilityContext
recordContext maybeMutabilityParameter = RecordContext maybeMutabilityParameter True

updateMutabilityContext :: MutabilityContext -> MutabilityContext
updateMutabilityContext (RecordContext maybeMutabilityParameter _) = RecordContext maybeMutabilityParameter False
updateMutabilityContext (NormalContext maybeMutabilityParameter) = NormalContext maybeMutabilityParameter

-- Variance calculation

initialVarianceFunc :: Seq IBTypeParameter -> Mutability -> Seq Variance
initialVarianceFunc typeParameters = const (typeParameters $> Bivariant)

type VarianceInfo = (Either (IBRecordIdentifier, Seq IBTypeExpression) (IBTypeSynonym, IBTypeExpression), Maybe IBMutabilityParameter, Seq IBTypeParameter)

type VarianceMap = Map (Either IBRecordIdentifier IBTypeSynonym, Mutability) (Seq Variance)

calculateVariances :: Seq VarianceInfo -> TypeChecker ()
calculateVariances typeInfos = do
  initialVarianceMap <- getVariancesMap $ typeInfos <&> \(typeNameAndValues, _, _) -> bimap fst fst typeNameAndValues
  calculateVariancesHelper typeInfos initialVarianceMap

calculateVariancesHelper :: Seq VarianceInfo -> VarianceMap -> TypeChecker ()
calculateVariancesHelper typeInfos initialVarianceMap = do
  forM_ typeInfos $ \(typeName, mutabilityParameter, typeParameters) -> case typeName of
    Left (recordName, fieldTypeExpressions) -> do
      let mutabilityContext = recordContext mutabilityParameter
      fieldVarianceFuncs <- forM fieldTypeExpressions $ getTypeVarianceFunc mutabilityContext typeParameters
      let combinedVarianceFunc mutability = case mutability of
            -- In an immutable record, field values are used only as outputs, so the record is covariant to all of its fields
            Immutable -> collapse $ fieldVarianceFuncs <&> ($ mutability)
            {- In a mutable record, field values are used both as inputs and outputs, so the record is invariant to all
               of its fields
            -}
            Mutable -> (Invariant ~*) <$> collapse (fieldVarianceFuncs <&> ($ mutability))
      setRecordVariancesFunc recordName combinedVarianceFunc
    Right (typeSynonym, vaueTypeExpression) -> do
      let mutabilityContext = NormalContext mutabilityParameter
      valueTypeExpressionVarianceFunc <- getTypeVarianceFunc mutabilityContext typeParameters vaueTypeExpression
      setTypeSynonymVariancesFunc typeSynonym valueTypeExpressionVarianceFunc
  finalVariancesMap <- getVariancesMap $ typeInfos <&> \(typeNameAndValues, _, _) -> bimap fst fst typeNameAndValues
  if initialVarianceMap == finalVariancesMap
    then return ()
    else calculateVariancesHelper typeInfos finalVariancesMap

getVariancesMap :: Seq (Either IBRecordIdentifier IBTypeSynonym) -> TypeChecker VarianceMap
getVariancesMap typeNames = do
  variancePairsList <- forM typeNames $ \typeName -> do
    let getVariances = case typeName of
          Left recordName -> getRecordVariances recordName
          Right typeSynonym -> getTypeSynonymVariances typeSynonym
    immutableVariances <- getVariances Immutable
    mutableVariances <- getVariances Mutable
    return [((typeName, Immutable), immutableVariances), ((typeName, Mutable), mutableVariances)]
  return $ Map.fromList . concat $ variancePairsList

getTypeVarianceFunc ::
  MutabilityContext ->
  Seq IBTypeParameter ->
  IBTypeExpression ->
  TypeChecker (Mutability -> Seq Variance)
getTypeVarianceFunc mutabilityContext typeParameters typeExpression = case typeExpression of
  IntTypeExpression _ -> return . const $ typeParameters $> Bivariant
  FloatTypeExpression _ -> return . const $ typeParameters $> Bivariant
  CharTypeExpression _ -> return . const $ typeParameters $> Bivariant
  StringTypeExpression _ -> return . const $ typeParameters $> Bivariant
  BoolTypeExpression _ -> return . const $ typeParameters $> Bivariant
  NilTypeExpression _ -> return . const $ typeParameters $> Bivariant
  FunctionTypeExpression _ parameterTypeExpression returnTypeExpression -> do
    parameterVarianceFuncs <- forM parameterTypeExpression $ getTypeVarianceFunc updatedMutabilityContext typeParameters
    returnVarianceFunc <- getTypeVarianceFunc updatedMutabilityContext typeParameters returnTypeExpression
    return $ \mutabilityArgument ->
      let parameterVariances = parameterVarianceFuncs <&> ($ mutabilityArgument)
          invertedParameterVariances = fmap (fmap (Contravariant ~*)) parameterVariances
          returnVariances = returnVarianceFunc mutabilityArgument
       in foldr (Seq.zipWith (<>)) returnVariances invertedParameterVariances
  IdentifierTypeExpression _ _ (Left typeParameter) _ ->
    return . const $ typeParameters <&> \tp -> if tp == typeParameter then Covariant else Bivariant
  IdentifierTypeExpression expressionRange mutabilityExpression (Right typeSynonym) typeArguments -> do
    typeArgumentVarianceFuncs <- forM typeArguments $ getTypeVarianceFunc updatedMutabilityContext typeParameters
    typeSynonymVarianceFunc <- typeSynonymVarianceFunc <$> getTypeSynonymTypeInfo typeSynonym
    mutabilityFunc <- getMutabilityFunc expressionRange mutabilityContext mutabilityExpression
    return $ \mutabilityArgument ->
      let mutability = mutabilityFunc mutabilityArgument
          typeArgumentVariances = typeArgumentVarianceFuncs <&> ($ mutabilityArgument)
          typeSynonymInnerVariances = typeSynonymVarianceFunc mutability
       in matrixMultiply typeArgumentVariances typeSynonymInnerVariances
  ListTypeExpression expressionRange mutabilityExpression typeArguments -> do
    unless (Seq.length typeArguments == 1) $
      throwError (ListWrongNumberOfTypeArgumentsError expressionRange (Seq.length typeArguments))
    let typeArgument = seqHead typeArguments
    typeArgumentVarianceFunc <- getTypeVarianceFunc updatedMutabilityContext typeParameters typeArgument
    mutabilityFunc <- getMutabilityFunc expressionRange mutabilityContext mutabilityExpression
    return $ \mutabilityArgument ->
      let mutability = mutabilityFunc mutabilityArgument
          listInnerVariance = getListVariance mutability
          typeArgumentVariances = typeArgumentVarianceFunc mutabilityArgument
       in (listInnerVariance ~*) <$> typeArgumentVariances
  RecordUnionTypeExpression expressionRange mutabilityExpression records -> do
    mutabilityFunc <- getMutabilityFunc expressionRange mutabilityContext mutabilityExpression
    recordVarianceFuncs <- forM records $ \(recordName, _) -> recordVarianceFunc <$> getRecordTypeInfo recordName
    typeArgumentVarianceFuncs <- forM records $ \(_, typeArguments) -> forM typeArguments $ getTypeVarianceFunc updatedMutabilityContext typeParameters
    return $ \mutabilityArgument ->
      let mutability = mutabilityFunc mutabilityArgument
          recordInnerVariances = recordVarianceFuncs <&> ($ mutability)
          typeArgumentVariancesList = typeArgumentVarianceFuncs <&> (<&> ($ mutabilityArgument))
       in collapse $ Seq.zipWith matrixMultiply typeArgumentVariancesList recordInnerVariances
  where
    updatedMutabilityContext = updateMutabilityContext mutabilityContext
    -- Performs a matrix multiplication between a variance matrix and vector. This can be used to find the variances of
    -- a type function application where the matrix is the list of type argument variance lists, and the vector is the
    -- variance list of the type function with respect to its own arguments
    matrixMultiply :: Seq (Seq Variance) -> Seq Variance -> Seq Variance
    matrixMultiply typeArgumentsVariances innerVariances =
      collapse $ Seq.zipWith (\typeArgumentVariance innerVariance -> (innerVariance ~*) <$> typeArgumentVariance) typeArgumentsVariances innerVariances

-- Takes a list of type parameter variance lists and sums them element-wise into a combined type parameter variance lists
collapse :: Seq (Seq Variance) -> Seq Variance
collapse matrix = fold <$> seqTranspose matrix

-- Type expression resolvers

fromTypeExpression :: IBTypeExpression -> TypeChecker Type
fromTypeExpression typeExpression = case typeExpression of
  (IntTypeExpression _) -> return IntType
  (FloatTypeExpression _) -> return FloatType
  (CharTypeExpression _) -> return CharType
  (StringTypeExpression _) -> return StringType
  (BoolTypeExpression _) -> return BoolType
  (NilTypeExpression _) -> return NilType
  (FunctionTypeExpression _ parameterTypeExpressions returnTypeExpression) -> do
    parameterTypes <- mapM fromTypeExpression parameterTypeExpressions
    returnType <- fromTypeExpression returnTypeExpression
    return $ FunctionType parameterTypes returnType
  (IdentifierTypeExpression expressionRange mutabilityExpression (Left typeParameter) typeArguments) -> do
    unless (null typeArguments) $
      throwError (TypeArrgumentsAppliedToTypeParameterError expressionRange (getTextName typeParameter))
    unless (mutabilityExpression == Left Immutable) $
      throwError (MutabilityAppliedToTypeParameterError expressionRange (getTextName typeParameter))
    return (IdentifierType (getTypeParameterIndex typeParameter) (getTextName typeParameter))
  (IdentifierTypeExpression typeSynonymRange mutabilityExpression (Right typeSynonym) typeSynonymArgumentExpressions) -> do
    mutability <- getMutability mutabilityExpression
    typeSynonymTypeFunc <- getTypeSynonymTypeFunc typeSynonymRange (Seq.length typeSynonymArgumentExpressions) typeSynonym
    typeSynonymArguments <- forM typeSynonymArgumentExpressions fromTypeExpression
    return $ typeSynonymTypeFunc mutability typeSynonymArguments
  (RecordUnionTypeExpression expressionRange mutabilityExpression recordExpressions) -> do
    mutability <- getMutability mutabilityExpression
    let addRecord recordMap (recordName, recordTypeParameterExpressions) = do
          recordTypeParameters <- mapM fromTypeExpression recordTypeParameterExpressions
          let (conflictingRecordType, updatedMap) = insertAndReplace recordName recordTypeParameters recordMap
          case conflictingRecordType of
            Just _ -> throwError $ RecordUnionTypeExpressionDuplicateRecordsError expressionRange (getTextName recordName)
            Nothing -> return updatedMap
    records <- foldM addRecord Map.empty recordExpressions
    return $ RecordUnionType mutability records
  (ListTypeExpression expressionRange mutabilityExpression typeArgumentExpressions) -> do
    mutability <- getMutability mutabilityExpression
    typeArguments <- mapM fromTypeExpression typeArgumentExpressions
    unless (Seq.length typeArguments == 1) $
      throwError (ListWrongNumberOfTypeArgumentsError expressionRange (Seq.length typeArguments))
    return $ ListType mutability (seqHead typeArguments)
  where
    getMutability mutabilityExpression = case mutabilityExpression of
      Left mutability -> return mutability
      Right _ -> throwError $ ShouldNotGetHereError "Encountered mutability parameter outside of scope"

getParametrizedTypeFunc :: MutabilityContext -> Seq IBTypeParameter -> IBTypeExpression -> TypeChecker (Mutability -> Seq Type -> Type)
getParametrizedTypeFunc mutabilityContext typeParameters typeExpression = case typeExpression of
  (IntTypeExpression _) -> return $ const . const IntType
  (FloatTypeExpression _) -> return $ const . const FloatType
  (CharTypeExpression _) -> return $ const . const CharType
  (StringTypeExpression _) -> return $ const . const StringType
  (BoolTypeExpression _) -> return $ const . const BoolType
  (NilTypeExpression _) -> return $ const . const NilType
  (IdentifierTypeExpression expressionRange mutabilityExpression (Left typeParameterReference) identifierTypeArgumentExpressions) -> do
    unless (null identifierTypeArgumentExpressions) $
      throwError (TypeArrgumentsAppliedToTypeParameterError expressionRange (getTextName typeParameterReference))
    unless (mutabilityExpression == Left Immutable) $
      throwError (MutabilityAppliedToTypeParameterError expressionRange (getTextName typeParameterReference))
    case Seq.elemIndexL typeParameterReference typeParameters of
      Nothing -> return $ const . const $ IdentifierType (getTypeParameterIndex typeParameterReference) (getTextName typeParameterReference)
      Just paramterIndex -> return $ \_ functionTypeArguments -> Seq.index functionTypeArguments paramterIndex
  (IdentifierTypeExpression expressionRange mutabilityExpression (Right typeSynonym) synonymTypeArgumentExpressions) -> do
    mutabilityFunc <- getMutabilityFunc expressionRange mutabilityContext mutabilityExpression
    typeSynonymTypeFunc <- getTypeSynonymTypeFunc expressionRange (Seq.length synonymTypeArgumentExpressions) typeSynonym
    synonymTypeArgumentFuncs <- forM synonymTypeArgumentExpressions $ getParametrizedTypeFunc updatedMutabilityContext typeParameters
    return $ \mutability typeArguments ->
      let synonymTypeArguments = synonymTypeArgumentFuncs <&> \f -> f mutability typeArguments
       in typeSynonymTypeFunc (mutabilityFunc mutability) synonymTypeArguments
  (FunctionTypeExpression _ functionParameterTypeExpressions functionReturnTypeExpression) -> do
    functionParameterTypeFuncs <- forM functionParameterTypeExpressions $ getParametrizedTypeFunc updatedMutabilityContext typeParameters
    functionReturnTypeFunc <- getParametrizedTypeFunc updatedMutabilityContext typeParameters functionReturnTypeExpression
    return $ \mutability typeArguments ->
      let functionParameterTypes = functionParameterTypeFuncs <&> \f -> f mutability typeArguments
          functionReturnType = functionReturnTypeFunc mutability typeArguments
       in FunctionType functionParameterTypes functionReturnType
  (RecordUnionTypeExpression expressionRange mutabilityExpression recordExpressions) -> do
    mutabilityFunc <- getMutabilityFunc expressionRange mutabilityContext mutabilityExpression
    let addRecord recordMap (recordName, recordTypeArgumentExpressions) = do
          recordTypeArgumentFuncs <- forM recordTypeArgumentExpressions $ getParametrizedTypeFunc updatedMutabilityContext typeParameters
          let (conflictingRecordType, updatedMap) =
                insertAndReplace
                  recordName
                  (\mutabilityArgument typeArguments -> recordTypeArgumentFuncs <&> \f -> f mutabilityArgument typeArguments)
                  recordMap
          case conflictingRecordType of
            Just _ -> throwError $ RecordUnionTypeExpressionDuplicateRecordsError expressionRange (getTextName recordName)
            Nothing -> return updatedMap
    recordTypeArgumentFuncsMap <- foldM addRecord Map.empty recordExpressions
    return $ \mutability typeArguments ->
      let recordTypeArgumentsMap = recordTypeArgumentFuncsMap <&> \f -> f mutability typeArguments
       in RecordUnionType (mutabilityFunc mutability) recordTypeArgumentsMap
  (ListTypeExpression expressionRange mutabilityExpression typeArgumentExpressions) -> do
    mutabilityFunc <- getMutabilityFunc expressionRange mutabilityContext mutabilityExpression
    typeArguments <- forM typeArgumentExpressions $ getParametrizedTypeFunc updatedMutabilityContext typeParameters
    unless (Seq.length typeArguments == 1) $
      throwError (ListWrongNumberOfTypeArgumentsError expressionRange (Seq.length typeArguments))
    return $ \recordMutability recordTypeParameters -> ListType (mutabilityFunc recordMutability) (seqHead typeArguments recordMutability recordTypeParameters)
  where
    updatedMutabilityContext = updateMutabilityContext mutabilityContext

-- Type functions

isCompatibleWith :: Type -> Type -> TypeChecker Bool
isCompatibleWith (RecordUnionType actualMutability actualRecordMap) (RecordUnionType expectedMutability expectedRecordMap) =
  case (expectedMutability, actualMutability) of
    (Mutable, Immutable) -> return False
    _ -> do
      recordCompatibilities <- forM (Map.toList actualRecordMap) $ \(recordName, actualRecordArguments) ->
        case Map.lookup recordName expectedRecordMap of
          Nothing -> return False
          Just expectedRecordArguments -> do
            parameterVariances <- getRecordVariances recordName expectedMutability
            argumentCompatibilities <-
              forM (Seq.zip3 parameterVariances actualRecordArguments expectedRecordArguments) $ uncurry3 typeArgumentCompatibility
            return (and argumentCompatibilities)
      return (and recordCompatibilities)
isCompatibleWith (FunctionType actualParameterTypes actualReturnType) (FunctionType expectedParameterTypes expectedReturnType) =
  if Seq.length expectedParameterTypes == Seq.length actualParameterTypes
    then do
      returnTypesAreCompatible <- actualReturnType `isCompatibleWith` expectedReturnType
      parameterTypesAreCompatible <- forM (Seq.zip actualParameterTypes expectedParameterTypes) $
        \(actualParameterType, expectedParameterType) -> expectedParameterType `isCompatibleWith` actualParameterType
      return $ returnTypesAreCompatible && and parameterTypesAreCompatible
    else return False
isCompatibleWith (ListType actualMutability actualValueType) (ListType expectedMutability expectedValueType) =
  case expectedMutability of
    Immutable -> actualValueType `isCompatibleWith` expectedValueType
    Mutable -> return $ actualMutability == Mutable && actualValueType == expectedValueType
isCompatibleWith actualType expectedType = return $ actualType == expectedType

typeArgumentCompatibility :: Variance -> Type -> Type -> TypeChecker Bool
typeArgumentCompatibility Covariant actualTypeArgument expectedTypeArgument =
  actualTypeArgument `isCompatibleWith` expectedTypeArgument
typeArgumentCompatibility Contravariant actualTypeArgument expectedTypeArgument =
  expectedTypeArgument `isCompatibleWith` actualTypeArgument
typeArgumentCompatibility Invariant actualTypeArgument expectedTypeArgument =
  return $ expectedTypeArgument == actualTypeArgument
typeArgumentCompatibility Bivariant _ _ =
  return True

typeUnion :: Type -> Type -> TypeChecker (Maybe Type)
typeUnion (RecordUnionType mutability1 recordMap1) (RecordUnionType mutability2 recordMap2) = do
  let combinedMutability = case (mutability1, mutability2) of
        (Mutable, Mutable) -> Mutable
        _ -> Immutable
  let combinedRecordNames = Set.union (Map.keysSet recordMap1) (Map.keysSet recordMap2)
  recordNameTypeParametersPairs <- forM (Set.toList combinedRecordNames) $ \recordName -> do
    case (Map.lookup recordName recordMap1, Map.lookup recordName recordMap2) of
      (Nothing, Nothing) -> throwError $ ShouldNotGetHereError "Got record name in neither map in typeUnion"
      (Just typeArguments1, Nothing) -> return $ Just (recordName, typeArguments1)
      (Nothing, Just typeArguments2) -> return $ Just (recordName, typeArguments2)
      (Just typeArguments1, Just typeArguments2) -> do
        parameterVariances <- getRecordVariances recordName combinedMutability
        combinedTypeParameters <- forM (Seq.zip3 parameterVariances typeArguments1 typeArguments2) $ uncurry3 typeArgumentUnion
        return $ (recordName,) <$> sequenceA combinedTypeParameters
  return $ RecordUnionType combinedMutability . Map.fromList <$> sequence recordNameTypeParametersPairs
typeUnion (FunctionType parameterTypes1 returnType1) (FunctionType parameterTypes2 returnType2) =
  if Seq.length parameterTypes1 == Seq.length parameterTypes2
    then do
      combinedParameterTypes <- sequenceA <$> mapM (uncurry typeIntersection) (Seq.zip parameterTypes1 parameterTypes2)
      combinedReturnType <- typeUnion returnType1 returnType2
      return $ liftA2 FunctionType combinedParameterTypes combinedReturnType
    else return Nothing
typeUnion (ListType mutability1 valueType1) (ListType mutability2 valueType2) = do
  let combinedMutability = case (mutability1, mutability2) of
        (Mutable, Mutable) -> Mutable
        _ -> Immutable
  combinedValueType <- typeArgumentUnion (getListVariance combinedMutability) valueType1 valueType2
  return $ ListType combinedMutability <$> combinedValueType
typeUnion type1 type2 = if type1 == type2 then return $ Just type1 else return Nothing

typeArgumentUnion :: Variance -> Type -> Type -> TypeChecker (Maybe Type)
typeArgumentUnion Covariant typeArgument1 typeArgument2 = typeUnion typeArgument1 typeArgument2
typeArgumentUnion Contravariant typeArgument1 typeArgument2 = typeIntersection typeArgument1 typeArgument2
typeArgumentUnion Invariant typeArgument1 typeArgument2 = if typeArgument1 == typeArgument2 then return $ Just typeArgument1 else return Nothing
typeArgumentUnion Bivariant typeArgument1 typeArgument2 = typeUnion typeArgument1 typeArgument2

typeUnionF :: (Foldable1 t, Functor t) => t Type -> TypeChecker (Maybe Type)
typeUnionF types = foldlM1 typeUnionM (Just <$> types)
  where
    typeUnionM maybeType1 maybeType2 = case (maybeType1, maybeType2) of
      (Just type1, Just type2) -> typeUnion type1 type2
      _ -> return Nothing

typeIntersection :: Type -> Type -> TypeChecker (Maybe Type)
typeIntersection (RecordUnionType mutability1 recordMap1) (RecordUnionType mutability2 recordMap2) = do
  let combinedMutability = case (mutability1, mutability2) of
        (Immutable, Immutable) -> Immutable
        _ -> Mutable
  let overlappingRecordNames = Set.intersection (Map.keysSet recordMap1) (Map.keysSet recordMap2)
  if Set.size overlappingRecordNames == 0
    then return Nothing
    else do
      recordNameTypeParametersPairs <- forM (Set.toList overlappingRecordNames) $ \recordName -> do
        case (Map.lookup recordName recordMap1, Map.lookup recordName recordMap2) of
          (Just typeArguments1, Just typeArguments2) -> do
            parameterVariances <- getRecordVariances recordName combinedMutability
            combinedTypeParameters <- forM (Seq.zip3 parameterVariances typeArguments1 typeArguments2) $ uncurry3 typeArgumentIntersection
            return $ (recordName,) <$> sequenceA combinedTypeParameters
          _ -> throwError $ ShouldNotGetHereError "Got record name missing in a map in typeIntersection"
      return $ RecordUnionType combinedMutability . Map.fromList <$> sequence recordNameTypeParametersPairs
typeIntersection (FunctionType parameterTypes1 returnType1) (FunctionType parameterTypes2 returnType2) =
  if Seq.length parameterTypes1 == Seq.length parameterTypes2
    then do
      combinedParameterTypes <- sequenceA <$> mapM (uncurry typeUnion) (Seq.zip parameterTypes1 parameterTypes2)
      combinedReturnType <- typeIntersection returnType1 returnType2
      return $ liftA2 FunctionType combinedParameterTypes combinedReturnType
    else return Nothing
typeIntersection (ListType mutability1 valueType1) (ListType mutability2 valueType2) = do
  let combinedMutability = case (mutability1, mutability2) of
        (Immutable, Immutable) -> Immutable
        _ -> Mutable
  combinedValueType <- typeArgumentIntersection (getListVariance combinedMutability) valueType1 valueType2
  return $ ListType combinedMutability <$> combinedValueType
typeIntersection type1 type2 = if type1 == type2 then return $ Just type1 else return Nothing

typeArgumentIntersection :: Variance -> Type -> Type -> TypeChecker (Maybe Type)
typeArgumentIntersection Covariant typeArgument1 typeArgument2 = typeIntersection typeArgument1 typeArgument2
typeArgumentIntersection Contravariant typeArgument1 typeArgument2 = typeUnion typeArgument1 typeArgument2
typeArgumentIntersection Invariant typeArgument1 typeArgument2 = if typeArgument1 == typeArgument2 then return $ Just typeArgument1 else return Nothing
typeArgumentIntersection Bivariant typeArgument1 typeArgument2 = typeIntersection typeArgument1 typeArgument2

typeIntersectionF :: (Foldable1 t, Functor t) => t Type -> TypeChecker (Maybe Type)
typeIntersectionF types = foldlM1 typeIntersectionM (Just <$> types)
  where
    typeIntersectionM maybeType1 maybeType2 = case (maybeType1, maybeType2) of
      (Just type1, Just type2) -> typeIntersection type1 type2
      _ -> return Nothing

-- Function context

getFunctionContext :: TypeChecker (Maybe FunctionContext)
getFunctionContext = listToMaybe . functionContextStack <$> getState

pushFunctionContext :: FunctionContext -> TypeChecker ()
pushFunctionContext functionContext = do
  state <- getState
  let updatedFunctionContextStack = functionContext : functionContextStack state
  setState state {functionContextStack = updatedFunctionContextStack}

popFunctionContext :: TypeChecker ()
popFunctionContext = do
  state <- getState
  let updatedFunctionContextStack = tail $ functionContextStack state
  setState state {functionContextStack = updatedFunctionContextStack}

withFunctionContext :: FunctionContext -> TypeChecker a -> TypeChecker a
withFunctionContext functionContext checker =
  do
    pushFunctionContext functionContext
    checker
    `andFinally` popFunctionContext
