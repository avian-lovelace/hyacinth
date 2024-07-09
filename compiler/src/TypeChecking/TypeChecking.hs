module TypeChecking.TypeChecking
  ( TypeChecker,
    TypeCheckingState (recordFieldOrders),
    FunctionContext (FunctionContext, contextReturnType),
    Variance (Covariant, Contravariant, Invariant),
    setValueIdentifierType,
    getValueIdentifierType,
    getFunctionContext,
    initialTypeCheckingState,
    setFunctionType,
    getFunctionType,
    setRecordFieldTypes,
    getRecordFieldTypes,
    withFunctionContext,
    addRecordFieldOrder,
    getRecordFieldOrders,
    isCompatibleWith,
    typeUnion,
    typeUnionF,
    typeIntersection,
    typeIntersectionF,
    getRecordNumTypeParameters,
    setRecordTypeParameterVariances,
    fromTypeExpression,
    getRecordFieldTypeFunc,
    getParametrizedTypeFunc,
    initializeTypeSynonymType,
    getTypeSynonymType,
  )
where

import Control.Monad (foldM, unless)
import Core.ErrorState
import Core.Errors
import Core.FilePositions
import Core.SyntaxTree
import Core.Utils
import Data.Foldable (toList)
import Data.Foldable1 (Foldable1, foldlM1)
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isNothing, listToMaybe)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Traversable (forM)
import IdentifierBinding.SyntaxTree
import Parsing.SyntaxTree
import TypeChecking.Type

data TypeCheckingState = TypeCheckingState
  { valueIdentifierTypes :: Map BoundValueIdentifier Type,
    functionTypes :: Map BoundFunctionIdentifier (Int, Seq Type -> Type),
    recordTypes :: Map BoundRecordIdentifier (Int, Mutability -> Seq Type -> Map UnboundIdentifier Type),
    typeSynonymTypes ::
      Map
        BoundTypeSynonym
        ( Either
            (Maybe IBMutabilityParameter, Seq IBTypeParameter, IBTypeExpression)
            (Int, Mutability -> Seq Type -> Type)
        ),
    functionContextStack :: [FunctionContext],
    recordFieldOrders :: Map BoundRecordIdentifier (Seq UnboundIdentifier),
    recordTypeParameterVariances :: Map BoundRecordIdentifier (Seq Variance, Seq Variance),
    inProgressTypeSynonyms :: Seq BoundTypeSynonym
  }

newtype FunctionContext = FunctionContext {contextReturnType :: Type}

data Variance = Covariant | Contravariant | Invariant

type TypeChecker = ErrorState TypeCheckingState

initialTypeCheckingState :: TypeCheckingState
initialTypeCheckingState =
  TypeCheckingState
    { valueIdentifierTypes = Map.empty,
      functionTypes = Map.empty,
      recordTypes = Map.empty,
      typeSynonymTypes = Map.empty,
      functionContextStack = [],
      recordFieldOrders = Map.empty,
      recordTypeParameterVariances = Map.empty,
      inProgressTypeSynonyms = []
    }

setValueIdentifierType :: BoundValueIdentifier -> Type -> TypeChecker ()
setValueIdentifierType identifier identifierType = do
  valueIdentifierTypes <- valueIdentifierTypes <$> getState
  setValueIdentifierTypes $ Map.insert identifier identifierType valueIdentifierTypes

getValueIdentifierType :: BoundValueIdentifier -> TypeChecker Type
getValueIdentifierType identifier = do
  valueIdentifierTypes <- valueIdentifierTypes <$> getState
  case Map.lookup identifier valueIdentifierTypes of
    Just identifierType -> return identifierType
    Nothing -> do
      throwError $ ShouldNotGetHereError "Called getValueIdentifierType before identifier was initialized"

setFunctionType :: BoundFunctionIdentifier -> Int -> (Seq Type -> Type) -> TypeChecker ()
setFunctionType functionName numTypeParameters functionTypeFunc = do
  state <- getState
  let updatedFunctionTypes = Map.insert functionName (numTypeParameters, functionTypeFunc) $ functionTypes state
  setState state {functionTypes = updatedFunctionTypes}

getFunctionType :: Range -> BoundFunctionIdentifier -> Seq Type -> TypeChecker Type
getFunctionType usageRange functionName typeArguments = do
  functionTypes <- functionTypes <$> getState
  case Map.lookup functionName functionTypes of
    Just (functionTypeArity, functionTypeFunc) -> do
      unless (Seq.length typeArguments == functionTypeArity) $
        throwError (FunctionWrongNumberOfTypeArgumentsError usageRange (getTextName functionName) functionTypeArity (Seq.length typeArguments))
      return $ functionTypeFunc typeArguments
    Nothing -> throwError $ ShouldNotGetHereError "Called getFunctionType before function was initialized"

setRecordFieldTypes :: BoundRecordIdentifier -> Int -> (Mutability -> Seq Type -> Map UnboundIdentifier Type) -> TypeChecker ()
setRecordFieldTypes recordName recordTypeArity fieldTypeMapFunc = do
  state <- getState
  let updatedRecordTypes = Map.insert recordName (recordTypeArity, fieldTypeMapFunc) $ recordTypes state
  setState $ state {recordTypes = updatedRecordTypes}

getRecordFieldTypes :: Range -> BoundRecordIdentifier -> Mutability -> Seq Type -> TypeChecker (Map UnboundIdentifier Type)
getRecordFieldTypes usageRange recordName mutability typeArguments = do
  recordTypes <- recordTypes <$> getState
  case Map.lookup recordName recordTypes of
    Nothing -> throwError $ ShouldNotGetHereError "Called getFunctionType before function was initialized"
    Just (recordTypeArity, fieldTypeMapFunc) -> do
      unless (Seq.length typeArguments == recordTypeArity) $
        throwError (RecordExpresssionNumTypeArgumentsError usageRange (getTextName recordName) recordTypeArity (Seq.length typeArguments))
      return $ fieldTypeMapFunc mutability typeArguments

initializeTypeSynonymType :: IBTypeSynonym -> Maybe IBMutabilityParameter -> Seq IBTypeParameter -> IBTypeExpression -> TypeChecker ()
initializeTypeSynonymType typeSynonym maybeMutabilityParameter typeParameters typeValueExpression = do
  state <- getState
  setState state {typeSynonymTypes = Map.insert typeSynonym (Left (maybeMutabilityParameter, typeParameters, typeValueExpression)) $ typeSynonymTypes state}

getTypeSynonymType :: Range -> Int -> BoundTypeSynonym -> TypeChecker (Mutability -> Seq Type -> Type)
getTypeSynonymType usageRange numTypeArguments typeSynonym = do
  typeSynonymTypes <- typeSynonymTypes <$> getState
  case Map.lookup typeSynonym typeSynonymTypes of
    Nothing -> throwError $ ShouldNotGetHereError "Called getTypeSynonymType before type synonym was initialized"
    Just (Right (typeSynonymArity, typeValueFunction)) -> do
      unless (numTypeArguments == typeSynonymArity) $
        throwError (TypeSynonymWrongNumberOfTypeArgumentsError usageRange (getTextName typeSynonym) typeSynonymArity numTypeArguments)
      return typeValueFunction
    Just (Left (maybeMutabilityParameter, typeParameters, typeValueExpression)) -> do
      unless (numTypeArguments == Seq.length typeParameters) $
        throwError (TypeSynonymWrongNumberOfTypeArgumentsError usageRange (getTextName typeSynonym) (Seq.length typeParameters) numTypeArguments)
      inProgressTypeSynonyms <- inProgressTypeSynonyms <$> getState
      case Seq.elemIndexL typeSynonym inProgressTypeSynonyms of
        Nothing -> return ()
        Just index -> throwError $ TypeSynonymCyclicReferencesError (toList $ Seq.take (index + 1) $ getTextName <$> inProgressTypeSynonyms)
      setTypeSynonymInProgress typeSynonym
      typeSynonymValueFunc <- getParametrizedTypeFunc maybeMutabilityParameter typeParameters typeValueExpression
      setTypeSynonymValueFunc typeSynonym (Seq.length typeParameters) typeSynonymValueFunc
      setTypeSynonymNotInProgress typeSynonym
      return typeSynonymValueFunc

setTypeSynonymValueFunc :: BoundTypeSynonym -> Int -> (Mutability -> Seq Type -> Type) -> TypeChecker ()
setTypeSynonymValueFunc typeSynonym typeSynonymArity typeSynonymValueFunc = do
  state <- getState
  setState state {typeSynonymTypes = Map.insert typeSynonym (Right (typeSynonymArity, typeSynonymValueFunc)) $ typeSynonymTypes state}

setTypeSynonymInProgress :: BoundTypeSynonym -> TypeChecker ()
setTypeSynonymInProgress typeSynonym = do
  state <- getState
  setState state {inProgressTypeSynonyms = typeSynonym :<| inProgressTypeSynonyms state}

setTypeSynonymNotInProgress :: BoundTypeSynonym -> TypeChecker ()
setTypeSynonymNotInProgress typeSynonym = do
  state <- getState
  case inProgressTypeSynonyms state of
    headInProgress :<| tailInProgress | headInProgress == typeSynonym -> setState state {inProgressTypeSynonyms = tailInProgress}
    _ -> throwError $ ShouldNotGetHereError "setTypeSynonymNotInProgress error"

getFunctionContext :: TypeChecker (Maybe FunctionContext)
getFunctionContext = listToMaybe . functionContextStack <$> getState

pushFunctionContext :: FunctionContext -> TypeChecker ()
pushFunctionContext functionContext = do
  functionContextStack <- functionContextStack <$> getState
  setFunctionContextStack (functionContext : functionContextStack)

popFunctionContext :: TypeChecker ()
popFunctionContext = do
  functionContextStack <- functionContextStack <$> getState
  setFunctionContextStack $ tail functionContextStack

withFunctionContext :: FunctionContext -> TypeChecker a -> TypeChecker a
withFunctionContext functionContext checker =
  do
    pushFunctionContext functionContext
    checker
    `andFinally` popFunctionContext

getRecordTypeParameterVariances :: BoundRecordIdentifier -> Mutability -> TypeChecker (Seq Variance)
getRecordTypeParameterVariances recordName mutability = do
  state <- getState
  case Map.lookup recordName (recordTypeParameterVariances state) of
    Nothing -> throwError $ ShouldNotGetHereError ""
    Just (immutableVariances, mutableVariances) -> case mutability of
      Immutable -> return immutableVariances
      Mutable -> return mutableVariances

setRecordTypeParameterVariances :: BoundRecordIdentifier -> Seq Variance -> Seq Variance -> TypeChecker ()
setRecordTypeParameterVariances recordName immutableVariances mutableVariances = do
  state <- getState
  let updatedRecordTypeParameterVariances = Map.insert recordName (immutableVariances, mutableVariances) (recordTypeParameterVariances state)
  setState state {recordTypeParameterVariances = updatedRecordTypeParameterVariances}

getRecordNumTypeParameters :: BoundRecordIdentifier -> TypeChecker Int
getRecordNumTypeParameters recordName = Seq.length <$> getRecordTypeParameterVariances recordName Mutable

isCompatibleWith :: Type -> Type -> TypeChecker Bool
isCompatibleWith (RecordUnionType actualMutability actualRecordMap) (RecordUnionType expectedMutability expectedRecordMap) =
  case (expectedMutability, actualMutability) of
    (Mutable, Immutable) -> return False
    _ -> do
      recordCompatibilities <- forM (Map.toList actualRecordMap) $ \(recordName, actualRecordArguments) ->
        case Map.lookup recordName expectedRecordMap of
          Nothing -> return False
          Just expectedRecordArguments -> do
            parameterVariances <- getRecordTypeParameterVariances recordName expectedMutability
            argumentCompatibilities <- forM (Seq.zip3 actualRecordArguments expectedRecordArguments parameterVariances) $
              \(actualTypeArgument, expectedTypeArgument, typeParameterVariance) -> case typeParameterVariance of
                Covariant -> actualTypeArgument `isCompatibleWith` expectedTypeArgument
                Contravariant -> expectedTypeArgument `isCompatibleWith` actualTypeArgument
                Invariant -> return $ expectedTypeArgument == actualTypeArgument
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
isCompatibleWith actualType expectedType = return $ actualType == expectedType

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
        parameterVariances <- getRecordTypeParameterVariances recordName combinedMutability
        combinedTypeParameters <- forM (Seq.zip3 typeArguments1 typeArguments2 parameterVariances) $
          \(typeArgument1, typeArgument2, parameterVariance) -> case parameterVariance of
            Covariant -> typeUnion typeArgument1 typeArgument2
            Contravariant -> typeIntersection typeArgument1 typeArgument2
            Invariant -> if typeArgument1 == typeArgument2 then return $ Just typeArgument1 else return Nothing
        return $ (recordName,) <$> sequenceA combinedTypeParameters
  return $ RecordUnionType combinedMutability . Map.fromList <$> sequence recordNameTypeParametersPairs
typeUnion (FunctionType parameterTypes1 returnType1) (FunctionType parameterTypes2 returnType2) =
  if Seq.length parameterTypes1 == Seq.length parameterTypes2
    then do
      combinedParameterTypes <- sequenceA <$> mapM (uncurry typeIntersection) (Seq.zip parameterTypes1 parameterTypes2)
      combinedReturnType <- typeUnion returnType1 returnType2
      return $ liftA2 FunctionType combinedParameterTypes combinedReturnType
    else return Nothing
typeUnion type1 type2 = if type1 == type2 then return $ Just type1 else return Nothing

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
            parameterVariances <- getRecordTypeParameterVariances recordName combinedMutability
            combinedTypeParameters <- forM (Seq.zip3 typeArguments1 typeArguments2 parameterVariances) $
              \(typeArgument1, typeArgument2, parameterVariance) -> case parameterVariance of
                Covariant -> typeIntersection typeArgument1 typeArgument2
                Contravariant -> typeUnion typeArgument1 typeArgument2
                Invariant -> if typeArgument1 == typeArgument2 then return $ Just typeArgument1 else return Nothing
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
typeIntersection type1 type2 = if type1 == type2 then return $ Just type1 else return Nothing

typeIntersectionF :: (Foldable1 t, Functor t) => t Type -> TypeChecker (Maybe Type)
typeIntersectionF types = foldlM1 typeIntersectionM (Just <$> types)
  where
    typeIntersectionM maybeType1 maybeType2 = case (maybeType1, maybeType2) of
      (Just type1, Just type2) -> typeIntersection type1 type2
      _ -> return Nothing

fromTypeExpression :: IBTypeExpression -> TypeChecker Type
fromTypeExpression (IntTypeExpression _) = return IntType
fromTypeExpression (FloatTypeExpression _) = return FloatType
fromTypeExpression (CharTypeExpression _) = return CharType
fromTypeExpression (StringTypeExpression _) = return StringType
fromTypeExpression (BoolTypeExpression _) = return BoolType
fromTypeExpression (NilTypeExpression _) = return NilType
fromTypeExpression (FunctionTypeExpression _ parameterTypeExpressions returnTypeExpression) = do
  parameterTypes <- mapM fromTypeExpression parameterTypeExpressions
  returnType <- fromTypeExpression returnTypeExpression
  return $ FunctionType parameterTypes returnType
fromTypeExpression (IdentifierTypeExpression expressionRange mutabilityExpression (Left typeParameter) typeArguments) = do
  unless (null typeArguments) $
    throwError (TypeArrgumentsAppliedToTypeParameterError expressionRange (getTextName typeParameter))
  unless (mutabilityExpression == Left Immutable) $
    throwError (MutabilityAppliedToTypeParameterError expressionRange (getTextName typeParameter))
  return (IdentifierType (getTypeParameterIndex typeParameter) (getTextName typeParameter))
fromTypeExpression (IdentifierTypeExpression typeSynonymRange mutabilityExpression (Right typeSynonym) typeSynonymArgumentExpressions) = do
  mutability <- case mutabilityExpression of
    Left mutability -> return mutability
    Right _ -> throwError $ ShouldNotGetHereError "Encountered mutability parameter outside of scope"
  typeSynonymValueFunc <- getTypeSynonymType typeSynonymRange (Seq.length typeSynonymArgumentExpressions) typeSynonym
  typeSynonymArguments <- forM typeSynonymArgumentExpressions fromTypeExpression
  return $ typeSynonymValueFunc mutability typeSynonymArguments
fromTypeExpression (RecordUnionTypeExpression _ (Right _) _) = throwError $ ShouldNotGetHereError "Encountered mutability parameter outside of scope"
fromTypeExpression (RecordUnionTypeExpression expressionRange (Left mutability) recordExpressions) = do
  let addRecord recordMap (recordName, recordTypeParameterExpressions) = do
        recordTypeParameters <- mapM fromTypeExpression recordTypeParameterExpressions
        let (conflictingRecordType, updatedMap) = insertAndReplace recordName recordTypeParameters recordMap
        case conflictingRecordType of
          Just _ -> throwError $ RecordUnionTypeExpressionDuplicateRecordsError expressionRange (getTextName recordName)
          Nothing -> return updatedMap
  records <- foldM addRecord Map.empty recordExpressions
  return $ RecordUnionType mutability records

getRecordFieldTypeFunc :: Bool -> Maybe IBMutabilityParameter -> Seq IBTypeParameter -> IBTypeExpression -> TypeChecker (Mutability -> Seq Type -> Type)
getRecordFieldTypeFunc _ _ _ (IntTypeExpression _) = return $ const . const IntType
getRecordFieldTypeFunc _ _ _ (FloatTypeExpression _) = return $ const . const FloatType
getRecordFieldTypeFunc _ _ _ (CharTypeExpression _) = return $ const . const CharType
getRecordFieldTypeFunc _ _ _ (StringTypeExpression _) = return $ const . const StringType
getRecordFieldTypeFunc _ _ _ (BoolTypeExpression _) = return $ const . const BoolType
getRecordFieldTypeFunc _ _ _ (NilTypeExpression _) = return $ const . const NilType
getRecordFieldTypeFunc _ _ typeParameters (IdentifierTypeExpression expressionRange mutabilityExpression (Left identifier) typeArguments) = do
  unless (null typeArguments) $
    throwError (TypeArrgumentsAppliedToTypeParameterError expressionRange (getTextName identifier))
  unless (mutabilityExpression == Left Immutable) $
    throwError (MutabilityAppliedToTypeParameterError expressionRange (getTextName identifier))
  case Seq.elemIndexL identifier typeParameters of
    Nothing -> return $ const . const $ IdentifierType (getTypeParameterIndex identifier) (getTextName identifier)
    Just paramterIndex -> return $ \_ recordTypeArguments -> Seq.index recordTypeArguments paramterIndex
getRecordFieldTypeFunc atTopLevel maybeMutabilityParameter typeParameters (IdentifierTypeExpression typeSynonymRange mutabilityExpression (Right typeSynonym) synonymTypeArgumentExpressions) = do
  mutabilityFunc <- getRecordFieldTypeMutability typeSynonymRange atTopLevel maybeMutabilityParameter mutabilityExpression
  typeSynonymValueFunc <- getTypeSynonymType typeSynonymRange (Seq.length synonymTypeArgumentExpressions) typeSynonym
  synonymTypeArgumentFuncs <- forM synonymTypeArgumentExpressions $ getRecordFieldTypeFunc False maybeMutabilityParameter typeParameters
  return $ \mutability typeArguments ->
    let synonymTypeArguments = synonymTypeArgumentFuncs <&> \f -> f mutability typeArguments
     in typeSynonymValueFunc (mutabilityFunc mutability) synonymTypeArguments
getRecordFieldTypeFunc _ maybeMutabilityParameter typeParameters (FunctionTypeExpression _ parameterTypeExpressions returnTypeExpression) = do
  parameterTypeFuncs <- mapM (getRecordFieldTypeFunc False maybeMutabilityParameter typeParameters) parameterTypeExpressions
  returnTypeFunc <- getRecordFieldTypeFunc False maybeMutabilityParameter typeParameters returnTypeExpression
  return $ \recordMutability recordTypeArguments ->
    FunctionType (parameterTypeFuncs <&> (\f -> f recordMutability recordTypeArguments)) (returnTypeFunc recordMutability recordTypeArguments)
getRecordFieldTypeFunc atTopLevel maybeMutabilityParameter typeParameters (RecordUnionTypeExpression expressionRange mutabilityExpression recordExpressions) = do
  mutabilityFunc <- getRecordFieldTypeMutability expressionRange atTopLevel maybeMutabilityParameter mutabilityExpression
  let addRecord recordMap (recordName, recordTypeParameterExpressions) = do
        recordTypeParameterFuncs <- mapM (getRecordFieldTypeFunc False maybeMutabilityParameter typeParameters) recordTypeParameterExpressions
        let (conflictingRecordType, updatedMap) = insertAndReplace recordName recordTypeParameterFuncs recordMap
        case conflictingRecordType of
          Just _ -> throwError $ RecordUnionTypeExpressionDuplicateRecordsError expressionRange (getTextName recordName)
          Nothing -> return updatedMap
  recordFuncs <- foldM addRecord Map.empty recordExpressions
  return $ \recordMutability recordTypeParameters -> RecordUnionType (mutabilityFunc recordMutability) (fmap (fmap $ \f -> f recordMutability recordTypeParameters) recordFuncs)

getRecordFieldTypeMutability :: Range -> Bool -> Maybe IBMutabilityParameter -> IBMutabilityExpression -> TypeChecker (Mutability -> Mutability)
getRecordFieldTypeMutability expressionRange atTopLevel maybeMutabilityParameter mutabilityExpression =
  if atTopLevel && isNothing maybeMutabilityParameter
    {- If a record does not explicitly handle mutability with a mutability parameter, mutability is automatically
      propogated to fields of record type.
    -}
    then case mutabilityExpression of
      Left Immutable -> return id
      Left Mutable -> throwError $ RecordFieldExplicitMutabilityError expressionRange
      Right _ -> throwError (ShouldNotGetHereError "Encountered out-of-scope mutability parameter in getRecordFieldTypeFunc")
    else case mutabilityExpression of
      Left mutability -> return $ const mutability
      Right mutabilityIdentifier -> do
        unless (maybeMutabilityParameter == Just mutabilityIdentifier) $
          throwError (ShouldNotGetHereError "Encountered out-of-scope mutability parameter in getRecordFieldTypeFunc")
        return id

getParametrizedTypeFunc :: Maybe IBMutabilityParameter -> Seq IBTypeParameter -> IBTypeExpression -> TypeChecker (Mutability -> Seq Type -> Type)
getParametrizedTypeFunc _ _ (IntTypeExpression _) = return $ const . const IntType
getParametrizedTypeFunc _ _ (FloatTypeExpression _) = return $ const . const FloatType
getParametrizedTypeFunc _ _ (CharTypeExpression _) = return $ const . const CharType
getParametrizedTypeFunc _ _ (StringTypeExpression _) = return $ const . const StringType
getParametrizedTypeFunc _ _ (BoolTypeExpression _) = return $ const . const BoolType
getParametrizedTypeFunc _ _ (NilTypeExpression _) = return $ const . const NilType
getParametrizedTypeFunc _ typeParameters (IdentifierTypeExpression expressionRange mutabilityExpression (Left typeParameterReference) identifierTypeArgumentExpressions) = do
  unless (null identifierTypeArgumentExpressions) $
    throwError (TypeArrgumentsAppliedToTypeParameterError expressionRange (getTextName typeParameterReference))
  unless (mutabilityExpression == Left Immutable) $
    throwError (MutabilityAppliedToTypeParameterError expressionRange (getTextName typeParameterReference))
  case Seq.elemIndexL typeParameterReference typeParameters of
    Nothing -> return $ const . const $ IdentifierType (getTypeParameterIndex typeParameterReference) (getTextName typeParameterReference)
    Just paramterIndex -> return $ \_ functionTypeArguments -> Seq.index functionTypeArguments paramterIndex
getParametrizedTypeFunc maybeMutabilityParameter typeParameters (IdentifierTypeExpression typeSynonymRange mutabilityExpression (Right typeSynonym) synonymTypeArgumentExpressions) = do
  mutabilityFunc <- case mutabilityExpression of
    Left mutability -> return $ const mutability
    Right mutabilityIdentifier | maybeMutabilityParameter == Just mutabilityIdentifier -> return id
    _ -> throwError $ ShouldNotGetHereError "Encountered out of scope mutability parameter"
  typeSynonymValueFunc <- getTypeSynonymType typeSynonymRange (Seq.length synonymTypeArgumentExpressions) typeSynonym
  synonymTypeArgumentFuncs <- forM synonymTypeArgumentExpressions $ getParametrizedTypeFunc maybeMutabilityParameter typeParameters
  return $ \mutability typeArguments ->
    let synonymTypeArguments = synonymTypeArgumentFuncs <&> \f -> f mutability typeArguments
     in typeSynonymValueFunc (mutabilityFunc mutability) synonymTypeArguments
getParametrizedTypeFunc maybeMutabilityParameter typeParameters (FunctionTypeExpression _ functionParameterTypeExpressions functionReturnTypeExpression) = do
  functionParameterTypeFuncs <- forM functionParameterTypeExpressions $ getParametrizedTypeFunc maybeMutabilityParameter typeParameters
  functionReturnTypeFunc <- getParametrizedTypeFunc maybeMutabilityParameter typeParameters functionReturnTypeExpression
  return $ \mutability typeArguments ->
    let functionParameterTypes = functionParameterTypeFuncs <&> \f -> f mutability typeArguments
        functionReturnType = functionReturnTypeFunc mutability typeArguments
     in FunctionType functionParameterTypes functionReturnType
getParametrizedTypeFunc maybeMutabilityParameter typeParameters (RecordUnionTypeExpression expressionRange mutabilityExpression recordExpressions) = do
  mutabilityFunc <- case mutabilityExpression of
    Left mutability -> return $ const mutability
    Right mutabilityIdentifier | maybeMutabilityParameter == Just mutabilityIdentifier -> return id
    _ -> throwError $ ShouldNotGetHereError "Encountered out of scope mutability parameter"
  let addRecord recordMap (recordName, recordTypeArgumentExpressions) = do
        recordTypeArgumentFuncs <- forM recordTypeArgumentExpressions $ getParametrizedTypeFunc maybeMutabilityParameter typeParameters
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

addRecordFieldOrder :: BoundRecordIdentifier -> Seq UnboundIdentifier -> TypeChecker ()
addRecordFieldOrder recordName fields = do
  recordFieldOrders <- recordFieldOrders <$> getState
  setRecordFieldOrders $ Map.insert recordName fields recordFieldOrders

getRecordFieldOrders :: TypeChecker (Map BoundRecordIdentifier (Seq UnboundIdentifier))
getRecordFieldOrders = recordFieldOrders <$> getState

setValueIdentifierTypes :: Map BoundValueIdentifier Type -> TypeChecker ()
setValueIdentifierTypes valueIdentifierTypes = do
  state <- getState
  setState state {valueIdentifierTypes}

setFunctionContextStack :: [FunctionContext] -> TypeChecker ()
setFunctionContextStack functionContextStack = do
  state <- getState
  setState state {functionContextStack}

setRecordFieldOrders :: Map BoundRecordIdentifier (Seq UnboundIdentifier) -> TypeChecker ()
setRecordFieldOrders recordFieldOrders = do
  state <- getState
  setState state {recordFieldOrders}
