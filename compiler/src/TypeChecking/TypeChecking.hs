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
  )
where

import Control.Monad (unless)
import Core.ErrorState
import Core.Errors
import Core.FilePositions
import Core.SyntaxTree
import Data.Foldable1 (Foldable1, foldlM1)
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Traversable (forM)
import IdentifierBinding.SyntaxTree
import Parsing.SyntaxTree
import TypeChecking.Type

data TypeCheckingState = TypeCheckingState
  { valueIdentifierTypes :: Map BoundValueIdentifier Type,
    functionTypes :: Map BoundFunctionIdentifier (Int, Seq Type -> Type),
    recordTypes :: Map BoundRecordIdentifier (Map UnboundIdentifier (Mutability -> Seq Type -> Type)),
    functionContextStack :: [FunctionContext],
    recordFieldOrders :: Map BoundRecordIdentifier (Seq UnboundIdentifier),
    recordTypeParameterVariances :: Map BoundRecordIdentifier (Seq Variance, Seq Variance)
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
      functionContextStack = [],
      recordFieldOrders = Map.empty,
      recordTypeParameterVariances = Map.empty
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
  functionTypes <- functionTypes <$> getState
  let updatedFunctionTypes = Map.insert functionName (numTypeParameters, functionTypeFunc) functionTypes
  setFunctionTypes updatedFunctionTypes

getFunctionType :: Range -> BoundFunctionIdentifier -> Seq Type -> TypeChecker Type
getFunctionType usageRange functionName typeArguments = do
  functionTypes <- functionTypes <$> getState
  case Map.lookup functionName functionTypes of
    Just (numTypeParameters, functionTypeFunc) -> do
      unless (Seq.length typeArguments == numTypeParameters) $
        throwError (FunctionWrongNumberOfTypeArgumentsError usageRange (getTextName functionName) numTypeParameters (Seq.length typeArguments))
      return $ functionTypeFunc typeArguments
    Nothing -> throwError $ ShouldNotGetHereError "Called getFunctionType before function was initialized"

setRecordFieldTypes :: BoundRecordIdentifier -> Map UnboundIdentifier (Mutability -> Seq Type -> Type) -> TypeChecker ()
setRecordFieldTypes recordName fieldTypeMap = do
  recordTypes <- recordTypes <$> getState
  let updatedRecordTypes = Map.insert recordName fieldTypeMap recordTypes
  setRecordTypes updatedRecordTypes

getRecordFieldTypes :: BoundRecordIdentifier -> Mutability -> Seq Type -> TypeChecker (Map UnboundIdentifier Type)
getRecordFieldTypes recordName mutability typeParameters = do
  recordTypes <- recordTypes <$> getState
  case Map.lookup recordName recordTypes of
    Nothing -> throwError $ ShouldNotGetHereError "Called getFunctionType before function was initialized"
    Just fieldTypeMap -> return $ fieldTypeMap <&> \f -> f mutability typeParameters

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

setFunctionTypes :: Map BoundFunctionIdentifier (Int, Seq Type -> Type) -> TypeChecker ()
setFunctionTypes functionTypes = do
  state <- getState
  setState state {functionTypes}

setRecordTypes :: Map BoundRecordIdentifier (Map UnboundIdentifier (Mutability -> Seq Type -> Type)) -> TypeChecker ()
setRecordTypes recordTypes = do
  state <- getState
  setState state {recordTypes}

setFunctionContextStack :: [FunctionContext] -> TypeChecker ()
setFunctionContextStack functionContextStack = do
  state <- getState
  setState state {functionContextStack}

setRecordFieldOrders :: Map BoundRecordIdentifier (Seq UnboundIdentifier) -> TypeChecker ()
setRecordFieldOrders recordFieldOrders = do
  state <- getState
  setState state {recordFieldOrders}
