module FunctionLifting.FunctionLifting
  ( FunctionLifter,
    initialFunctionLiftingState,
    getIdentifierInContext,
    getFunctionCapturedIdentifiers,
    getFunctionCapturedIdentifiersInContext,
    addFunctionCapturedIdentifiers,
    withCapturedIdentifiers,
    addLiftedFunction,
    getLiftedFunctions,
    getCapturedValueBinding,
    getNewFunctionIndex,
    assertIdentifierIsNotCaptured,
    setIdentifierIsUsable,
    getRecordFieldOrder,
    getRecordFieldIndex,
  )
where

import Core.ErrorState
import Core.Errors
import Core.FilePositions
import Core.SyntaxTree
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import FunctionLifting.SyntaxTree
import IdentifierBinding.SyntaxTree
import Parsing.SyntaxTree

type FunctionLifter a = ErrorState FunctionLiftingState a

data FunctionLiftingState = FunctionLiftingState
  { capturedIdentifierStack :: [Map BoundValueIdentifier BoundValueIdentifier],
    functionCapturedIdentifiers :: Map BoundFunctionIdentifier (Set BoundValueIdentifier),
    usableValueIdentifiers :: Set BoundValueIdentifier,
    liftedFunctions :: Map FunctionIndex FLSubFunction,
    boundValueIdentifierCounter :: Int,
    boundFunctionIdentifierCounter :: Int,
    recordFieldOrders :: Map BoundRecordIdentifier (Seq UnboundIdentifier)
  }

initialFunctionLiftingState :: Int -> Int -> Map BoundRecordIdentifier (Seq UnboundIdentifier) -> FunctionLiftingState
initialFunctionLiftingState boundValueIdentifierCounter boundFunctionIdentifierCounter recordFieldOrders =
  FunctionLiftingState
    { capturedIdentifierStack = [],
      functionCapturedIdentifiers = Map.empty,
      usableValueIdentifiers = Set.empty,
      liftedFunctions = Map.empty,
      boundValueIdentifierCounter,
      boundFunctionIdentifierCounter,
      recordFieldOrders
    }

getIdentifierInContext :: Error -> BoundValueIdentifier -> FunctionLifter BoundValueIdentifier
getIdentifierInContext identifierUnusableError identifier = do
  capturedIdentifierStack <- capturedIdentifierStack <$> getState
  case capturedIdentifierStack of
    currentIdentifierMap : _ -> case Map.lookup identifier currentIdentifierMap of
      Just innerCapturedIdentifier -> return innerCapturedIdentifier
      Nothing -> do
        assertIdentifierIsUsable identifierUnusableError identifier
        return identifier
    [] -> return identifier

assertIdentifierIsNotCaptured :: Error -> BoundValueIdentifier -> FunctionLifter ()
assertIdentifierIsNotCaptured identifierCapturedError identifier = do
  capturedIdentifierStack <- capturedIdentifierStack <$> getState
  case capturedIdentifierStack of
    currentIdentifierMap : _ -> case Map.lookup identifier currentIdentifierMap of
      Just _ -> throwError identifierCapturedError
      Nothing -> return ()
    [] -> return ()

getFunctionCapturedIdentifiers :: BoundFunctionIdentifier -> FunctionLifter (Maybe (Set BoundValueIdentifier))
getFunctionCapturedIdentifiers functionIdentifier = Map.lookup functionIdentifier . functionCapturedIdentifiers <$> getState

getFunctionCapturedIdentifiersInContext :: Range -> BoundFunctionIdentifier -> FunctionLifter (Seq BoundValueIdentifier)
getFunctionCapturedIdentifiersInContext captureRange functionIdentifier = do
  maybeOuterCapturedIdentifiers <- getFunctionCapturedIdentifiers functionIdentifier
  case maybeOuterCapturedIdentifiers of
    Nothing -> throwError $ ShouldNotGetHereError "Failed to get captured identifiers list in getFunctionCapturedIdentifiersInContext"
    Just outerCapturedIdentifiers -> do
      let orderedCapturedIdentifiers = Set.toAscList outerCapturedIdentifiers
      let processIdentifier identifier = do
            let (BoundFunctionIdentifier _ functionName) = functionIdentifier
            let (BoundValueIdentifier _ identifierName) = identifier
            let identifierUnusableError = IdentifierUndefinedBeforeCaptureError functionName identifierName captureRange
            getIdentifierInContext identifierUnusableError identifier
      orderedCapturedIdentifiersInContext <- mapM processIdentifier orderedCapturedIdentifiers
      return $ Seq.fromList orderedCapturedIdentifiersInContext

addFunctionCapturedIdentifiers :: BoundFunctionIdentifier -> Set BoundValueIdentifier -> FunctionLifter ()
addFunctionCapturedIdentifiers functionIdentifier boundIdentifiers = do
  functionCapturedIdentifiers <- functionCapturedIdentifiers <$> getState
  setFunctionCapturedIdentifiers $ Map.insert functionIdentifier boundIdentifiers functionCapturedIdentifiers

withCapturedIdentifiers :: Set BoundValueIdentifier -> FunctionLifter a -> FunctionLifter (a, Seq BoundValueIdentifier)
withCapturedIdentifiers capturedIdentifiers lifter =
  do
    capturedIdentifierPairs <- mapM toCapturedIdentifierPair (Set.toAscList capturedIdentifiers)
    let capturedIdentifierMap = Map.fromAscList capturedIdentifierPairs
    pushCapturedIdentifierMap capturedIdentifierMap
    result <- lifter
    return (result, Seq.fromList $ snd <$> capturedIdentifierPairs)
    `andFinally` popCapturedIdentifierMap
  where
    toCapturedIdentifierPair outerIdentifer = do
      innerIdentifier <- getCapturedValueBinding outerIdentifer
      return (outerIdentifer, innerIdentifier)

pushCapturedIdentifierMap :: Map BoundValueIdentifier BoundValueIdentifier -> FunctionLifter ()
pushCapturedIdentifierMap capturedIdentifierMap = do
  capturedIdentifierStack <- capturedIdentifierStack <$> getState
  setCapturedIdentifierStack $ capturedIdentifierMap : capturedIdentifierStack

popCapturedIdentifierMap :: FunctionLifter ()
popCapturedIdentifierMap = do
  capturedIdentifierStack <- capturedIdentifierStack <$> getState
  case capturedIdentifierStack of
    [] -> throwError $ ShouldNotGetHereError "called popCapturedIdentifierMap when capturedIdentifierStack was empty"
    _capturedIdentifierMap : restStack -> do
      setCapturedIdentifierStack restStack
      return ()

setIdentifierIsUsable :: BoundValueIdentifier -> FunctionLifter ()
setIdentifierIsUsable valueIdentifier = do
  usableValueIdentifiers <- usableValueIdentifiers <$> getState
  setUsableValueIdentifiers $ Set.insert valueIdentifier usableValueIdentifiers

assertIdentifierIsUsable :: Error -> BoundValueIdentifier -> FunctionLifter ()
assertIdentifierIsUsable identifierUnusableError valueIdentifier = do
  usableValueIdentifiers <- usableValueIdentifiers <$> getState
  if Set.member valueIdentifier usableValueIdentifiers
    then return ()
    else throwError identifierUnusableError

addLiftedFunction :: FunctionIndex -> FLSubFunction -> FunctionLifter ()
addLiftedFunction functionIndex subFunction = do
  liftedFunctions <- liftedFunctions <$> getState
  setLiftedFunctions $ Map.insert functionIndex subFunction liftedFunctions

getLiftedFunctions :: FunctionLifter (Seq FLSubFunction)
getLiftedFunctions = do
  liftedFunctionMap <- liftedFunctions <$> getState
  liftedFunctions <- mapM (getFunctionWithIndex liftedFunctionMap) [1 .. Map.size liftedFunctionMap]
  return $ Seq.fromList liftedFunctions
  where
    getFunctionWithIndex :: Map FunctionIndex FLSubFunction -> FunctionIndex -> FunctionLifter FLSubFunction
    getFunctionWithIndex liftedFunctionMap functionIndex = case Map.lookup functionIndex liftedFunctionMap of
      Nothing -> throwError $ ShouldNotGetHereError $ "function with index " ++ show functionIndex ++ " was missing in getLiftedFunctions"
      Just subFunction -> return subFunction

getCapturedValueBinding :: BoundValueIdentifier -> FunctionLifter BoundValueIdentifier
getCapturedValueBinding (BoundValueIdentifier _ identifierName) = do
  valueIdentifierIndex <- boundValueIdentifierCounter <$> getState
  setBoundValueIdentifierCounter $ valueIdentifierIndex + 1
  return $ BoundValueIdentifier valueIdentifierIndex identifierName

getNewFunctionIndex :: FunctionLifter FunctionIndex
getNewFunctionIndex = do
  functionIndex <- boundFunctionIdentifierCounter <$> getState
  setBoundFunctionIdentifierCounter $ functionIndex + 1
  return functionIndex

getRecordFieldOrder :: BoundRecordIdentifier -> FunctionLifter (Seq UnboundIdentifier)
getRecordFieldOrder recordName = do
  recordFieldOrders <- recordFieldOrders <$> getState
  case Map.lookup recordName recordFieldOrders of
    Nothing -> throwError $ ShouldNotGetHereError "Failed to find record in getRecordFieldOrder"
    Just fieldOrder -> return fieldOrder

getRecordFieldIndex :: BoundRecordIdentifier -> UnboundIdentifier -> FunctionLifter FieldIndex
getRecordFieldIndex recordName fieldName = do
  fieldOrder <- getRecordFieldOrder recordName
  case Seq.elemIndexL fieldName fieldOrder of
    Nothing -> throwError $ ShouldNotGetHereError "Failed to find field in getRecordFieldIndex"
    Just index -> return index

setCapturedIdentifierStack :: [Map BoundValueIdentifier BoundValueIdentifier] -> ErrorState FunctionLiftingState ()
setCapturedIdentifierStack capturedIdentifierStack = do
  state <- getState
  setState state {capturedIdentifierStack}

setFunctionCapturedIdentifiers :: Map BoundFunctionIdentifier (Set BoundValueIdentifier) -> ErrorState FunctionLiftingState ()
setFunctionCapturedIdentifiers functionCapturedIdentifiers = do
  state <- getState
  setState state {functionCapturedIdentifiers}

setUsableValueIdentifiers :: Set BoundValueIdentifier -> ErrorState FunctionLiftingState ()
setUsableValueIdentifiers usableValueIdentifiers = do
  state <- getState
  setState state {usableValueIdentifiers}

setLiftedFunctions :: Map FunctionIndex FLSubFunction -> ErrorState FunctionLiftingState ()
setLiftedFunctions liftedFunctions = do
  state <- getState
  setState state {liftedFunctions}

setBoundValueIdentifierCounter :: Int -> ErrorState FunctionLiftingState ()
setBoundValueIdentifierCounter boundValueIdentifierCounter = do
  state <- getState
  setState state {boundValueIdentifierCounter}

setBoundFunctionIdentifierCounter :: Int -> ErrorState FunctionLiftingState ()
setBoundFunctionIdentifierCounter boundFunctionIdentifierCounter = do
  state <- getState
  setState state {boundFunctionIdentifierCounter}