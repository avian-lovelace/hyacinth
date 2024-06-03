module IntermediateCodeGeneration.IntermediateCodeGeneration
  ( IntermediateCodeGenerator,
    initialIntermediateCodeGenerationState,
    getIdentifierInContext,
    getFunctionCapturedIdentifiers,
    getFunctionCapturedIdentifiersInContext,
    addFunctionCapturedIdentifiers,
    withCapturedIdentifiers,
    addSubFunction,
    getSubFunctions,
    getCapturedValueBinding,
    getNewFunctionIndex,
    assertIdentifierIsNotCaptured,
    setIdentifierIsUsable,
    getRecordFieldOrder,
    getRecordFieldIndex,
    getNewValueIdentifierIndex,
  )
where

import Core.ErrorState
import Core.Errors
import Core.FilePositions
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import IdentifierBinding.SyntaxTree
import IntermediateCodeGeneration.IntermediateCode
import Parsing.SyntaxTree

type IntermediateCodeGenerator a = ErrorState IntermediateCodeGenerationState a

data IntermediateCodeGenerationState = IntermediateCodeGenerationState
  { capturedIdentifierStack :: [Map BoundValueIdentifier BoundValueIdentifier],
    functionCapturedIdentifiers :: Map BoundFunctionIdentifier (Set BoundValueIdentifier),
    usableValueIdentifiers :: Set BoundValueIdentifier,
    subFunctions :: Map FunctionIndex SubFunc,
    boundValueIdentifierCounter :: Int,
    boundFunctionIdentifierCounter :: Int,
    recordFieldOrders :: Map BoundRecordIdentifier (Seq UnboundIdentifier)
  }

initialIntermediateCodeGenerationState :: Int -> Int -> Map BoundRecordIdentifier (Seq UnboundIdentifier) -> IntermediateCodeGenerationState
initialIntermediateCodeGenerationState boundValueIdentifierCounter boundFunctionIdentifierCounter recordFieldOrders =
  IntermediateCodeGenerationState
    { capturedIdentifierStack = [],
      functionCapturedIdentifiers = Map.empty,
      usableValueIdentifiers = Set.empty,
      subFunctions = Map.empty,
      boundValueIdentifierCounter,
      boundFunctionIdentifierCounter,
      recordFieldOrders
    }

getIdentifierInContext :: Error -> BoundValueIdentifier -> IntermediateCodeGenerator BoundValueIdentifier
getIdentifierInContext identifierUnusableError identifier = do
  capturedIdentifierStack <- capturedIdentifierStack <$> getState
  case capturedIdentifierStack of
    currentIdentifierMap : _ -> case Map.lookup identifier currentIdentifierMap of
      Just innerCapturedIdentifier -> return innerCapturedIdentifier
      Nothing -> do
        assertIdentifierIsUsable identifierUnusableError identifier
        return identifier
    [] -> return identifier

assertIdentifierIsNotCaptured :: Error -> BoundValueIdentifier -> IntermediateCodeGenerator ()
assertIdentifierIsNotCaptured identifierCapturedError identifier = do
  capturedIdentifierStack <- capturedIdentifierStack <$> getState
  case capturedIdentifierStack of
    currentIdentifierMap : _ -> case Map.lookup identifier currentIdentifierMap of
      Just _ -> throwError identifierCapturedError
      Nothing -> return ()
    [] -> return ()

getFunctionCapturedIdentifiers :: BoundFunctionIdentifier -> IntermediateCodeGenerator (Maybe (Set BoundValueIdentifier))
getFunctionCapturedIdentifiers functionIdentifier = Map.lookup functionIdentifier . functionCapturedIdentifiers <$> getState

getFunctionCapturedIdentifiersInContext :: Range -> BoundFunctionIdentifier -> IntermediateCodeGenerator (Seq BoundValueIdentifier)
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

addFunctionCapturedIdentifiers :: BoundFunctionIdentifier -> Set BoundValueIdentifier -> IntermediateCodeGenerator ()
addFunctionCapturedIdentifiers functionIdentifier boundIdentifiers = do
  functionCapturedIdentifiers <- functionCapturedIdentifiers <$> getState
  setFunctionCapturedIdentifiers $ Map.insert functionIdentifier boundIdentifiers functionCapturedIdentifiers

withCapturedIdentifiers :: Set BoundValueIdentifier -> IntermediateCodeGenerator a -> IntermediateCodeGenerator (a, Seq BoundValueIdentifier)
withCapturedIdentifiers capturedIdentifiers generator =
  do
    capturedIdentifierPairs <- mapM toCapturedIdentifierPair (Set.toAscList capturedIdentifiers)
    let capturedIdentifierMap = Map.fromAscList capturedIdentifierPairs
    pushCapturedIdentifierMap capturedIdentifierMap
    result <- generator
    return (result, Seq.fromList $ snd <$> capturedIdentifierPairs)
    `andFinally` popCapturedIdentifierMap
  where
    toCapturedIdentifierPair outerIdentifer = do
      innerIdentifier <- getCapturedValueBinding outerIdentifer
      return (outerIdentifer, innerIdentifier)

pushCapturedIdentifierMap :: Map BoundValueIdentifier BoundValueIdentifier -> IntermediateCodeGenerator ()
pushCapturedIdentifierMap capturedIdentifierMap = do
  capturedIdentifierStack <- capturedIdentifierStack <$> getState
  setCapturedIdentifierStack $ capturedIdentifierMap : capturedIdentifierStack

popCapturedIdentifierMap :: IntermediateCodeGenerator ()
popCapturedIdentifierMap = do
  capturedIdentifierStack <- capturedIdentifierStack <$> getState
  case capturedIdentifierStack of
    [] -> throwError $ ShouldNotGetHereError "called popCapturedIdentifierMap when capturedIdentifierStack was empty"
    _capturedIdentifierMap : restStack -> do
      setCapturedIdentifierStack restStack
      return ()

setIdentifierIsUsable :: BoundValueIdentifier -> IntermediateCodeGenerator ()
setIdentifierIsUsable valueIdentifier = do
  usableValueIdentifiers <- usableValueIdentifiers <$> getState
  setUsableValueIdentifiers $ Set.insert valueIdentifier usableValueIdentifiers

assertIdentifierIsUsable :: Error -> BoundValueIdentifier -> IntermediateCodeGenerator ()
assertIdentifierIsUsable identifierUnusableError valueIdentifier = do
  usableValueIdentifiers <- usableValueIdentifiers <$> getState
  if Set.member valueIdentifier usableValueIdentifiers
    then return ()
    else throwError identifierUnusableError

addSubFunction :: FunctionIndex -> SubFunc -> IntermediateCodeGenerator ()
addSubFunction functionIndex subFunction = do
  subFunctions <- subFunctions <$> getState
  setSubFunctions $ Map.insert functionIndex subFunction subFunctions

getSubFunctions :: IntermediateCodeGenerator (Seq SubFunc)
getSubFunctions = do
  subFunctionMap <- subFunctions <$> getState
  subFunctions <- mapM (getFunctionWithIndex subFunctionMap) [1 .. Map.size subFunctionMap]
  return $ Seq.fromList subFunctions
  where
    getFunctionWithIndex :: Map FunctionIndex SubFunc -> FunctionIndex -> IntermediateCodeGenerator SubFunc
    getFunctionWithIndex subFunctionMap functionIndex = case Map.lookup functionIndex subFunctionMap of
      Nothing -> throwError $ ShouldNotGetHereError $ "function with index " ++ show functionIndex ++ " was missing in getSubFunctions"
      Just subFunction -> return subFunction

getCapturedValueBinding :: BoundValueIdentifier -> IntermediateCodeGenerator BoundValueIdentifier
getCapturedValueBinding (BoundValueIdentifier _ identifierName) = do
  valueIdentifierIndex <- getNewValueIdentifierIndex
  return $ BoundValueIdentifier valueIdentifierIndex identifierName

getNewValueIdentifierIndex :: IntermediateCodeGenerator ValueIdentifierIndex
getNewValueIdentifierIndex = do
  valueIdentifierIndex <- boundValueIdentifierCounter <$> getState
  setBoundValueIdentifierCounter $ valueIdentifierIndex + 1
  return valueIdentifierIndex

getNewFunctionIndex :: IntermediateCodeGenerator FunctionIndex
getNewFunctionIndex = do
  functionIndex <- boundFunctionIdentifierCounter <$> getState
  setBoundFunctionIdentifierCounter $ functionIndex + 1
  return functionIndex

getRecordFieldOrder :: BoundRecordIdentifier -> IntermediateCodeGenerator (Seq UnboundIdentifier)
getRecordFieldOrder recordName = do
  recordFieldOrders <- recordFieldOrders <$> getState
  case Map.lookup recordName recordFieldOrders of
    Nothing -> throwError $ ShouldNotGetHereError "Failed to find record in getRecordFieldOrder"
    Just fieldOrder -> return fieldOrder

getRecordFieldIndex :: BoundRecordIdentifier -> UnboundIdentifier -> IntermediateCodeGenerator FieldIndex
getRecordFieldIndex recordName fieldName = do
  fieldOrder <- getRecordFieldOrder recordName
  case Seq.elemIndexL fieldName fieldOrder of
    Nothing -> throwError $ ShouldNotGetHereError "Failed to find field in getRecordFieldIndex"
    Just index -> return index

setCapturedIdentifierStack :: [Map BoundValueIdentifier BoundValueIdentifier] -> ErrorState IntermediateCodeGenerationState ()
setCapturedIdentifierStack capturedIdentifierStack = do
  state <- getState
  setState state {capturedIdentifierStack}

setFunctionCapturedIdentifiers :: Map BoundFunctionIdentifier (Set BoundValueIdentifier) -> ErrorState IntermediateCodeGenerationState ()
setFunctionCapturedIdentifiers functionCapturedIdentifiers = do
  state <- getState
  setState state {functionCapturedIdentifiers}

setUsableValueIdentifiers :: Set BoundValueIdentifier -> ErrorState IntermediateCodeGenerationState ()
setUsableValueIdentifiers usableValueIdentifiers = do
  state <- getState
  setState state {usableValueIdentifiers}

setSubFunctions :: Map FunctionIndex SubFunc -> ErrorState IntermediateCodeGenerationState ()
setSubFunctions subFunctions = do
  state <- getState
  setState state {subFunctions}

setBoundValueIdentifierCounter :: Int -> ErrorState IntermediateCodeGenerationState ()
setBoundValueIdentifierCounter boundValueIdentifierCounter = do
  state <- getState
  setState state {boundValueIdentifierCounter}

setBoundFunctionIdentifierCounter :: Int -> ErrorState IntermediateCodeGenerationState ()
setBoundFunctionIdentifierCounter boundFunctionIdentifierCounter = do
  state <- getState
  setState state {boundFunctionIdentifierCounter}