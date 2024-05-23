module TypeChecking.TypeChecking
  ( TypeChecker,
    TypeCheckingState (recordFieldOrders),
    FunctionContext (FunctionContext, contextReturnType, contextReturnTypeRange),
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
  )
where

import Core.ErrorState
import Core.Errors
import Core.FilePositions
import Core.SyntaxTree
import Core.Type
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Data.Sequence (Seq)
import IdentifierBinding.SyntaxTree
import Parsing.SyntaxTree

data TypeCheckingState = TypeCheckingState
  { valueIdentifierTypes :: Map BoundValueIdentifier Type,
    functionTypes :: Map BoundFunctionIdentifier Type,
    recordTypes :: Map BoundRecordIdentifier (Map UnboundIdentifier (Mutability -> Type)),
    functionContextStack :: [FunctionContext],
    recordFieldOrders :: Map BoundRecordIdentifier (Seq UnboundIdentifier)
  }

data FunctionContext = FunctionContext {contextReturnType :: Type, contextReturnTypeRange :: Range}

type TypeChecker = ErrorState TypeCheckingState

initialTypeCheckingState :: TypeCheckingState
initialTypeCheckingState =
  TypeCheckingState
    { valueIdentifierTypes = Map.empty,
      functionTypes = Map.empty,
      recordTypes = Map.empty,
      functionContextStack = [],
      recordFieldOrders = Map.empty
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

setFunctionType :: BoundFunctionIdentifier -> Type -> TypeChecker ()
setFunctionType functionName functionType = do
  functionTypes <- functionTypes <$> getState
  let updatedFunctionTypes = Map.insert functionName functionType functionTypes
  setFunctionTypes updatedFunctionTypes

getFunctionType :: BoundFunctionIdentifier -> TypeChecker Type
getFunctionType functionName = do
  functionTypes <- functionTypes <$> getState
  case Map.lookup functionName functionTypes of
    Just functionType -> return functionType
    Nothing -> throwError $ ShouldNotGetHereError "Called getFunctionType before function was initialized"

setRecordFieldTypes :: BoundRecordIdentifier -> Map UnboundIdentifier (Mutability -> Type) -> TypeChecker ()
setRecordFieldTypes recordName fieldTypeMap = do
  recordTypes <- recordTypes <$> getState
  let updatedRecordTypes = Map.insert recordName fieldTypeMap recordTypes
  setRecordTypes updatedRecordTypes

getRecordFieldTypes :: BoundRecordIdentifier -> TypeChecker (Map UnboundIdentifier (Mutability -> Type))
getRecordFieldTypes recordName = do
  recordTypes <- recordTypes <$> getState
  case Map.lookup recordName recordTypes of
    Nothing -> throwError $ ShouldNotGetHereError "Called getFunctionType before function was initialized"
    Just fieldTypeMap -> return fieldTypeMap

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

addRecordFieldOrder :: BoundRecordIdentifier -> Seq UnboundIdentifier -> TypeChecker ()
addRecordFieldOrder recordName fields = do
  recordFieldOrders <- recordFieldOrders <$> getState
  setRecordFieldOrders $ Map.insert recordName fields recordFieldOrders

getRecordFieldOrders :: TypeChecker (Map BoundRecordIdentifier (Seq UnboundIdentifier))
getRecordFieldOrders = recordFieldOrders <$> getState

setValueIdentifierTypes :: Map BoundValueIdentifier Type -> TypeChecker ()
setValueIdentifierTypes valueIdentifierTypes = do
  TypeCheckingState {functionTypes, functionContextStack, recordTypes, recordFieldOrders} <- getState
  setState TypeCheckingState {valueIdentifierTypes, functionTypes, functionContextStack, recordTypes, recordFieldOrders}

setFunctionTypes :: Map BoundFunctionIdentifier Type -> TypeChecker ()
setFunctionTypes functionTypes = do
  TypeCheckingState {valueIdentifierTypes, functionContextStack, recordTypes, recordFieldOrders} <- getState
  setState TypeCheckingState {valueIdentifierTypes, functionTypes, functionContextStack, recordTypes, recordFieldOrders}

setRecordTypes :: Map BoundRecordIdentifier (Map UnboundIdentifier (Mutability -> Type)) -> TypeChecker ()
setRecordTypes recordTypes = do
  TypeCheckingState {valueIdentifierTypes, functionTypes, functionContextStack, recordFieldOrders} <- getState
  setState TypeCheckingState {valueIdentifierTypes, functionTypes, functionContextStack, recordTypes, recordFieldOrders}

setFunctionContextStack :: [FunctionContext] -> TypeChecker ()
setFunctionContextStack functionContextStack = do
  TypeCheckingState {valueIdentifierTypes, functionTypes, recordTypes, recordFieldOrders} <- getState
  setState TypeCheckingState {valueIdentifierTypes, functionTypes, functionContextStack, recordTypes, recordFieldOrders}

setRecordFieldOrders :: Map BoundRecordIdentifier (Seq UnboundIdentifier) -> TypeChecker ()
setRecordFieldOrders recordFieldOrders = do
  TypeCheckingState {valueIdentifierTypes, functionTypes, recordTypes, functionContextStack} <- getState
  setState TypeCheckingState {valueIdentifierTypes, functionTypes, functionContextStack, recordTypes, recordFieldOrders}
