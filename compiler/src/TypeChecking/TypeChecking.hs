module TypeChecking.TypeChecking
  ( TypeChecker,
    FunctionContext (MainFunctionContext, FunctionContext, contextReturnType, contextReturnTypeRange),
    setIdentifierType,
    getIdentifierType,
    getFunctionContext,
    setFunctionContext,
    getFunctionType,
    setFunctionType,
    pushCapturedIdentifierTypes,
    popCapturedIdentifierTypes,
    addCheckedFunction,
    getCheckedFunctions,
    initialTypeCheckingState,
  )
where

import Core.ErrorState
import Core.Errors
import Core.FilePositions
import Core.Type
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq (Empty, (:<|)), (|>))
import qualified Data.Sequence as Seq
import IdentifierBinding.SyntaxTree
import TypeChecking.SyntaxTree

data TypeCheckingState = TypeCheckingState
  { identifierTypes :: Map BoundIdentifier Type,
    functionTypes :: Map FunctionIndex Type,
    capturedIdentifierTypes :: Seq (FunctionIndex, Seq Type),
    functionContext :: FunctionContext,
    checkedFunctions :: Map FunctionIndex TCFunctionDefinition
  }

data FunctionContext
  = MainFunctionContext
  | FunctionContext {contextReturnType :: Type, contextReturnTypeRange :: Range}

type TypeChecker = ErrorState TypeCheckingState

initialTypeCheckingState :: TypeCheckingState
initialTypeCheckingState =
  TypeCheckingState
    { identifierTypes = Map.empty,
      functionTypes = Map.empty,
      capturedIdentifierTypes = Seq.Empty,
      functionContext = MainFunctionContext,
      checkedFunctions = Map.empty
    }

setIdentifierType :: BoundIdentifier -> Type -> TypeChecker ()
setIdentifierType identifier identifierType = do
  identifierTypes <- identifierTypes <$> getState
  setIdentifierTypes $ Map.insert identifier identifierType identifierTypes

getIdentifierType :: BoundIdentifier -> TypeChecker Type
getIdentifierType identifier = do
  identifierTypes <- identifierTypes <$> getState
  case Map.lookup identifier identifierTypes of
    Just identifierType -> return identifierType
    Nothing -> throwError $ ShouldNotGetHereError "Called getIdentifierType before identifier type was set"

setFunctionType :: FunctionIndex -> Type -> TypeChecker ()
setFunctionType functionIndex functionType = do
  functionTypes <- functionTypes <$> getState
  setFunctionTypes $ Map.insert functionIndex functionType functionTypes

getFunctionType :: FunctionIndex -> TypeChecker Type
getFunctionType functionIndex = do
  functionTypes <- functionTypes <$> getState
  case Map.lookup functionIndex functionTypes of
    Just functionType -> return functionType
    Nothing -> throwError $ ShouldNotGetHereError "Failed to find function type with getFunctionType"

pushCapturedIdentifierTypes :: FunctionIndex -> Seq Type -> TypeChecker ()
pushCapturedIdentifierTypes functionIndex types = do
  capturedIdentifierTypes <- capturedIdentifierTypes <$> getState
  setCapturedIdentifierTypes $ capturedIdentifierTypes |> (functionIndex, types)

popCapturedIdentifierTypes :: TypeChecker (Maybe (FunctionIndex, Seq Type))
popCapturedIdentifierTypes = do
  capturedIdentifierTypes <- capturedIdentifierTypes <$> getState
  case capturedIdentifierTypes of
    (firstTypes :<| restTypes) -> do
      setCapturedIdentifierTypes restTypes
      return $ Just firstTypes
    Empty -> return Nothing

getFunctionContext :: TypeChecker FunctionContext
getFunctionContext = functionContext <$> getState

addCheckedFunction :: FunctionIndex -> TCFunctionDefinition -> TypeChecker ()
addCheckedFunction functionIndex checkedFunction = do
  checkedFunctions <- checkedFunctions <$> getState
  setCheckedFunctions $ Map.insert functionIndex checkedFunction checkedFunctions

getCheckedFunctions :: Int -> TypeChecker (Seq TCFunctionDefinition)
getCheckedFunctions numFunctions = do
  checkedFunctions <- checkedFunctions <$> getState
  mapM
    ( \functionIndex -> case Map.lookup functionIndex checkedFunctions of
        Nothing -> throwError $ ShouldNotGetHereError "Failed to find checked function in getCheckedFunctions"
        Just functionDefinition -> return functionDefinition
    )
    (Seq.fromList [1 .. numFunctions])

setIdentifierTypes :: Map BoundIdentifier Type -> TypeChecker ()
setIdentifierTypes identifierTypes = do
  TypeCheckingState {functionTypes, capturedIdentifierTypes, functionContext, checkedFunctions} <- getState
  setState TypeCheckingState {identifierTypes, functionTypes, capturedIdentifierTypes, functionContext, checkedFunctions}

setFunctionTypes :: Map FunctionIndex Type -> TypeChecker ()
setFunctionTypes functionTypes = do
  TypeCheckingState {identifierTypes, capturedIdentifierTypes, functionContext, checkedFunctions} <- getState
  setState TypeCheckingState {identifierTypes, functionTypes, capturedIdentifierTypes, functionContext, checkedFunctions}

setCapturedIdentifierTypes :: Seq (FunctionIndex, Seq Type) -> TypeChecker ()
setCapturedIdentifierTypes capturedIdentifierTypes = do
  TypeCheckingState {identifierTypes, functionTypes, functionContext, checkedFunctions} <- getState
  setState TypeCheckingState {identifierTypes, functionTypes, capturedIdentifierTypes, functionContext, checkedFunctions}

setFunctionContext :: FunctionContext -> TypeChecker ()
setFunctionContext functionContext = do
  TypeCheckingState {identifierTypes, functionTypes, capturedIdentifierTypes, checkedFunctions} <- getState
  setState TypeCheckingState {identifierTypes, functionTypes, capturedIdentifierTypes, functionContext, checkedFunctions}

setCheckedFunctions :: Map FunctionIndex TCFunctionDefinition -> TypeChecker ()
setCheckedFunctions checkedFunctions = do
  TypeCheckingState {identifierTypes, functionTypes, capturedIdentifierTypes, functionContext} <- getState
  setState TypeCheckingState {identifierTypes, functionTypes, capturedIdentifierTypes, functionContext, checkedFunctions}