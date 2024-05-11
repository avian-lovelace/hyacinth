module TypeChecking.TypeChecking
  ( TypeChecker,
    FunctionContext (FunctionContext, contextReturnType, contextReturnTypeRange),
    setValueIdentifierType,
    getValueIdentifierType,
    getFunctionContext,
    initialTypeCheckingState,
    setFunctionType,
    getFunctionType,
    withFunctionContext,
  )
where

import Core.ErrorState
import Core.Errors
import Core.FilePositions
import Core.Type
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import IdentifierBinding.SyntaxTree

data TypeCheckingState = TypeCheckingState
  { valueIdentifierTypes :: Map BoundValueIdentifier Type,
    functionTypes :: Map BoundFunctionIdentifier Type,
    functionContextStack :: [FunctionContext]
  }

data FunctionContext = FunctionContext {contextReturnType :: Type, contextReturnTypeRange :: Range}

type TypeChecker = ErrorState TypeCheckingState

initialTypeCheckingState :: TypeCheckingState
initialTypeCheckingState =
  TypeCheckingState
    { valueIdentifierTypes = Map.empty,
      functionTypes = Map.empty,
      functionContextStack = []
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

setValueIdentifierTypes :: Map BoundValueIdentifier Type -> TypeChecker ()
setValueIdentifierTypes valueIdentifierTypes = do
  TypeCheckingState {functionTypes, functionContextStack} <- getState
  setState TypeCheckingState {valueIdentifierTypes, functionTypes, functionContextStack}

setFunctionTypes :: Map BoundFunctionIdentifier Type -> TypeChecker ()
setFunctionTypes functionTypes = do
  TypeCheckingState {valueIdentifierTypes, functionContextStack} <- getState
  setState TypeCheckingState {valueIdentifierTypes, functionTypes, functionContextStack}

setFunctionContextStack :: [FunctionContext] -> TypeChecker ()
setFunctionContextStack functionContextStack = do
  TypeCheckingState {valueIdentifierTypes, functionTypes} <- getState
  setState TypeCheckingState {valueIdentifierTypes, functionTypes, functionContextStack}
