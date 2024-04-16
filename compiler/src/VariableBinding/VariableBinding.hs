module VariableBinding.VariableBinding
  ( VariableBinder (VariableBinder, runBinder),
    VariableInfo (VariableInfo, boundIdentifier, declarationRange),
    traverse',
    addVariable,
    getVariableInfo,
    assertHasValue,
    initialBindingState,
    setVariableIsDoneBeingDeclared,
    setVariableIsBeingDeclared,
    assertVariableIsNotBeingDeclared,
    andFinally,
  )
where

import Core.Errors
import Core.FilePositions
import Data.Bifunctor (Bifunctor (second))
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq (Empty), (<|), (|>))
import Data.Set (Set)
import qualified Data.Set as Set
import Parsing.SyntaxTree
import VariableBinding.SyntaxTree

data VariableBinder a = VariableBinder {runBinder :: VariableBindingState -> (VariableBindingState, WithErrors a)} deriving (Functor)

instance Applicative VariableBinder where
  pure a = VariableBinder $ \state -> (state, Success a)
  (<*>) binderF binderA = VariableBinder $ \state ->
    case runBinder binderF state of
      (newState, Success f) -> runBinder (fmap f binderA) newState
      (newState, Error e) -> (newState, Error e)

instance Monad VariableBinder where
  binderA >>= makeBinderB = VariableBinder $ \state ->
    case runBinder binderA state of
      (newState, Success a) -> runBinder (makeBinderB a) newState
      (newState, Error e) -> (newState, Error e)

data VariableBindingState = VariableBindingState
  { scopes :: [Scope],
    boundIdentifierCounter :: Int,
    variablesInDeclaration :: Set UnboundIdentifier
  }

newtype Scope = Scope {variables :: Map UnboundIdentifier VariableInfo}

data VariableInfo = VariableInfo {boundIdentifier :: BoundIdentifier, declarationRange :: Range}

initialBindingState :: VariableBindingState
initialBindingState =
  VariableBindingState
    { scopes = singleton Scope {variables = Map.empty},
      boundIdentifierCounter = 0,
      variablesInDeclaration = Set.empty
    }

addVariable :: UnboundIdentifier -> Range -> VariableBinder (BoundIdentifier)
addVariable identifier dRange = do
  state <- getState
  case scopes state of
    [] -> throwBindingError $ ShouldNotGetHereError ""
    Scope {variables} : restScopes -> case Map.lookup identifier variables of
      Just conflictingInfo ->
        throwBindingError $
          ConflictingVariableDeclarationsError identifier (declarationRange conflictingInfo) dRange
      Nothing -> do
        newBoundIdentifier <- getNewBoundIdentifier
        let variableInfo = VariableInfo {boundIdentifier = newBoundIdentifier, declarationRange = dRange}
        setScopes $ Scope {variables = Map.insert identifier variableInfo variables} : restScopes
        return newBoundIdentifier

getVariableInfo :: UnboundIdentifier -> VariableBinder (Maybe VariableInfo)
getVariableInfo identifier = do
  state <- getState
  return $ getInScopes $ scopes state
  where
    getInScopes [] = Nothing
    getInScopes (Scope {variables} : tailScopes) = case Map.lookup identifier variables of
      Just info -> Just info
      Nothing -> getInScopes tailScopes

setVariableIsBeingDeclared :: UnboundIdentifier -> VariableBinder ()
setVariableIsBeingDeclared identifier = do
  VariableBindingState {scopes, boundIdentifierCounter, variablesInDeclaration} <- getState
  let updatedVariablesInDeclaration = Set.insert identifier variablesInDeclaration
  setState VariableBindingState {scopes, boundIdentifierCounter, variablesInDeclaration = updatedVariablesInDeclaration}
  return ()

setVariableIsDoneBeingDeclared :: UnboundIdentifier -> VariableBinder ()
setVariableIsDoneBeingDeclared identifier = do
  VariableBindingState {scopes, boundIdentifierCounter, variablesInDeclaration} <- getState
  let updatedVariablesInDeclaration = Set.delete identifier variablesInDeclaration
  setState VariableBindingState {scopes, boundIdentifierCounter, variablesInDeclaration = updatedVariablesInDeclaration}
  return ()

assertVariableIsNotBeingDeclared :: Range -> UnboundIdentifier -> VariableBinder ()
assertVariableIsNotBeingDeclared usageRange identifier = do
  VariableBindingState {variablesInDeclaration} <- getState
  if Set.member identifier variablesInDeclaration
    then do
      info <- getVariableInfo identifier
      case info of
        Just VariableInfo {declarationRange} -> throwBindingError $ VariableReferencedInDeclarationError identifier declarationRange usageRange
        _ -> throwBindingError $ ShouldNotGetHereError "Variable is in declaration but its info could not be found"
    else return ()

getNewBoundIdentifier :: VariableBinder (BoundIdentifier)
getNewBoundIdentifier = do
  VariableBindingState {boundIdentifierCounter, scopes, variablesInDeclaration} <- getState
  setState $ VariableBindingState {boundIdentifierCounter = boundIdentifierCounter + 1, scopes, variablesInDeclaration}
  return boundIdentifierCounter

getState :: VariableBinder VariableBindingState
getState = VariableBinder $ \state -> (state, Success state)

setState :: VariableBindingState -> VariableBinder ()
setState newState = VariableBinder $ const (newState, Success ())

setScopes :: [Scope] -> VariableBinder ()
setScopes scopes = do
  VariableBindingState {boundIdentifierCounter, variablesInDeclaration} <- getState
  setState VariableBindingState {boundIdentifierCounter, scopes, variablesInDeclaration}

throwBindingError :: Error -> VariableBinder a
throwBindingError e = VariableBinder $ \state -> (state, singleError e)

assertHasValue :: Error -> VariableBinder (Maybe a) -> VariableBinder a
assertHasValue e binder = VariableBinder $ \state -> case runBinder binder state of
  (newState, Success Nothing) -> (newState, singleError e)
  (newState, Success (Just a)) -> (newState, Success a)
  (newState, Error es) -> (newState, Error es)

-- andFinally enables running a binder after another, even if the first binder errors
andFinally :: VariableBinder a -> VariableBinder () -> VariableBinder a
binder1 `andFinally` binder2 = VariableBinder $ \state1 ->
  let (state2, result) = runBinder binder1 state1
   in let (state3, _) = runBinder binder2 state2
       in (state3, result)

{- A variant of the traverse function where if multiple of the binders have errors, they are all included in the final
resulting Error output.
-}
traverse' :: (a -> VariableBinder b) -> Seq a -> VariableBinder (Seq b)
traverse' makeBinder xs = sequenceA' $ makeBinder <$> xs

sequenceA' :: Seq (VariableBinder a) -> VariableBinder (Seq a)
sequenceA' binders = VariableBinder $ \state -> second collectResults (runBinders state)
  where
    runBinders initialState = foldl' (\(state, results) binder -> let (newState, result) = runBinder binder state in (newState, results |> result)) (initialState, Empty) binders
    collectResults xs = foldrWithErrors (<|) Empty xs