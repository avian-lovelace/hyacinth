{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module IdentifierBinding.IdentifierBinding
  ( IdentifierBinder (IdentifierBinder, runBinder),
    IdentifierInfo (IdentifierInfo, boundIdentifier, declarationRange),
    VariableUsability (BeforeDeclaration, InDeclaration, Usable),
    CapturedIdentifierInfo (CapturedIdentifierInfo, outsideIdentifier, insideIdentifier),
    traverse',
    addVariable,
    initialBindingState,
    andFinally,
    withNewExpressionScope,
    withNewFunctionScope,
    getIdentifierBinding,
    setVariableUsability,
    addParameter,
    addBoundFunctionDefinition,
    getCapturedIdentifiers,
    getBoundFunctions,
  )
where

import Core.Errors
import Core.FilePositions
import Data.Bifunctor (Bifunctor (second))
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq (Empty), (<|), (|>))
import qualified Data.Sequence as Seq
import IdentifierBinding.SyntaxTree
import Parsing.SyntaxTree

newtype IdentifierBinder a = IdentifierBinder {runBinder :: IdentifierBindingState -> (IdentifierBindingState, WithErrors a)} deriving (Functor)

instance Applicative IdentifierBinder where
  pure a = IdentifierBinder $ \state -> (state, Success a)
  (<*>) binderF binderA = IdentifierBinder $ \state ->
    case runBinder binderF state of
      (newState, Success f) -> runBinder (fmap f binderA) newState
      (newState, Error e) -> (newState, Error e)

instance Monad IdentifierBinder where
  binderA >>= makeBinderB = IdentifierBinder $ \state ->
    case runBinder binderA state of
      (newState, Success a) -> runBinder (makeBinderB a) newState
      (newState, Error e) -> (newState, Error e)

data IdentifierBindingState = IdentifierBindingState
  { scopes :: [Scope],
    boundIdentifierCounter :: Int,
    boundFunctions :: Seq IBFunctionDefinition
  }

data Scope
  = ExpressionScope {variables :: Map UnboundIdentifier VariableInfo}
  | FunctionScope {parameters :: Map UnboundIdentifier IdentifierInfo, capturedIdentifiers :: Map UnboundIdentifier CapturedIdentifierInfo}

data VariableInfo = VariableInfo IdentifierInfo VariableUsability

data CapturedIdentifierInfo = CapturedIdentifierInfo {outsideIdentifier :: IdentifierInfo, insideIdentifier :: IdentifierInfo}

data IdentifierInfo = IdentifierInfo {boundIdentifier :: BoundIdentifier, declarationRange :: Range}

data VariableUsability = BeforeDeclaration | InDeclaration | Usable

initialBindingState :: IdentifierBindingState
initialBindingState =
  IdentifierBindingState
    { scopes = [],
      boundIdentifierCounter = 0,
      boundFunctions = Empty
    }

withNewExpressionScope :: IdentifierBinder a -> IdentifierBinder a
withNewExpressionScope binder =
  do
    pushScope ExpressionScope {variables = Map.empty}
    binder
    `andFinally` popScope

withNewFunctionScope :: IdentifierBinder a -> IdentifierBinder a
withNewFunctionScope binder =
  do
    pushScope FunctionScope {parameters = Map.empty, capturedIdentifiers = Map.empty}
    binder
    `andFinally` popScope

addVariable :: Range -> UnboundIdentifier -> IdentifierBinder IdentifierInfo
addVariable dRange identifier = do
  currentScope <- getCurrentScope
  variables <- liftWithError $ expectExpressionScope currentScope
  case Map.lookup identifier variables of
    Just (VariableInfo conflictingInfo _) ->
      throwBindingError $
        ConflictingVariableDeclarationsError identifier (declarationRange conflictingInfo) dRange
    Nothing -> do
      checkForInvalidShadow dRange identifier
      identifierInfo <- getNewBinding dRange
      let variableInfo = VariableInfo identifierInfo BeforeDeclaration
      setCurrentScope $ ExpressionScope {variables = Map.insert identifier variableInfo variables}
      return identifierInfo

setVariableUsability :: UnboundIdentifier -> VariableUsability -> IdentifierBinder IdentifierInfo
setVariableUsability identifier usability = do
  currentScope <- getCurrentScope
  variables <- liftWithError $ expectExpressionScope currentScope
  case Map.lookup identifier variables of
    Just (VariableInfo info _) -> do
      let updatedVariables = Map.insert identifier (VariableInfo info usability) variables
      setCurrentScope $ ExpressionScope {variables = updatedVariables}
      return info
    Nothing -> throwBindingError $ ShouldNotGetHereError "Set variable usability of identifier not in scope"

addParameter :: Range -> UnboundIdentifier -> IdentifierBinder IdentifierInfo
addParameter dRange identifier = do
  currentScope <- getCurrentScope
  (parameters, capturedIdentifiers) <- liftWithError $ expectFunctionScope currentScope
  case Map.lookup identifier parameters of
    Just conflictingInfo -> throwBindingError $ ConflictingParameterNamesError identifier (declarationRange conflictingInfo) dRange
    Nothing -> do
      checkForInvalidShadow dRange identifier
      variableInfo <- getNewBinding dRange
      setCurrentScope $ FunctionScope {parameters = Map.insert identifier variableInfo parameters, capturedIdentifiers}
      return variableInfo

checkForInvalidShadow :: Range -> UnboundIdentifier -> IdentifierBinder ()
checkForInvalidShadow shadowRange identifier = do
  scopes <- getScopes
  checkForInvalidShadowHelper shadowRange identifier scopes

checkForInvalidShadowHelper :: Range -> UnboundIdentifier -> [Scope] -> IdentifierBinder ()
checkForInvalidShadowHelper _ _ [] = return ()
checkForInvalidShadowHelper shadowRange identifier (FunctionScope {parameters, capturedIdentifiers} : restScopes) =
  if Map.member identifier parameters || Map.member identifier capturedIdentifiers
    -- Parameters are always usable if they are in scope, and if a variable is captured, we have already confirmed it is usable
    then return ()
    else checkForInvalidShadowHelper shadowRange identifier restScopes
checkForInvalidShadowHelper shadowRange identifier (ExpressionScope {variables} : restScopes) =
  case Map.lookup identifier variables of
    Just (VariableInfo _ Usable) -> return ()
    Just (VariableInfo IdentifierInfo {declarationRange} _) -> throwBindingError $ VariableShadowedInDeclarationError identifier declarationRange shadowRange
    Nothing -> checkForInvalidShadowHelper shadowRange identifier restScopes

{- Get the bound identifier corresponding to the input unbound identifier. If there are any function scopes above the
found binding, their variable capture maps are updated to include this identifier. If the identifier is not usable, an
error is thrown.
-}
getIdentifierBinding :: Range -> UnboundIdentifier -> IdentifierBinder IdentifierInfo
getIdentifierBinding usageRange identifier = do
  scopes <- getScopes
  (info, updatedScopes) <- getIdentifierBindingHelper usageRange identifier scopes
  setScopes updatedScopes
  return info

getIdentifierBindingHelper :: Range -> UnboundIdentifier -> [Scope] -> IdentifierBinder (IdentifierInfo, [Scope])
getIdentifierBindingHelper usageRange identifier scopes = case scopes of
  [] -> throwBindingError $ VariableUndefinedAtReferenceError identifier usageRange
  ExpressionScope {variables} : restScopes -> case Map.lookup identifier variables of
    Just (VariableInfo info usability) -> do
      assertVariableIsUsable $ VariableInfo info usability
      return (info, scopes)
    Nothing -> do
      (info, updatedRestScopes) <- getIdentifierBindingHelper usageRange identifier restScopes
      return (info, ExpressionScope {variables} : updatedRestScopes)
  FunctionScope {parameters, capturedIdentifiers} : restScopes -> case Map.lookup identifier parameters of
    Just info -> do
      return (info, scopes)
    Nothing -> case Map.lookup identifier capturedIdentifiers of
      Just (CapturedIdentifierInfo {insideIdentifier}) -> do
        return (insideIdentifier, scopes)
      Nothing -> do
        (outsideIdentifier, updatedRestScopes) <- getIdentifierBindingHelper usageRange identifier restScopes
        insideIdentifier <- getNewBinding (declarationRange outsideIdentifier)
        let updatedCapturedVariables = Map.insert identifier (CapturedIdentifierInfo {outsideIdentifier, insideIdentifier}) capturedIdentifiers
        return (insideIdentifier, FunctionScope {parameters, capturedIdentifiers = updatedCapturedVariables} : updatedRestScopes)
  where
    assertVariableIsUsable :: VariableInfo -> IdentifierBinder ()
    assertVariableIsUsable (VariableInfo _ Usable) = return ()
    assertVariableIsUsable (VariableInfo info BeforeDeclaration) =
      throwBindingError $ VariableDeclaredAfterReferenceError identifier usageRange (declarationRange info)
    assertVariableIsUsable (VariableInfo info InDeclaration) =
      throwBindingError $ VariableReferencedInDeclarationError identifier usageRange (declarationRange info)

getState :: IdentifierBinder IdentifierBindingState
getState = IdentifierBinder $ \state -> (state, Success state)

setState :: IdentifierBindingState -> IdentifierBinder ()
setState newState = IdentifierBinder $ const (newState, Success ())

getNewBinding :: Range -> IdentifierBinder IdentifierInfo
getNewBinding declarationRange = do
  IdentifierBindingState {boundIdentifierCounter, scopes, boundFunctions} <- getState
  setState $ IdentifierBindingState {boundIdentifierCounter = boundIdentifierCounter + 1, scopes, boundFunctions}
  return IdentifierInfo {boundIdentifier = boundIdentifierCounter, declarationRange}

getBoundFunctions :: IdentifierBinder (Seq IBFunctionDefinition)
getBoundFunctions = boundFunctions <$> getState

setBoundFunctions :: Seq IBFunctionDefinition -> IdentifierBinder ()
setBoundFunctions boundFunctions = do
  IdentifierBindingState {boundIdentifierCounter, scopes} <- getState
  setState $ IdentifierBindingState {boundFunctions, boundIdentifierCounter, scopes}

addBoundFunctionDefinition :: IBFunctionDefinition -> IdentifierBinder FunctionIndex
addBoundFunctionDefinition newFunction = do
  boundFunctions <- boundFunctions <$> getState
  let updatedBoundFunctions = boundFunctions |> newFunction
  setBoundFunctions updatedBoundFunctions
  -- The functions are 1-indexed here because function 0 will be the main function, which is not yet included
  return $ length updatedBoundFunctions

getScopes :: IdentifierBinder [Scope]
getScopes = do
  IdentifierBindingState {scopes} <- getState
  return scopes

setScopes :: [Scope] -> IdentifierBinder ()
setScopes scopes = do
  IdentifierBindingState {boundIdentifierCounter, boundFunctions} <- getState
  setState IdentifierBindingState {scopes, boundIdentifierCounter, boundFunctions}

popScope :: IdentifierBinder Scope
popScope = do
  scopes <- getScopes
  case scopes of
    [] -> throwBindingError $ ShouldNotGetHereError "Called popScope while no scopes were active"
    currentScope : restScopes -> do
      setScopes restScopes
      return currentScope

pushScope :: Scope -> IdentifierBinder ()
pushScope scope = do
  scopes <- getScopes
  setScopes $ scope : scopes

getCurrentScope :: IdentifierBinder Scope
getCurrentScope = do
  scopes <- getScopes
  case scopes of
    [] -> throwBindingError $ ShouldNotGetHereError "Called getCurrentScope while no scopes were active"
    currentScope : _ -> do
      return currentScope

setCurrentScope :: Scope -> IdentifierBinder ()
setCurrentScope scope = do
  scopes <- getScopes
  case scopes of
    [] -> throwBindingError $ ShouldNotGetHereError "Called getCurrentScope while no scopes were active"
    _ : restScopes -> setScopes $ scope : restScopes

expectExpressionScope :: Scope -> WithErrors (Map UnboundIdentifier VariableInfo)
expectExpressionScope (ExpressionScope {variables}) = Success variables
expectExpressionScope _ = singleError $ ShouldNotGetHereError "Expected expression scope"

expectFunctionScope :: Scope -> WithErrors (Map UnboundIdentifier IdentifierInfo, Map UnboundIdentifier CapturedIdentifierInfo)
expectFunctionScope (FunctionScope {parameters, capturedIdentifiers}) = Success (parameters, capturedIdentifiers)
expectFunctionScope _ = singleError $ ShouldNotGetHereError "Expected function scope"

getCapturedIdentifiers :: IdentifierBinder (Seq CapturedIdentifierInfo)
getCapturedIdentifiers = do
  currentScope <- getCurrentScope
  (_, capturedIdentifiersMap) <- liftWithError $ expectFunctionScope currentScope
  return $ Seq.fromList $ Map.elems capturedIdentifiersMap

liftWithError :: WithErrors a -> IdentifierBinder a
liftWithError a = IdentifierBinder (,a)

throwBindingError :: Error -> IdentifierBinder a
throwBindingError = liftWithError . singleError

-- andFinally enables running a binder after another, even if the first binder errors
andFinally :: IdentifierBinder a -> IdentifierBinder b -> IdentifierBinder a
binder1 `andFinally` binder2 = IdentifierBinder $ \state1 ->
  let (state2, result) = runBinder binder1 state1
   in let (state3, _) = runBinder binder2 state2
       in (state3, result)

{- A variant of the traverse function where if multiple of the binders have errors, they are all included in the final
resulting Error output.
-}
traverse' :: (a -> IdentifierBinder b) -> Seq a -> IdentifierBinder (Seq b)
traverse' makeBinder xs = sequenceA' $ makeBinder <$> xs

sequenceA' :: Seq (IdentifierBinder a) -> IdentifierBinder (Seq a)
sequenceA' binders = IdentifierBinder $ \state -> second collectResults (runBinders state)
  where
    runBinders initialState = foldl' (\(state, results) binder -> let (newState, result) = runBinder binder state in (newState, results |> result)) (initialState, Empty) binders
    collectResults xs = foldrWithErrors (<|) Empty xs