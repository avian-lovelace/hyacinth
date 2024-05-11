module IdentifierBinding.IdentifierBinding
  ( IdentifierBinder,
    IdentifierBindingState (IdentifierBindingState, boundValueIdentifierCounter, boundFunctionIdentifierCounter),
    IdentifierInfo (VariableIdentifierInfo, ParameterIdentifierInfo, FunctionIdentifierInfo),
    VariableUsability (BeforeDeclaration, InDeclaration, Usable),
    addVariable,
    initialBindingState,
    andFinally,
    withNewExpressionScope,
    withNewFunctionScope,
    getIdentifierBinding,
    setVariableUsability,
    addParameter,
    getCapturedIdentifiers,
    addFunction,
    getFunctionNameBinding,
  )
where

import Core.ErrorState
import Core.Errors
import Core.FilePositions
import Core.SyntaxTree
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import IdentifierBinding.SyntaxTree
import Parsing.SyntaxTree

type IdentifierBinder a = ErrorState IdentifierBindingState a

data IdentifierBindingState = IdentifierBindingState
  { scopes :: [ScopeInfo],
    boundValueIdentifierCounter :: Int,
    boundFunctionIdentifierCounter :: Int
  }

data ScopeInfo
  = ExpressionScope {scopeIdentifiers :: Map UnboundIdentifier IdentifierInfo}
  | FunctionScope {parameters :: Map UnboundIdentifier IdentifierInfo, capturedIdentifiers :: Set IdentifierInfo}

data IdentifierInfo
  = VariableIdentifierInfo Range BoundValueIdentifier Mutability VariableUsability
  | ParameterIdentifierInfo Range BoundValueIdentifier
  | FunctionIdentifierInfo Range BoundFunctionIdentifier
  deriving (Eq, Ord)

getIdentifier :: IdentifierInfo -> IBIdentifier
getIdentifier (VariableIdentifierInfo _ boundValueIdentifier _ _) = Left boundValueIdentifier
getIdentifier (ParameterIdentifierInfo _ boundValueIdentifier) = Left boundValueIdentifier
getIdentifier (FunctionIdentifierInfo _ boundFunctionIdentifier) = Right boundFunctionIdentifier

instance WithRange IdentifierInfo where
  getRange (VariableIdentifierInfo range _ _ _) = range
  getRange (ParameterIdentifierInfo range _) = range
  getRange (FunctionIdentifierInfo range _) = range

data VariableUsability = BeforeDeclaration | InDeclaration | Usable deriving (Eq, Ord)

initialBindingState :: IdentifierBindingState
initialBindingState =
  IdentifierBindingState
    { scopes = [],
      boundValueIdentifierCounter = 0,
      boundFunctionIdentifierCounter = 1
    }

withNewExpressionScope :: IdentifierBinder a -> IdentifierBinder a
withNewExpressionScope binder =
  do
    pushScope ExpressionScope {scopeIdentifiers = Map.empty}
    binder
    `andFinally` popScope

withNewFunctionScope :: IdentifierBinder a -> IdentifierBinder a
withNewFunctionScope binder =
  do
    pushScope FunctionScope {parameters = Map.empty, capturedIdentifiers = Set.empty}
    binder
    `andFinally` popScope

addVariable :: Range -> Mutability -> UnboundIdentifier -> IdentifierBinder ()
addVariable declarationRange mutability unboundIdentifier = do
  currentScope <- getCurrentScope
  scopeIdentifiers <- liftWithErrors $ expectExpressionScope currentScope
  case Map.lookup unboundIdentifier scopeIdentifiers of
    Just conflictingInfo ->
      throwError $
        ConflictingIdentifierDefinitionsError unboundIdentifier (getRange conflictingInfo) declarationRange
    Nothing -> do
      checkForInvalidShadow declarationRange unboundIdentifier
      boundValueIdentifier <- getNewValueBinding unboundIdentifier
      let variableIdentifierInfo = VariableIdentifierInfo declarationRange boundValueIdentifier mutability BeforeDeclaration
      setCurrentScope $ ExpressionScope {scopeIdentifiers = Map.insert unboundIdentifier variableIdentifierInfo scopeIdentifiers}

setVariableUsability :: UnboundIdentifier -> VariableUsability -> IdentifierBinder BoundValueIdentifier
setVariableUsability unboundIdentifier usability = do
  currentScope <- getCurrentScope
  scopeIdentifiers <- liftWithErrors $ expectExpressionScope currentScope
  case Map.lookup unboundIdentifier scopeIdentifiers of
    Just (VariableIdentifierInfo range boundIdentifier mutability _) -> do
      let updatedVariables = Map.insert unboundIdentifier (VariableIdentifierInfo range boundIdentifier mutability usability) scopeIdentifiers
      setCurrentScope $ ExpressionScope {scopeIdentifiers = updatedVariables}
      return boundIdentifier
    Just _ -> throwError $ ShouldNotGetHereError "Set variable usability of non-variable identifier"
    Nothing -> throwError $ ShouldNotGetHereError "Set variable usability of identifier not in scope"

addFunction :: Range -> UnboundIdentifier -> IdentifierBinder ()
addFunction definitionRange unboundFunctionName = do
  currentScope <- getCurrentScope
  scopeIdentifiers <- liftWithErrors $ expectExpressionScope currentScope
  case Map.lookup unboundFunctionName scopeIdentifiers of
    Just conflictingInfo ->
      throwError $
        ConflictingIdentifierDefinitionsError unboundFunctionName (getRange conflictingInfo) definitionRange
    Nothing -> do
      checkForInvalidShadow definitionRange unboundFunctionName
      boundFunctionIdentifier <- getNewFunctionBinding unboundFunctionName
      let functionIdentifierInfo = FunctionIdentifierInfo definitionRange boundFunctionIdentifier
      setCurrentScope $ ExpressionScope {scopeIdentifiers = Map.insert unboundFunctionName functionIdentifierInfo scopeIdentifiers}

addParameter :: Range -> UnboundIdentifier -> IdentifierBinder BoundValueIdentifier
addParameter parameterRange unboundParameter = do
  currentScope <- getCurrentScope
  (parameters, capturedIdentifiers) <- liftWithErrors $ expectFunctionScope currentScope
  case Map.lookup unboundParameter parameters of
    Just conflictingInfo -> throwError $ ConflictingParameterNamesError unboundParameter (getRange conflictingInfo) parameterRange
    Nothing -> do
      checkForInvalidShadow parameterRange unboundParameter
      boundValueIdentifier <- getNewValueBinding unboundParameter
      let parameterInfo = ParameterIdentifierInfo parameterRange boundValueIdentifier
      setCurrentScope $ FunctionScope {parameters = Map.insert unboundParameter parameterInfo parameters, capturedIdentifiers}
      return boundValueIdentifier

checkForInvalidShadow :: Range -> UnboundIdentifier -> IdentifierBinder ()
checkForInvalidShadow shadowRange unboundIdentifier = do
  scopes <- scopes <$> getState
  checkForInvalidShadowInScopes shadowRange unboundIdentifier scopes

checkForInvalidShadowInScopes :: Range -> UnboundIdentifier -> [ScopeInfo] -> IdentifierBinder ()
checkForInvalidShadowInScopes _ _ [] = return ()
checkForInvalidShadowInScopes shadowRange unboundIdentifier (FunctionScope {parameters} : restScopes) =
  if Map.member unboundIdentifier parameters
    -- Parameters are always usable if they are in scope, and if a variable is captured, we have already confirmed it is usable
    then return ()
    else checkForInvalidShadowInScopes shadowRange unboundIdentifier restScopes
checkForInvalidShadowInScopes shadowRange unboundIdentifier (ExpressionScope {scopeIdentifiers} : restScopes) =
  case Map.lookup unboundIdentifier scopeIdentifiers of
    Just (VariableIdentifierInfo declarationRange _ _ InDeclaration) -> throwError $ VariableShadowedInDeclarationError unboundIdentifier declarationRange shadowRange
    Just _ -> return ()
    Nothing -> checkForInvalidShadowInScopes shadowRange unboundIdentifier restScopes

{- Get the bound identifier corresponding to the input unbound identifier. If there are any function scopes above the
found binding, their variable capture maps are updated to include this identifier. If the identifier is not usable, an
error is thrown.
-}
getIdentifierBinding :: Range -> UnboundIdentifier -> IdentifierBinder IdentifierInfo
getIdentifierBinding usageRange unboundIdentifier = do
  scopes <- scopes <$> getState
  (info, updatedScopes) <- getIdentifierBindingInScopes usageRange unboundIdentifier scopes
  setScopes updatedScopes
  return info

getIdentifierBindingInScopes :: Range -> UnboundIdentifier -> [ScopeInfo] -> IdentifierBinder (IdentifierInfo, [ScopeInfo])
getIdentifierBindingInScopes usageRange unboundIdentifier scopes = case scopes of
  [] -> throwError $ IdentifierUndefinedAtReferenceError unboundIdentifier usageRange
  ExpressionScope {scopeIdentifiers} : restScopes -> case Map.lookup unboundIdentifier scopeIdentifiers of
    Just info -> return (info, scopes)
    Nothing -> do
      (info, updatedRestScopes) <- getIdentifierBindingInScopes usageRange unboundIdentifier restScopes
      return (info, ExpressionScope {scopeIdentifiers} : updatedRestScopes)
  FunctionScope {parameters, capturedIdentifiers} : restScopes -> case Map.lookup unboundIdentifier parameters of
    Just info -> return (info, scopes)
    Nothing -> do
      (info, updatedRestScopes) <- getIdentifierBindingInScopes usageRange unboundIdentifier restScopes
      let updatedCapturedIdentifiers = Set.insert info capturedIdentifiers
      return (info, FunctionScope {parameters, capturedIdentifiers = updatedCapturedIdentifiers} : updatedRestScopes)

--   where
--     assertVariableIsUsable :: IdentifierInfo -> VariableUsability -> IdentifierBinder ()
--     assertVariableIsUsable _ Usable = return ()
--     assertVariableIsUsable info BeforeDeclaration =
--       throwError $ VariableDeclaredAfterReferenceError identifier usageRange (declarationRange info)
--     assertVariableIsUsable info InDeclaration =
--       throwError $ VariableReferencedInDeclarationError identifier usageRange (declarationRange info)

getNewValueBinding :: UnboundIdentifier -> IdentifierBinder BoundValueIdentifier
getNewValueBinding unboundIdentifier = do
  IdentifierBindingState {boundValueIdentifierCounter, scopes, boundFunctionIdentifierCounter} <- getState
  setState $ IdentifierBindingState {boundValueIdentifierCounter = boundValueIdentifierCounter + 1, scopes, boundFunctionIdentifierCounter}
  return $ BoundValueIdentifier boundValueIdentifierCounter unboundIdentifier

getNewFunctionBinding :: UnboundIdentifier -> IdentifierBinder BoundFunctionIdentifier
getNewFunctionBinding unboundIdentifier = do
  IdentifierBindingState {boundFunctionIdentifierCounter, boundValueIdentifierCounter, scopes} <- getState
  setState $ IdentifierBindingState {boundFunctionIdentifierCounter = boundFunctionIdentifierCounter + 1, boundValueIdentifierCounter, scopes}
  return $ BoundFunctionIdentifier boundFunctionIdentifierCounter unboundIdentifier

setScopes :: [ScopeInfo] -> IdentifierBinder ()
setScopes scopes = do
  IdentifierBindingState {boundValueIdentifierCounter, boundFunctionIdentifierCounter} <- getState
  setState IdentifierBindingState {scopes, boundValueIdentifierCounter, boundFunctionIdentifierCounter}

popScope :: IdentifierBinder ScopeInfo
popScope = do
  scopes <- scopes <$> getState
  case scopes of
    [] -> throwError $ ShouldNotGetHereError "Called popScope while no scopes were active"
    currentScope : restScopes -> do
      setScopes restScopes
      return currentScope

pushScope :: ScopeInfo -> IdentifierBinder ()
pushScope scope = do
  scopes <- scopes <$> getState
  setScopes $ scope : scopes

getCurrentScope :: IdentifierBinder ScopeInfo
getCurrentScope = do
  scopes <- scopes <$> getState
  case scopes of
    [] -> throwError $ ShouldNotGetHereError "Called getCurrentScope while no scopes were active"
    currentScope : _ -> do
      return currentScope

setCurrentScope :: ScopeInfo -> IdentifierBinder ()
setCurrentScope scope = do
  scopes <- scopes <$> getState
  case scopes of
    [] -> throwError $ ShouldNotGetHereError "Called getCurrentScope while no scopes were active"
    _ : restScopes -> setScopes $ scope : restScopes

expectExpressionScope :: ScopeInfo -> WithErrors (Map UnboundIdentifier IdentifierInfo)
expectExpressionScope (ExpressionScope {scopeIdentifiers}) = Success scopeIdentifiers
expectExpressionScope _ = singleError $ ShouldNotGetHereError "Expected expression scope"

expectFunctionScope :: ScopeInfo -> WithErrors (Map UnboundIdentifier IdentifierInfo, Set IdentifierInfo)
expectFunctionScope (FunctionScope {parameters, capturedIdentifiers}) = Success (parameters, capturedIdentifiers)
expectFunctionScope _ = singleError $ ShouldNotGetHereError "Expected function scope"

getCapturedIdentifiers :: IdentifierBinder (Set IBIdentifier)
getCapturedIdentifiers = do
  currentScope <- getCurrentScope
  (_, capturedIdentifiers) <- liftWithErrors $ expectFunctionScope currentScope
  return $ Set.map getIdentifier capturedIdentifiers

getFunctionNameBinding :: UnboundIdentifier -> IdentifierBinder IBFunctionIdentifier
getFunctionNameBinding unboundFunctionName = do
  currentScope <- getCurrentScope
  identifierMap <- liftWithErrors $ expectExpressionScope currentScope
  case Map.lookup unboundFunctionName identifierMap of
    Just (FunctionIdentifierInfo _ boundFunctionName) -> return boundFunctionName
    _ -> throwError $ ShouldNotGetHereError "Failed to find function name binding"