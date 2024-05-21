module IdentifierBinding.IdentifierBinding
  ( IdentifierBinder,
    IdentifierBindingState (IdentifierBindingState, boundValueIdentifierCounter, boundFunctionIdentifierCounter),
    IdentifierInfo (VariableIdentifierInfo, ParameterIdentifierInfo, FunctionIdentifierInfo, RecordIdentifierInfo, CaseParameterInfo),
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
    addRecord,
    getRecordNameBinding,
    withNewCaseScope,
  )
where

import Core.ErrorState
import Core.Errors
import Core.FilePositions
import Core.SyntaxTree
import Core.Utils
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
    boundFunctionIdentifierCounter :: Int,
    boundRecordIdentifierCounter :: Int
  }

data ScopeInfo
  = ExpressionScope {scopeIdentifiers :: Map UnboundIdentifier IdentifierInfo}
  | FunctionScope {parameters :: Map UnboundIdentifier IdentifierInfo, capturedIdentifiers :: Set IdentifierInfo}
  | CaseScope {caseParameter :: (UnboundIdentifier, IdentifierInfo)}

data IdentifierInfo
  = VariableIdentifierInfo Range BoundValueIdentifier Mutability VariableUsability
  | ParameterIdentifierInfo Range BoundValueIdentifier
  | FunctionIdentifierInfo Range BoundFunctionIdentifier
  | RecordIdentifierInfo Range BoundRecordIdentifier
  | CaseParameterInfo Range BoundValueIdentifier
  deriving (Eq, Ord)

asBoundIdentifier :: IdentifierInfo -> Maybe IBIdentifier
asBoundIdentifier (VariableIdentifierInfo _ boundValueIdentifier _ _) = Just . Left $ boundValueIdentifier
asBoundIdentifier (ParameterIdentifierInfo _ boundValueIdentifier) = Just . Left $ boundValueIdentifier
asBoundIdentifier (FunctionIdentifierInfo _ boundFunctionIdentifier) = Just . Right $ boundFunctionIdentifier
asBoundIdentifier (RecordIdentifierInfo _ _) = Nothing
asBoundIdentifier (CaseParameterInfo _ boundValueIdentifier) = Just . Left $ boundValueIdentifier

instance WithRange IdentifierInfo where
  getRange (VariableIdentifierInfo range _ _ _) = range
  getRange (ParameterIdentifierInfo range _) = range
  getRange (FunctionIdentifierInfo range _) = range
  getRange (RecordIdentifierInfo range _) = range
  getRange (CaseParameterInfo range _) = range

data VariableUsability = BeforeDeclaration | InDeclaration | Usable deriving (Eq, Ord)

initialBindingState :: IdentifierBindingState
initialBindingState =
  IdentifierBindingState
    { scopes = [],
      boundValueIdentifierCounter = 0,
      boundFunctionIdentifierCounter = 1,
      boundRecordIdentifierCounter = 0
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

withNewCaseScope :: Range -> UnboundIdentifier -> IdentifierBinder a -> IdentifierBinder (BoundValueIdentifier, a)
withNewCaseScope caseParameterRange unboundCaseParameter binder =
  do
    checkForInvalidShadow caseParameterRange unboundCaseParameter
    boundCaseSwitchValue <- getNewValueBinding unboundCaseParameter
    let caseParameterInfo = CaseParameterInfo caseParameterRange boundCaseSwitchValue
    pushScope CaseScope {caseParameter = (unboundCaseParameter, caseParameterInfo)}
    result <- binder
    return (boundCaseSwitchValue, result)
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

addRecord :: Range -> UnboundIdentifier -> IdentifierBinder ()
addRecord definitionRange unboundRecordName = do
  currentScope <- getCurrentScope
  scopeIdentifiers <- liftWithErrors $ expectExpressionScope currentScope
  case Map.lookup unboundRecordName scopeIdentifiers of
    Just conflictingInfo ->
      throwError $
        ConflictingIdentifierDefinitionsError unboundRecordName (getRange conflictingInfo) definitionRange
    Nothing -> do
      checkForInvalidShadow definitionRange unboundRecordName
      boundRecordIdentifier <- getNewRecordBinding unboundRecordName
      let recordIdentifierInfo = RecordIdentifierInfo definitionRange boundRecordIdentifier
      setCurrentScope $ ExpressionScope {scopeIdentifiers = Map.insert unboundRecordName recordIdentifierInfo scopeIdentifiers}

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
    -- Parameters are always usable if they are in scope
    then return ()
    else checkForInvalidShadowInScopes shadowRange unboundIdentifier restScopes
checkForInvalidShadowInScopes shadowRange unboundIdentifier (CaseScope {caseParameter} : restScopes) =
  if unboundIdentifier == fst caseParameter
    -- Case values are always usable if they are in scope
    then return ()
    else checkForInvalidShadowInScopes shadowRange unboundIdentifier restScopes
checkForInvalidShadowInScopes shadowRange unboundIdentifier (ExpressionScope {scopeIdentifiers} : restScopes) =
  case Map.lookup unboundIdentifier scopeIdentifiers of
    Just (VariableIdentifierInfo declarationRange _ _ InDeclaration) -> throwError $ VariableShadowedInDeclarationError unboundIdentifier declarationRange shadowRange
    Just (RecordIdentifierInfo definitionRange _) -> throwError $ IdentifierConflictsWithRecordError unboundIdentifier definitionRange shadowRange
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
  CaseScope {caseParameter} : restScopes ->
    if unboundIdentifier == fst caseParameter
      then
        return (snd caseParameter, scopes)
      else do
        (info, updatedRestScopes) <- getIdentifierBindingInScopes usageRange unboundIdentifier restScopes
        return (info, CaseScope {caseParameter} : updatedRestScopes)
  FunctionScope {parameters, capturedIdentifiers} : restScopes -> case Map.lookup unboundIdentifier parameters of
    Just info -> return (info, scopes)
    Nothing -> do
      (info, updatedRestScopes) <- getIdentifierBindingInScopes usageRange unboundIdentifier restScopes
      let updatedCapturedIdentifiers = Set.insert info capturedIdentifiers
      return (info, FunctionScope {parameters, capturedIdentifiers = updatedCapturedIdentifiers} : updatedRestScopes)

getNewValueBinding :: UnboundIdentifier -> IdentifierBinder BoundValueIdentifier
getNewValueBinding unboundIdentifier = do
  IdentifierBindingState {boundValueIdentifierCounter, scopes, boundFunctionIdentifierCounter, boundRecordIdentifierCounter} <- getState
  setState $ IdentifierBindingState {boundValueIdentifierCounter = boundValueIdentifierCounter + 1, scopes, boundFunctionIdentifierCounter, boundRecordIdentifierCounter}
  return $ BoundValueIdentifier boundValueIdentifierCounter unboundIdentifier

getNewFunctionBinding :: UnboundIdentifier -> IdentifierBinder BoundFunctionIdentifier
getNewFunctionBinding unboundIdentifier = do
  IdentifierBindingState {boundFunctionIdentifierCounter, boundValueIdentifierCounter, scopes, boundRecordIdentifierCounter} <- getState
  setState $ IdentifierBindingState {boundFunctionIdentifierCounter = boundFunctionIdentifierCounter + 1, boundValueIdentifierCounter, scopes, boundRecordIdentifierCounter}
  return $ BoundFunctionIdentifier boundFunctionIdentifierCounter unboundIdentifier

getNewRecordBinding :: UnboundIdentifier -> IdentifierBinder BoundRecordIdentifier
getNewRecordBinding unboundIdentifier = do
  IdentifierBindingState {boundFunctionIdentifierCounter, boundValueIdentifierCounter, scopes, boundRecordIdentifierCounter} <- getState
  setState $ IdentifierBindingState {boundRecordIdentifierCounter = boundRecordIdentifierCounter + 1, boundFunctionIdentifierCounter, boundValueIdentifierCounter, scopes}
  return $ BoundRecordIdentifier boundRecordIdentifierCounter unboundIdentifier

setScopes :: [ScopeInfo] -> IdentifierBinder ()
setScopes scopes = do
  IdentifierBindingState {boundValueIdentifierCounter, boundFunctionIdentifierCounter, boundRecordIdentifierCounter} <- getState
  setState IdentifierBindingState {scopes, boundValueIdentifierCounter, boundFunctionIdentifierCounter, boundRecordIdentifierCounter}

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
  return $ setFilterMap asBoundIdentifier capturedIdentifiers

getFunctionNameBinding :: UnboundIdentifier -> IdentifierBinder IBFunctionIdentifier
getFunctionNameBinding unboundFunctionName = do
  currentScope <- getCurrentScope
  identifierMap <- liftWithErrors $ expectExpressionScope currentScope
  case Map.lookup unboundFunctionName identifierMap of
    Just (FunctionIdentifierInfo _ boundFunctionName) -> return boundFunctionName
    _ -> throwError $ ShouldNotGetHereError "Failed to find function name binding"

getRecordNameBinding :: UnboundIdentifier -> IdentifierBinder IBRecordIdentifier
getRecordNameBinding unboundRecordName = do
  currentScope <- getCurrentScope
  identifierMap <- liftWithErrors $ expectExpressionScope currentScope
  case Map.lookup unboundRecordName identifierMap of
    Just (RecordIdentifierInfo _ boundRecordName) -> return boundRecordName
    _ -> throwError $ ShouldNotGetHereError "Failed to find function name binding"