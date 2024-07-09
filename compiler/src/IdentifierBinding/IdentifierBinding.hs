module IdentifierBinding.IdentifierBinding
  ( IdentifierBinder,
    IdentifierBindingState (IdentifierBindingState, boundValueIdentifierCounter, boundFunctionIdentifierCounter),
    IdentifierInfo
      ( VariableIdentifierInfo,
        ParameterIdentifierInfo,
        FunctionIdentifierInfo,
        RecordIdentifierInfo,
        CaseParameterInfo,
        TypeParameterInfo,
        MutabilityParameterInfo,
        TypeSynonymInfo
      ),
    VariableUsability (BeforeDeclaration, InDeclaration, Usable),
    addVariable,
    initialBindingState,
    andFinally,
    withNewExpressionScope,
    withNewFunctionScope,
    withNewRecordScope,
    getIdentifierBinding,
    setVariableUsability,
    addParameter,
    getCapturedIdentifiers,
    addFunction,
    getFunctionNameBinding,
    addRecord,
    getRecordNameBinding,
    withNewCaseScope,
    addRecordTypeParameter,
    addFunctionTypeParameter,
    addTypeSynonym,
    addTypeSynonymTypeParameter,
    getTypeSynonymBinding,
    withNewTypeSynonymScope,
    addTypeSynonymMutabilityParameter,
    getIdentifierInfoTextName,
  )
where

import Core.ErrorState
import Core.Errors
import Core.FilePositions
import Core.SyntaxTree
import Core.Utils
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import IdentifierBinding.SyntaxTree
import Parsing.SyntaxTree

type IdentifierBinder a = ErrorState IdentifierBindingState a

data IdentifierBindingState = IdentifierBindingState
  { scopes :: [ScopeInfo],
    boundValueIdentifierCounter :: Int,
    boundFunctionIdentifierCounter :: Int,
    boundRecordIdentifierCounter :: Int,
    boundTypeParameterCounter :: Int,
    boundMutabilityParameterCounter :: Int,
    boundTypeSynonymCounter :: Int
  }

data ScopeInfo
  = ExpressionScope {scopeIdentifiers :: Map UnboundIdentifier IdentifierInfo}
  | FunctionScope {functionTypeParameters :: Map UnboundIdentifier IdentifierInfo, parameters :: Map UnboundIdentifier IdentifierInfo, capturedIdentifiers :: Set IdentifierInfo}
  | CaseScope {caseParameter :: (UnboundIdentifier, IdentifierInfo)}
  | RecordScope {recordMutabilityParameter :: Maybe (UnboundIdentifier, IdentifierInfo), recordTypeParameters :: Map UnboundIdentifier IdentifierInfo}
  | TypeSynonymScope {typeSynonymMutabilityParameter :: Maybe (UnboundIdentifier, IdentifierInfo), typeSynonymTypeParameters :: Map UnboundIdentifier IdentifierInfo}

data IdentifierInfo
  = VariableIdentifierInfo Range BoundValueIdentifier Mutability VariableUsability
  | ParameterIdentifierInfo Range BoundValueIdentifier
  | FunctionIdentifierInfo Range BoundFunctionIdentifier
  | RecordIdentifierInfo Range BoundRecordIdentifier
  | CaseParameterInfo Range BoundValueIdentifier
  | TypeParameterInfo Range BoundTypeParameter
  | MutabilityParameterInfo Range BoundMutabilityParameter
  | TypeSynonymInfo Range BoundTypeSynonym
  deriving (Eq, Ord)

getIdentifierInfoTextName :: IdentifierInfo -> Text
getIdentifierInfoTextName (VariableIdentifierInfo {}) = "variable"
getIdentifierInfoTextName (ParameterIdentifierInfo {}) = "function parameter"
getIdentifierInfoTextName (FunctionIdentifierInfo {}) = "function"
getIdentifierInfoTextName (RecordIdentifierInfo {}) = "record"
getIdentifierInfoTextName (CaseParameterInfo {}) = "case parameter"
getIdentifierInfoTextName (TypeParameterInfo {}) = "type parameter"
getIdentifierInfoTextName (MutabilityParameterInfo {}) = "mutability parameter"
getIdentifierInfoTextName (TypeSynonymInfo {}) = "type synonym"

instance WithRange IdentifierInfo where
  getRange (VariableIdentifierInfo range _ _ _) = range
  getRange (ParameterIdentifierInfo range _) = range
  getRange (FunctionIdentifierInfo range _) = range
  getRange (RecordIdentifierInfo range _) = range
  getRange (CaseParameterInfo range _) = range
  getRange (TypeParameterInfo range _) = range
  getRange (MutabilityParameterInfo range _) = range
  getRange (TypeSynonymInfo range _) = range

data VariableUsability = BeforeDeclaration | InDeclaration | Usable deriving (Eq, Ord)

initialBindingState :: IdentifierBindingState
initialBindingState =
  IdentifierBindingState
    { scopes = [],
      boundValueIdentifierCounter = 0,
      boundFunctionIdentifierCounter = 1,
      boundRecordIdentifierCounter = 0,
      boundTypeParameterCounter = 0,
      boundMutabilityParameterCounter = 0,
      boundTypeSynonymCounter = 0
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
    pushScope FunctionScope {functionTypeParameters = Map.empty, parameters = Map.empty, capturedIdentifiers = Set.empty}
    binder
    `andFinally` popScope

withNewRecordScope :: Range -> Maybe UnboundIdentifier -> IdentifierBinder a -> IdentifierBinder (Maybe BoundMutabilityParameter, a)
withNewRecordScope mutabilityParameterRange maybeUnboundMutabilityParameter binder =
  do
    case maybeUnboundMutabilityParameter of
      Nothing -> do
        pushScope RecordScope {recordMutabilityParameter = Nothing, recordTypeParameters = Map.empty}
        result <- binder
        return (Nothing, result)
      Just unboundMutabilityParameter -> do
        checkForInvalidShadow mutabilityParameterRange unboundMutabilityParameter
        boundMutabilityParameter <- getNewMutabilityParameterBinding unboundMutabilityParameter
        let mutabilityParameterInfo = MutabilityParameterInfo mutabilityParameterRange boundMutabilityParameter
        pushScope RecordScope {recordMutabilityParameter = Just (unboundMutabilityParameter, mutabilityParameterInfo), recordTypeParameters = Map.empty}
        result <- binder
        return (Just boundMutabilityParameter, result)
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

withNewTypeSynonymScope :: IdentifierBinder a -> IdentifierBinder a
withNewTypeSynonymScope binder =
  do
    pushScope TypeSynonymScope {typeSynonymTypeParameters = Map.empty, typeSynonymMutabilityParameter = Nothing}
    binder
    `andFinally` popScope

addVariable :: Range -> Mutability -> UnboundIdentifier -> IdentifierBinder ()
addVariable declarationRange mutability unboundIdentifier = do
  currentScope <- getCurrentScope
  scopeIdentifiers <- liftWithErrors $ expectExpressionScope currentScope
  case Map.lookup unboundIdentifier scopeIdentifiers of
    Just conflictingInfo ->
      throwError $ ConflictingIdentifierDefinitionsError unboundIdentifier (getRange conflictingInfo) declarationRange
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
      throwError $ ConflictingIdentifierDefinitionsError unboundFunctionName (getRange conflictingInfo) definitionRange
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
      throwError $ ConflictingIdentifierDefinitionsError unboundRecordName (getRange conflictingInfo) definitionRange
    Nothing -> do
      checkForInvalidShadow definitionRange unboundRecordName
      boundRecordIdentifier <- getNewRecordBinding unboundRecordName
      let recordIdentifierInfo = RecordIdentifierInfo definitionRange boundRecordIdentifier
      setCurrentScope $ ExpressionScope {scopeIdentifiers = Map.insert unboundRecordName recordIdentifierInfo scopeIdentifiers}

addTypeSynonym :: Range -> UnboundIdentifier -> IdentifierBinder ()
addTypeSynonym definitionRange unboundTypeSynonym = do
  currentScope <- getCurrentScope
  scopeIdentifiers <- liftWithErrors $ expectExpressionScope currentScope
  case Map.lookup unboundTypeSynonym scopeIdentifiers of
    Just conflictingInfo ->
      throwError $ ConflictingIdentifierDefinitionsError unboundTypeSynonym (getRange conflictingInfo) definitionRange
    Nothing -> do
      checkForInvalidShadow definitionRange unboundTypeSynonym
      boundTypeSynonym <- getNewTypeSynonymBinding unboundTypeSynonym
      let typeSynonymInfo = TypeSynonymInfo definitionRange boundTypeSynonym
      setCurrentScope $ ExpressionScope {scopeIdentifiers = Map.insert unboundTypeSynonym typeSynonymInfo scopeIdentifiers}

addParameter :: Range -> UnboundIdentifier -> IdentifierBinder BoundValueIdentifier
addParameter parameterRange unboundParameter = do
  currentScope <- getCurrentScope
  (_typeParameters, parameters, _capturedIdentifiers) <- liftWithErrors $ expectFunctionScope currentScope
  case Map.lookup unboundParameter parameters of
    Just conflictingInfo -> throwError $ ConflictingParameterNamesError unboundParameter (getRange conflictingInfo) parameterRange
    Nothing -> do
      checkForInvalidShadow parameterRange unboundParameter
      boundValueIdentifier <- getNewValueBinding unboundParameter
      let parameterInfo = ParameterIdentifierInfo parameterRange boundValueIdentifier
      setCurrentScope $ currentScope {parameters = Map.insert unboundParameter parameterInfo parameters}
      return boundValueIdentifier

addFunctionTypeParameter :: Range -> UnboundIdentifier -> UnboundIdentifier -> IdentifierBinder BoundTypeParameter
addFunctionTypeParameter typeParameterRange functionName unboundTypeParameter = do
  currentScope <- getCurrentScope
  (typeParameters, _parameters, _capturedIdentifiers) <- liftWithErrors $ expectFunctionScope currentScope
  case Map.lookup unboundTypeParameter typeParameters of
    Just conflictingInfo -> throwError $ ConflictingTypeParametersError functionName unboundTypeParameter (getRange conflictingInfo) typeParameterRange
    Nothing -> do
      checkForInvalidShadow typeParameterRange unboundTypeParameter
      boundTypeParameter <- getNewTypeParameterBinding unboundTypeParameter
      let typeParameterInfo = TypeParameterInfo typeParameterRange boundTypeParameter
      setCurrentScope $ currentScope {functionTypeParameters = Map.insert unboundTypeParameter typeParameterInfo typeParameters}
      return boundTypeParameter

addRecordTypeParameter :: Range -> UnboundIdentifier -> UnboundIdentifier -> IdentifierBinder BoundTypeParameter
addRecordTypeParameter typeParameterRange recordName unboundTypeParameter = do
  currentScope <- getCurrentScope
  recordTypeParameters <- liftWithErrors $ expectRecordScope currentScope
  case Map.lookup unboundTypeParameter recordTypeParameters of
    Just conflictingInfo -> throwError $ ConflictingTypeParametersError recordName unboundTypeParameter (getRange conflictingInfo) typeParameterRange
    Nothing -> do
      checkForInvalidShadow typeParameterRange unboundTypeParameter
      boundTypeParameter <- getNewTypeParameterBinding unboundTypeParameter
      let typeParameterInfo = TypeParameterInfo typeParameterRange boundTypeParameter
      setCurrentScope $ currentScope {recordTypeParameters = Map.insert unboundTypeParameter typeParameterInfo recordTypeParameters}
      return boundTypeParameter

addTypeSynonymTypeParameter :: Range -> UnboundIdentifier -> UnboundIdentifier -> IdentifierBinder BoundTypeParameter
addTypeSynonymTypeParameter typeParameterRange typeSynonym unboundTypeParameter = do
  currentScope <- getCurrentScope
  typeSynonymTypeParameters <- liftWithErrors $ expectTypeSynonymScope currentScope
  case Map.lookup unboundTypeParameter typeSynonymTypeParameters of
    Just conflictingInfo -> throwError $ ConflictingTypeParametersError typeSynonym unboundTypeParameter (getRange conflictingInfo) typeParameterRange
    Nothing -> do
      checkForInvalidShadow typeParameterRange unboundTypeParameter
      boundTypeParameter <- getNewTypeParameterBinding unboundTypeParameter
      let typeParameterInfo = TypeParameterInfo typeParameterRange boundTypeParameter
      setCurrentScope $ currentScope {typeSynonymTypeParameters = Map.insert unboundTypeParameter typeParameterInfo typeSynonymTypeParameters}
      return boundTypeParameter

addTypeSynonymMutabilityParameter :: Range -> UnboundIdentifier -> IdentifierBinder BoundMutabilityParameter
addTypeSynonymMutabilityParameter mutabilityParameterRange unboundMutabilityParameter = do
  currentScope <- getCurrentScope
  _ <- liftWithErrors $ expectTypeSynonymScope currentScope
  checkForInvalidShadow mutabilityParameterRange unboundMutabilityParameter
  boundMutabilityParameter <- getNewMutabilityParameterBinding unboundMutabilityParameter
  let mutabilityParameterInfo = MutabilityParameterInfo mutabilityParameterRange boundMutabilityParameter
  setCurrentScope $ currentScope {typeSynonymMutabilityParameter = Just (unboundMutabilityParameter, mutabilityParameterInfo)}
  return boundMutabilityParameter

checkForInvalidShadow :: Range -> UnboundIdentifier -> IdentifierBinder ()
checkForInvalidShadow shadowRange unboundIdentifier = do
  scopes <- scopes <$> getState
  checkForInvalidShadowInScopes shadowRange unboundIdentifier scopes

checkForInvalidShadowInScopes :: Range -> UnboundIdentifier -> [ScopeInfo] -> IdentifierBinder ()
checkForInvalidShadowInScopes _ _ [] = return ()
checkForInvalidShadowInScopes shadowRange unboundIdentifier (FunctionScope {functionTypeParameters, parameters} : restScopes) =
  case Map.lookup unboundIdentifier functionTypeParameters of
    Just shadowedIdentifierInfo -> throwError (ShadowedTypeIdentifierError unboundIdentifier (getRange shadowedIdentifierInfo) shadowRange)
    Nothing ->
      if Map.member unboundIdentifier parameters
        -- Parameters are always usable if they are in scope
        then return ()
        else checkForInvalidShadowInScopes shadowRange unboundIdentifier restScopes
checkForInvalidShadowInScopes shadowRange unboundIdentifier (RecordScope {recordMutabilityParameter, recordTypeParameters} : restScopes) =
  case Map.lookup unboundIdentifier recordTypeParameters of
    Just shadowedIdentifierInfo -> throwError (ShadowedTypeIdentifierError unboundIdentifier (getRange shadowedIdentifierInfo) shadowRange)
    Nothing ->
      if Just unboundIdentifier == (fst <$> recordMutabilityParameter)
        then throwError (ShadowedTypeIdentifierError unboundIdentifier (getRange . snd . fromJust $ recordMutabilityParameter) shadowRange)
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
checkForInvalidShadowInScopes shadowRange unboundIdentifier (TypeSynonymScope {typeSynonymTypeParameters, typeSynonymMutabilityParameter} : restScopes) =
  case Map.lookup unboundIdentifier typeSynonymTypeParameters of
    Just shadowedIdentifierInfo -> throwError (ShadowedTypeIdentifierError unboundIdentifier (getRange shadowedIdentifierInfo) shadowRange)
    Nothing ->
      if Just unboundIdentifier == (fst <$> typeSynonymMutabilityParameter)
        then throwError (ShadowedTypeIdentifierError unboundIdentifier (getRange . snd . fromJust $ typeSynonymMutabilityParameter) shadowRange)
        else checkForInvalidShadowInScopes shadowRange unboundIdentifier restScopes

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
  FunctionScope {functionTypeParameters, parameters, capturedIdentifiers} : restScopes -> case Map.lookup unboundIdentifier parameters of
    Just info -> return (info, scopes)
    Nothing -> case Map.lookup unboundIdentifier functionTypeParameters of
      Just info -> return (info, scopes)
      Nothing -> do
        (info, updatedRestScopes) <- getIdentifierBindingInScopes usageRange unboundIdentifier restScopes
        let updatedCapturedIdentifiers = Set.insert info capturedIdentifiers
        return (info, FunctionScope {functionTypeParameters, parameters, capturedIdentifiers = updatedCapturedIdentifiers} : updatedRestScopes)
  RecordScope {recordMutabilityParameter, recordTypeParameters} : restScopes -> case Map.lookup unboundIdentifier recordTypeParameters of
    Just info -> return (info, scopes)
    Nothing -> case recordMutabilityParameter of
      Just mutabilityParameter ->
        if unboundIdentifier == fst mutabilityParameter
          then return (snd mutabilityParameter, scopes)
          else do
            (info, updatedRestScopes) <- getIdentifierBindingInScopes usageRange unboundIdentifier restScopes
            return (info, RecordScope {recordMutabilityParameter, recordTypeParameters} : updatedRestScopes)
      Nothing -> do
        (info, updatedRestScopes) <- getIdentifierBindingInScopes usageRange unboundIdentifier restScopes
        return (info, RecordScope {recordMutabilityParameter, recordTypeParameters} : updatedRestScopes)
  TypeSynonymScope {typeSynonymTypeParameters, typeSynonymMutabilityParameter} : restScopes -> case Map.lookup unboundIdentifier typeSynonymTypeParameters of
    Just info -> return (info, scopes)
    Nothing -> case typeSynonymMutabilityParameter of
      Just (unboundMutabilityParameter, mutabilityParameterInfo)
        | unboundIdentifier == unboundMutabilityParameter ->
            return (mutabilityParameterInfo, scopes)
      _ -> do
        (info, updatedRestScopes) <- getIdentifierBindingInScopes usageRange unboundIdentifier restScopes
        return (info, TypeSynonymScope {typeSynonymTypeParameters, typeSynonymMutabilityParameter} : updatedRestScopes)

getNewValueBinding :: UnboundIdentifier -> IdentifierBinder BoundValueIdentifier
getNewValueBinding unboundIdentifier = do
  state <- getState
  let currentBoundValueIdentifierCounter = boundValueIdentifierCounter state
  setState $ state {boundValueIdentifierCounter = currentBoundValueIdentifierCounter + 1}
  return $ BoundValueIdentifier currentBoundValueIdentifierCounter unboundIdentifier

getNewFunctionBinding :: UnboundIdentifier -> IdentifierBinder BoundFunctionIdentifier
getNewFunctionBinding unboundIdentifier = do
  state <- getState
  let currentBoundFunctionIdentifierCounter = boundFunctionIdentifierCounter state
  setState $ state {boundFunctionIdentifierCounter = currentBoundFunctionIdentifierCounter + 1}
  return $ BoundFunctionIdentifier currentBoundFunctionIdentifierCounter unboundIdentifier

getNewRecordBinding :: UnboundIdentifier -> IdentifierBinder BoundRecordIdentifier
getNewRecordBinding unboundIdentifier = do
  state <- getState
  let currentBoundRecordIdentifierCounter = boundRecordIdentifierCounter state
  setState $ state {boundRecordIdentifierCounter = currentBoundRecordIdentifierCounter + 1}
  return $ BoundRecordIdentifier currentBoundRecordIdentifierCounter unboundIdentifier

getNewTypeParameterBinding :: UnboundIdentifier -> IdentifierBinder BoundTypeParameter
getNewTypeParameterBinding unboundIdentifier = do
  state <- getState
  let currentBoundTypeParameterCounter = boundTypeParameterCounter state
  setState $ state {boundTypeParameterCounter = currentBoundTypeParameterCounter + 1}
  return $ BoundTypeParameter currentBoundTypeParameterCounter unboundIdentifier

getNewMutabilityParameterBinding :: UnboundIdentifier -> IdentifierBinder BoundMutabilityParameter
getNewMutabilityParameterBinding unboundIdentifier = do
  state <- getState
  let currentBoundMutabilityParameterCounter = boundMutabilityParameterCounter state
  setState $ state {boundMutabilityParameterCounter = currentBoundMutabilityParameterCounter + 1}
  return $ BoundMutabilityParameter currentBoundMutabilityParameterCounter unboundIdentifier

getNewTypeSynonymBinding :: UnboundIdentifier -> IdentifierBinder BoundTypeSynonym
getNewTypeSynonymBinding unboundIdentifier = do
  state <- getState
  let currentBoundTypeSynonymCounter = boundTypeSynonymCounter state
  setState $ state {boundTypeSynonymCounter = currentBoundTypeSynonymCounter + 1}
  return $ BoundTypeSynonym currentBoundTypeSynonymCounter unboundIdentifier

setScopes :: [ScopeInfo] -> IdentifierBinder ()
setScopes scopes = do
  state <- getState
  setState state {scopes}

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

expectFunctionScope :: ScopeInfo -> WithErrors (Map UnboundIdentifier IdentifierInfo, Map UnboundIdentifier IdentifierInfo, Set IdentifierInfo)
expectFunctionScope (FunctionScope {functionTypeParameters, parameters, capturedIdentifiers}) = Success (functionTypeParameters, parameters, capturedIdentifiers)
expectFunctionScope _ = singleError $ ShouldNotGetHereError "Expected function scope"

expectRecordScope :: ScopeInfo -> WithErrors (Map UnboundIdentifier IdentifierInfo)
expectRecordScope (RecordScope {recordTypeParameters}) = Success recordTypeParameters
expectRecordScope _ = singleError $ ShouldNotGetHereError "Expected function scope"

expectTypeSynonymScope :: ScopeInfo -> WithErrors (Map UnboundIdentifier IdentifierInfo)
expectTypeSynonymScope (TypeSynonymScope {typeSynonymTypeParameters}) = Success typeSynonymTypeParameters
expectTypeSynonymScope _ = singleError $ ShouldNotGetHereError "Expected function scope"

getCapturedIdentifiers :: IdentifierBinder (Set (Either IBValueIdentifier IBFunctionIdentifier))
getCapturedIdentifiers = do
  currentScope <- getCurrentScope
  (_, _, capturedIdentifiers) <- liftWithErrors $ expectFunctionScope currentScope
  return $ setFilterMap asCapturedIdentifier capturedIdentifiers
  where
    asCapturedIdentifier :: IdentifierInfo -> Maybe (Either IBValueIdentifier IBFunctionIdentifier)
    asCapturedIdentifier (VariableIdentifierInfo _ boundValueIdentifier _ _) = Just . Left $ boundValueIdentifier
    asCapturedIdentifier (ParameterIdentifierInfo _ boundValueIdentifier) = Just . Left $ boundValueIdentifier
    asCapturedIdentifier (FunctionIdentifierInfo _ boundFunctionIdentifier) = Just . Right $ boundFunctionIdentifier
    asCapturedIdentifier (RecordIdentifierInfo _ _) = Nothing
    asCapturedIdentifier (CaseParameterInfo _ boundValueIdentifier) = Just . Left $ boundValueIdentifier
    asCapturedIdentifier (TypeParameterInfo _ _) = Nothing
    asCapturedIdentifier (MutabilityParameterInfo _ _) = Nothing
    asCapturedIdentifier (TypeSynonymInfo _ _) = Nothing

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
    _ -> throwError $ ShouldNotGetHereError "Failed to find record name binding"

getTypeSynonymBinding :: UnboundIdentifier -> IdentifierBinder IBTypeSynonym
getTypeSynonymBinding unboundTypeSynonym = do
  currentScope <- getCurrentScope
  identifierMap <- liftWithErrors $ expectExpressionScope currentScope
  case Map.lookup unboundTypeSynonym identifierMap of
    Just (TypeSynonymInfo _ boundTypeSynonym) -> return boundTypeSynonym
    _ -> throwError $ ShouldNotGetHereError "Failed to find type synonym binding"