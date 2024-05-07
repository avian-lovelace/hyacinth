{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module IdentifierBinding.IdentifierBinding
  ( IdentifierBinder,
    IdentifierInfo (IdentifierInfo, boundIdentifier, declarationRange),
    VariableUsability (BeforeDeclaration, InDeclaration, Usable),
    CapturedIdentifierInfo (CapturedIdentifierInfo, outsideIdentifier, insideIdentifier),
    addVariable,
    initialBindingState,
    andFinally,
    withNewExpressionScope,
    withNewFunctionScope,
    getIdentifierBinding,
    setVariableUsability,
    addParameter,
    addBoundSubFunction,
    getCapturedIdentifiers,
    getBoundFunctions,
  )
where

import Core.ErrorState
import Core.Errors
import Core.FilePositions
import Core.SyntaxTree
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq (Empty), (|>))
import qualified Data.Sequence as Seq
import IdentifierBinding.SyntaxTree
import Parsing.SyntaxTree

type IdentifierBinder a = ErrorState IdentifierBindingState a

data IdentifierBindingState = IdentifierBindingState
  { scopes :: [Scope],
    boundIdentifierCounter :: Int,
    boundFunctions :: Seq IBSubFunction
  }

data Scope
  = ExpressionScope {variables :: Map UnboundIdentifier VariableInfo}
  | FunctionScope {parameters :: Map UnboundIdentifier IdentifierInfo, capturedIdentifiers :: Map UnboundIdentifier CapturedIdentifierInfo}

data VariableInfo = VariableInfo IdentifierInfo VariableUsability Mutability

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

addVariable :: Range -> Mutability -> UnboundIdentifier -> IdentifierBinder IdentifierInfo
addVariable dRange mutability identifier = do
  currentScope <- getCurrentScope
  variables <- liftWithErrors $ expectExpressionScope currentScope
  case Map.lookup identifier variables of
    Just (VariableInfo conflictingInfo _ _) ->
      throwError $
        ConflictingVariableDeclarationsError identifier (declarationRange conflictingInfo) dRange
    Nothing -> do
      checkForInvalidShadow dRange identifier
      identifierInfo <- getNewBinding dRange
      let variableInfo = VariableInfo identifierInfo BeforeDeclaration mutability
      setCurrentScope $ ExpressionScope {variables = Map.insert identifier variableInfo variables}
      return identifierInfo

setVariableUsability :: UnboundIdentifier -> VariableUsability -> IdentifierBinder IdentifierInfo
setVariableUsability identifier usability = do
  currentScope <- getCurrentScope
  variables <- liftWithErrors $ expectExpressionScope currentScope
  case Map.lookup identifier variables of
    Just (VariableInfo info _ isMutable) -> do
      let updatedVariables = Map.insert identifier (VariableInfo info usability isMutable) variables
      setCurrentScope $ ExpressionScope {variables = updatedVariables}
      return info
    Nothing -> throwError $ ShouldNotGetHereError "Set variable usability of identifier not in scope"

addParameter :: Range -> UnboundIdentifier -> IdentifierBinder IdentifierInfo
addParameter dRange identifier = do
  currentScope <- getCurrentScope
  (parameters, capturedIdentifiers) <- liftWithErrors $ expectFunctionScope currentScope
  case Map.lookup identifier parameters of
    Just conflictingInfo -> throwError $ ConflictingParameterNamesError identifier (declarationRange conflictingInfo) dRange
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
    Just (VariableInfo IdentifierInfo {declarationRange} InDeclaration _) -> throwError $ VariableShadowedInDeclarationError identifier declarationRange shadowRange
    Just _ -> return ()
    Nothing -> checkForInvalidShadowHelper shadowRange identifier restScopes

{- Get the bound identifier corresponding to the input unbound identifier. If there are any function scopes above the
found binding, their variable capture maps are updated to include this identifier. If the identifier is not usable, an
error is thrown.
-}
getIdentifierBinding :: Range -> Bool -> UnboundIdentifier -> IdentifierBinder IdentifierInfo
getIdentifierBinding usageRange requireMutable identifier = do
  scopes <- getScopes
  (info, updatedScopes) <- getIdentifierBindingHelper usageRange requireMutable identifier scopes
  setScopes updatedScopes
  return info

getIdentifierBindingHelper :: Range -> Bool -> UnboundIdentifier -> [Scope] -> IdentifierBinder (IdentifierInfo, [Scope])
getIdentifierBindingHelper usageRange requireMutable identifier scopes = case scopes of
  [] -> throwError $ VariableUndefinedAtReferenceError identifier usageRange
  ExpressionScope {variables} : restScopes -> case Map.lookup identifier variables of
    Just (VariableInfo info usability mutability) -> do
      assertVariableIsUsable info usability
      if requireMutable && not (mutability == Mutable)
        then throwError $ MutatedImmutableVariableError identifier (declarationRange info) usageRange
        else return ()
      return (info, scopes)
    Nothing -> do
      (info, updatedRestScopes) <- getIdentifierBindingHelper usageRange requireMutable identifier restScopes
      return (info, ExpressionScope {variables} : updatedRestScopes)
  FunctionScope {parameters, capturedIdentifiers} : restScopes -> case Map.lookup identifier parameters of
    Just info -> do
      if requireMutable
        then throwError $ MutatedParameterError identifier (declarationRange info) usageRange
        else return (info, scopes)
    Nothing -> case Map.lookup identifier capturedIdentifiers of
      Just (CapturedIdentifierInfo {insideIdentifier}) -> do
        if requireMutable
          then throwError $ MutatedCapturedIdentifierError identifier (declarationRange insideIdentifier) usageRange
          else return (insideIdentifier, scopes)
      Nothing -> do
        (outsideIdentifier, updatedRestScopes) <- getIdentifierBindingHelper usageRange requireMutable identifier restScopes
        if requireMutable
          then throwError $ MutatedCapturedIdentifierError identifier (declarationRange outsideIdentifier) usageRange
          else return ()
        insideIdentifier <- getNewBinding (declarationRange outsideIdentifier)
        let updatedCapturedVariables = Map.insert identifier (CapturedIdentifierInfo {outsideIdentifier, insideIdentifier}) capturedIdentifiers
        return (insideIdentifier, FunctionScope {parameters, capturedIdentifiers = updatedCapturedVariables} : updatedRestScopes)
  where
    assertVariableIsUsable :: IdentifierInfo -> VariableUsability -> IdentifierBinder ()
    assertVariableIsUsable _ Usable = return ()
    assertVariableIsUsable info BeforeDeclaration =
      throwError $ VariableDeclaredAfterReferenceError identifier usageRange (declarationRange info)
    assertVariableIsUsable info InDeclaration =
      throwError $ VariableReferencedInDeclarationError identifier usageRange (declarationRange info)

getNewBinding :: Range -> IdentifierBinder IdentifierInfo
getNewBinding declarationRange = do
  IdentifierBindingState {boundIdentifierCounter, scopes, boundFunctions} <- getState
  setState $ IdentifierBindingState {boundIdentifierCounter = boundIdentifierCounter + 1, scopes, boundFunctions}
  return IdentifierInfo {boundIdentifier = boundIdentifierCounter, declarationRange}

getBoundFunctions :: IdentifierBinder (Seq IBSubFunction)
getBoundFunctions = boundFunctions <$> getState

setBoundFunctions :: Seq IBSubFunction -> IdentifierBinder ()
setBoundFunctions boundFunctions = do
  IdentifierBindingState {boundIdentifierCounter, scopes} <- getState
  setState $ IdentifierBindingState {boundFunctions, boundIdentifierCounter, scopes}

addBoundSubFunction :: IBSubFunction -> IdentifierBinder FunctionIndex
addBoundSubFunction newFunction = do
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
    [] -> throwError $ ShouldNotGetHereError "Called popScope while no scopes were active"
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
    [] -> throwError $ ShouldNotGetHereError "Called getCurrentScope while no scopes were active"
    currentScope : _ -> do
      return currentScope

setCurrentScope :: Scope -> IdentifierBinder ()
setCurrentScope scope = do
  scopes <- getScopes
  case scopes of
    [] -> throwError $ ShouldNotGetHereError "Called getCurrentScope while no scopes were active"
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
  (_, capturedIdentifiersMap) <- liftWithErrors $ expectFunctionScope currentScope
  return $ Seq.fromList $ Map.elems capturedIdentifiersMap
