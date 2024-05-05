module VariableSpec
  ( testVariables,
  )
where

import Core.Errors
import EndToEnd
import Test.Hspec

testVariables :: Spec
testVariables = do
  describe "Variables" $ do
    it "Variables can be defined and used" $
      "let foo = 4; print foo;" `runsSuccessfullyWithOutput` "4\n"
    it "Variable declarations can have type annotations" $
      "let foo: Float = 1.2; print foo;" `runsSuccessfullyWithOutput` "1.2\n"
    it "Multiple variables can be defined and retrieved from the same scope" $ do
      "let foo = 4; let bar = true; print foo; print bar;" `runsSuccessfullyWithOutput` "4\ntrue\n"
      "let foo = false; let bar = 3.5; print bar; print foo;" `runsSuccessfullyWithOutput` "3.5\nfalse\n"
    it "Variables can be mutated if defined as mutable" $
      "let mut foo = 4; print foo; mut foo = 23; print foo;" `runsSuccessfullyWithOutput` "4\n23\n"
    it "Variables can be used inside their own mutation" $
      "let mut foo = 4; mut foo = foo * 3; print foo;" `runsSuccessfullyWithOutput` "12\n"
    it "Variables can be accessed from within a nested scope" $
      "let foo = 5; { print foo; };" `runsSuccessfullyWithOutput` "5\n"
    it "Variables can be mutated from within a nested scope" $
      "let mut foo = 5; { mut foo = 3; }; print foo;" `runsSuccessfullyWithOutput` "3\n"
    it "Variables can be shadowed from within a nested scope" $
      "let foo = 5; { let foo = 3; print foo; }; print foo;" `runsSuccessfullyWithOutput` "3\n5\n"
    it "Variables can be shadowed from within a nested scope before they are declared" $
      "{ let foo = 3; print foo; }; let foo = 5; print foo;" `runsSuccessfullyWithOutput` "3\n5\n"
  describe "Variable errors" $ do
    it "Multiple variables with the same name cannot be declared in the same scope" $
      "let foo = 3; let foo = 5;" `failsToCompileWithError` conflictingVariableDeclarationsError
    it "Variables cannot be mutated without being declared" $
      "mut foo = 5;" `failsToCompileWithError` variableUndefinedAtReferenceError
    it "Variables cannot be mutated before being declared" $
      "mut foo = 5; let foo = 3;" `failsToCompileWithError` variableDeclaredAfterReferenceError
    it "Variables cannot be used without being declared" $
      "print foo;" `failsToCompileWithError` variableUndefinedAtReferenceError
    it "Variables cannot be used before being declared" $
      "print foo; let foo = 3;" `failsToCompileWithError` variableDeclaredAfterReferenceError
    it "Variables cannot be used in the value of their declaration" $
      "let foo = 1 + foo;" `failsToCompileWithError` variableReferencedInDeclarationError
    it "Variables cannot be used in nested scopes in the value of their declaration" $
      "let foo = 1 + { print foo; };" `failsToCompileWithError` variableReferencedInDeclarationError
    it "Variables cannot be shadowed in nested scopes in the value of their declaration" $
      "let foo = 1 + { let foo = 3; print foo; };" `failsToCompileWithError` variableShadowedInDeclarationError
    it "Variables declared as immutable cannot be mutated" $
      "let foo = 5; mut foo = 2;" `failsToCompileWithError` mutatedImmutableVariableError
    it "Variables declared with a type must have their value match that type" $
      "let foo: String = 5; print foo;" `failsToCompileWithError` variableDeclarationTypeError
    it "Variable mutation values must match the variable type when declared" $
      "let mut foo: Bool = true; mut foo = 7;" `failsToCompileWithError` variableMutationTypeError
    it "Variable mutation values must match the variable type when inferred" $
      "let mut foo = true; mut foo = 7;" `failsToCompileWithError` variableMutationTypeError

conflictingVariableDeclarationsError :: Error -> Bool
conflictingVariableDeclarationsError (ConflictingVariableDeclarationsError _ _ _) = True
conflictingVariableDeclarationsError _ = False

variableUndefinedAtReferenceError :: Error -> Bool
variableUndefinedAtReferenceError (VariableUndefinedAtReferenceError _ _) = True
variableUndefinedAtReferenceError _ = False

variableDeclaredAfterReferenceError :: Error -> Bool
variableDeclaredAfterReferenceError (VariableDeclaredAfterReferenceError _ _ _) = True
variableDeclaredAfterReferenceError _ = False

variableReferencedInDeclarationError :: Error -> Bool
variableReferencedInDeclarationError (VariableReferencedInDeclarationError _ _ _) = True
variableReferencedInDeclarationError _ = False

variableShadowedInDeclarationError :: Error -> Bool
variableShadowedInDeclarationError (VariableShadowedInDeclarationError _ _ _) = True
variableShadowedInDeclarationError _ = False

mutatedImmutableVariableError :: Error -> Bool
mutatedImmutableVariableError (MutatedImmutableVariableError _ _ _) = True
mutatedImmutableVariableError _ = False

variableDeclarationTypeError :: Error -> Bool
variableDeclarationTypeError (VariableDeclarationTypeError _ _ _) = True
variableDeclarationTypeError _ = False

variableMutationTypeError :: Error -> Bool
variableMutationTypeError (VariableMutationTypeError _ _ _) = True
variableMutationTypeError _ = False