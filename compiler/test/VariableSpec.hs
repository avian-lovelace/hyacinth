module VariableSpec
  ( testVariables,
  )
where

import Core.Errors
import EndToEnd
import Test.Hspec

testVariables :: Spec
testVariables = do
  describe "Variables:" $ do
    it "Variables can be defined and used" $
      "let foo = 4; print⟨Int⟩[foo];" `runsSuccessfullyWithOutput` "4"
    it "Variable declarations can have type annotations" $
      "let foo: Float = 1.2; print⟨Float⟩[foo];" `runsSuccessfullyWithOutput` "1.2"
    it "Multiple variables can be defined and retrieved from the same scope" $ do
      "let foo = 4; let bar = true; printLine⟨Int⟩[foo]; printLine⟨Bool⟩[bar];" `runsSuccessfullyWithOutput` "4\ntrue\n"
      "let foo = false; let bar = 3.5; printLine⟨Float⟩[bar]; printLine⟨Bool⟩[foo];" `runsSuccessfullyWithOutput` "3.5\nfalse\n"
    it "Variables can be mutated if defined as mutable" $
      "let mut foo = 4; printLine⟨Int⟩[foo]; mut foo = 23; printLine⟨Int⟩[foo];" `runsSuccessfullyWithOutput` "4\n23\n"
    it "Variables can be used inside their own mutation" $
      "let mut foo = 4; mut foo = foo * 3; print⟨Int⟩[foo];" `runsSuccessfullyWithOutput` "12"
    it "Variables can be accessed from within a nested scope" $
      "let foo = 5; { print⟨Int⟩[foo]; };" `runsSuccessfullyWithOutput` "5"
    it "Variables can be mutated from within a nested scope" $
      "let mut foo = 5; { mut foo = 3; }; print⟨Int⟩[foo];" `runsSuccessfullyWithOutput` "3"
    it "Variables can be shadowed from within a nested scope" $
      "let foo = 5; { let foo = 3; printLine⟨Int⟩[foo]; }; printLine⟨Int⟩[foo];" `runsSuccessfullyWithOutput` "3\n5\n"
    it "Variables can be shadowed from within a nested scope before they are declared" $
      "{ let foo = 3; printLine⟨Int⟩[foo]; }; let foo = 5; printLine⟨Int⟩[foo];" `runsSuccessfullyWithOutput` "3\n5\n"
    it "Variables can be defined and used from a nested scope at a point in an expression where the stack is not empty" $
      "nil == { let foo = 1; print⟨Int⟩[foo]; };" `runsSuccessfullyWithOutput` "1"
  describe "Variable errors:" $ do
    it "Multiple variables with the same name cannot be declared in the same scope" $
      "let foo = 3; let foo = 5;" `failsToCompileWithError` conflictingIdentifierDefinitionsError
    it "Variables cannot be mutated without being declared" $
      "mut foo = 5;" `failsToCompileWithError` identifierUndefinedAtReferenceError
    it "Variables cannot be mutated before being declared" $
      "mut foo = 5; let foo = 3;" `failsToCompileWithError` variableDefinedAfterReferenceError
    it "Variables cannot be used without being declared" $
      "print⟨Bool⟩[foo];" `failsToCompileWithError` identifierUndefinedAtReferenceError
    it "Variables cannot be used before being declared" $
      "print⟨Bool⟩[foo]; let foo = 3;" `failsToCompileWithError` variableDefinedAfterReferenceError
    it "Variables cannot be used in the value of their declaration" $
      "let foo = 1 + foo;" `failsToCompileWithError` variableReferencedInDeclarationError
    it "Variables cannot be used in nested scopes in the value of their declaration" $
      "let foo = 1 + { print⟨Int⟩[foo]; };" `failsToCompileWithError` variableReferencedInDeclarationError
    it "Variables cannot be shadowed in nested scopes in the value of their declaration" $
      "let foo = 1 + { let foo = 3; print⟨Int⟩[foo]; };" `failsToCompileWithError` variableShadowedInDeclarationError
    it "Variables declared as immutable cannot be mutated" $
      "let foo = 5; mut foo = 2;" `failsToCompileWithError` mutatedImmutableVariableError
    it "Variables declared with a type must have their value match that type" $
      "let foo: String = 5; print⟨String⟩[foo];" `failsToCompileWithError` typeExpectationError
    it "Variable mutation values must match the variable type when declared" $
      "let mut foo: Bool = true; mut foo = 7;" `failsToCompileWithError` typeExpectationError
    it "Variable mutation values must match the variable type when inferred" $
      "let mut foo = true; mut foo = 7;" `failsToCompileWithError` typeExpectationError

conflictingIdentifierDefinitionsError :: Error -> Bool
conflictingIdentifierDefinitionsError (ConflictingIdentifierDefinitionsError {}) = True
conflictingIdentifierDefinitionsError _ = False

identifierUndefinedAtReferenceError :: Error -> Bool
identifierUndefinedAtReferenceError (IdentifierUndefinedAtReferenceError {}) = True
identifierUndefinedAtReferenceError _ = False

variableDefinedAfterReferenceError :: Error -> Bool
variableDefinedAfterReferenceError (VariableDefinedAfterReferenceError {}) = True
variableDefinedAfterReferenceError _ = False

variableReferencedInDeclarationError :: Error -> Bool
variableReferencedInDeclarationError (VariableReferencedInDeclarationError {}) = True
variableReferencedInDeclarationError _ = False

variableShadowedInDeclarationError :: Error -> Bool
variableShadowedInDeclarationError (VariableShadowedInDeclarationError {}) = True
variableShadowedInDeclarationError _ = False

mutatedImmutableVariableError :: Error -> Bool
mutatedImmutableVariableError (MutatedImmutableVariableError {}) = True
mutatedImmutableVariableError _ = False

typeExpectationError :: Error -> Bool
typeExpectationError (TypeExpectationError {}) = True
typeExpectationError _ = False
