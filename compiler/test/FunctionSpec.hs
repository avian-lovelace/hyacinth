module FunctionSpec
  ( testFunctions,
  )
where

import Core.Errors
import EndToEnd
import Test.Hspec

testFunctions :: Spec
testFunctions = do
  describe "Functions" $ do
    it "Functions can be created and called" $
      "let foo = [] -> 5; print foo[];" `runsSuccessfullyWithOutput` "5\n"
    it "Functions can have parameters" $
      "let foo = [x] -> x * 2; print foo[10];" `runsSuccessfullyWithOutput` "20\n"
    it "Functions can have multiple parameters" $
      "let foo = [x, y, z] -> x - y * z; print foo[10, 3, 4];" `runsSuccessfullyWithOutput` "-2\n"
    it "Functions can execute statements" $
      "let foo = [x] -> { print x; }; foo[1]; foo[2]; foo[3];" `runsSuccessfullyWithOutput` "1\n2\n3\n"
    it "Functions can use return statements to return values" $
      "let foo = [x] -> { print x; return x + 1; }; let result = foo[1]; print result;" `runsSuccessfullyWithOutput` "1\n2\n"
    it "Functions can use return statements to return early" $
      "let foo = [x] -> { if x < 0 then { return x; }; print 2 * x; return x + 1; }; print foo[-5]; print foo[3];" `runsSuccessfullyWithOutput` "-5\n6\n4\n"
    it "Return statements without an argument return nil" $
      "let foo = [x] -> { if x < 0 then { return; }; print x; }; print foo[-5]; print foo[3];" `runsSuccessfullyWithOutput` "()\n3\n()\n"
    it "Functions can return functions" $
      "let foo = [] -> [x] -> x + 1; print foo[][5];" `runsSuccessfullyWithOutput` "6\n"
    it "Functions can capture parameters of outer functions" $
      "let foo = [x] -> [y] -> x * y; print foo[3][5];" `runsSuccessfullyWithOutput` "15\n"
    it "Functions can capture variable values" $
      "let cap = 3; let foo = [] -> { print cap; }; foo[];" `runsSuccessfullyWithOutput` "3\n"
    it "The value of a captured variable is its value when it was captured, even if it changes later" $
      "let mut cap = 3; let foo = [] -> { print cap; }; mut cap = 5; foo[];" `runsSuccessfullyWithOutput` "3\n"
    it "Captured variables can be used, even if the original variable no longer exists" $
      "let foo = [] -> { let cap = 3; return [] -> cap; }; print foo[][];" `runsSuccessfullyWithOutput` "3\n"
  describe "Function errors" $ do
    it "Functions cannot have multiple parameters with the same name" $
      "let foo = [x, y, x] -> x + y + x;" `failsToCompileWithError` conflictingParameterNamesError
    it "Functions parameters cannot shadow a variable in its declaration" $
      "let foo = [foo] -> 5;" `failsToCompileWithError` variableShadowedInDeclarationError
    it "Functions parameters cannot be mutated" $
      "let foo = [x] -> {mut x = x + 1;};" `failsToCompileWithError` mutatedParameterError
    it "Variables captured by a function cannot be mutated" $
      "let mut cap = 10; let foo = [] -> {mut cap = 0;};" `failsToCompileWithError` mutatedCapturedIdentifierError
    it "Variables captured by a function cannot be mutated even after they are referenced" $
      "let mut cap = 10; let foo = [] -> {print cap; mut cap = 0;};" `failsToCompileWithError` mutatedCapturedIdentifierError

conflictingParameterNamesError :: Error -> Bool
conflictingParameterNamesError (ConflictingParameterNamesError _ _ _) = True
conflictingParameterNamesError _ = False

variableShadowedInDeclarationError :: Error -> Bool
variableShadowedInDeclarationError (VariableShadowedInDeclarationError _ _ _) = True
variableShadowedInDeclarationError _ = False

mutatedParameterError :: Error -> Bool
mutatedParameterError (MutatedParameterError _ _ _) = True
mutatedParameterError _ = False

mutatedCapturedIdentifierError :: Error -> Bool
mutatedCapturedIdentifierError (MutatedCapturedIdentifierError _ _ _) = True
mutatedCapturedIdentifierError _ = False