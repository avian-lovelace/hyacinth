module MethodSpec
  ( testMethods,
  )
where

import Core.Errors
import EndToEnd
import Test.Hspec

testMethods :: Spec
testMethods = do
  describe "Methods:" $ do
    it "Functions can be called via method syntax" $
      "func foo = [x: Int]: Nil -> print⟨Int⟩[x * 2]; 3>>foo[];" `runsSuccessfullyWithOutput` "6"
    it "Functions with multiple arguments can be called via method syntax" $
      "func foo = [x: String, y: Bool, z: Int]: Nil -> {printLine⟨String⟩[x]; printLine⟨Bool⟩[y]; printLine⟨Int⟩[z]}; \"test\">>foo[true, 5];" `runsSuccessfullyWithOutput` "test\ntrue\n5\n"
    it "Functions defined by variable can be called via method syntax" $
      "let mult = [x: Int, y: Int]: Int -> x * y; print⟨Int⟩[3>>mult[5]];" `runsSuccessfullyWithOutput` "15"
    it "Built-in functions defined by variable can be called via method syntax" $
      "let output = 7; output>>print⟨Int⟩[]" `runsSuccessfullyWithOutput` "7"
  describe "Function errors:" $ do
    it "Non-function values cannot be used as a function" $
      "let foo = 5; let bar = 2.3; foo>>bar[];" `failsToCompileWithError` methodCallExpressionNotAFunctionTypeError
    it "Functions cannot be called as a method with too few arguments" $
      "let mult = [x: Int, y: Int]: Int -> x * y; print⟨Int⟩[3>>mult[]];" `failsToCompileWithError` methodCallExpressionArityError
    it "Functions cannot be called as a method with too many arguments" $
      "let mult = [x: Int, y: Int]: Int -> x * y; print⟨Int⟩[3>>mult[5, 7]];" `failsToCompileWithError` methodCallExpressionArityError
    it "The self expression of a method call must have the type of the function's first parameter" $
      "let mult = [x: String, y: Int]: Nil -> {}; 'a'>>mult[5];" `failsToCompileWithError` typeExpectationError
    it "The arguments of a method call must match type with the parameter types" $
      "let mult = [x: Int, y: String]: Nil -> {}; 3>>mult['a'];" `failsToCompileWithError` typeExpectationError

methodCallExpressionNotAFunctionTypeError :: Error -> Bool
methodCallExpressionNotAFunctionTypeError (MethodCallExpressionNotAFunctionTypeError {}) = True
methodCallExpressionNotAFunctionTypeError _ = False

methodCallExpressionArityError :: Error -> Bool
methodCallExpressionArityError (MethodCallExpressionArityError {}) = True
methodCallExpressionArityError _ = False

typeExpectationError :: Error -> Bool
typeExpectationError (TypeExpectationError {}) = True
typeExpectationError _ = False