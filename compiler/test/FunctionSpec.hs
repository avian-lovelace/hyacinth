module FunctionSpec
  ( testFunctions,
  )
where

import Core.Errors
import EndToEnd
import Test.Hspec

testFunctions :: Spec
testFunctions = do
  describe "Functions:" $ do
    it "Functions can be created and called" $
      "let foo = []: Int -> 5; print⟨Int⟩[foo[]];" `runsSuccessfullyWithOutput` "5"
    it "Functions can have parameters" $
      "let foo = [x: Int]: Int -> x * 2; print⟨Int⟩[foo[10]];" `runsSuccessfullyWithOutput` "20"
    it "Functions can have multiple parameters" $
      "let foo = [x: Int, y: Int, z: Int]: Int -> x - y * z; print⟨Int⟩[foo[10, 3, 4]];" `runsSuccessfullyWithOutput` "-2"
    it "Functions can execute statements" $
      "let foo = [x: Int]: Nil -> { printLine⟨Int⟩[x]; }; foo[1]; foo[2]; foo[3];" `runsSuccessfullyWithOutput` "1\n2\n3\n"
    it "Functions can use return statements to return values" $
      "let foo = [x: Int]: Int -> { printLine⟨Int⟩[x]; return x + 1; }; let result = foo[1]; printLine⟨Int⟩[result];" `runsSuccessfullyWithOutput` "1\n2\n"
    it "Functions can use return statements to return early" $
      "let foo = [x: Int]: Int -> { if x < 0 then { return x; }; printLine⟨Int⟩[2 * x]; return x + 1; }; printLine⟨Int⟩[foo[-5]]; printLine⟨Int⟩[foo[3]];" `runsSuccessfullyWithOutput` "-5\n6\n4\n"
    it "Return statements without an argument return nil" $
      "let foo = [x: Int]: Nil -> { if x < 0 then { return; }; printLine⟨Int⟩[x]; }; printLine⟨Nil⟩[foo[-5]]; printLine⟨Nil⟩[foo[3]];" `runsSuccessfullyWithOutput` "nil\n3\nnil\n"
    it "Functions can return functions" $
      "let foo = []: ([Int] -> Int) -> [x: Int]: Int -> x + 1; print⟨Int⟩[foo[][5]];" `runsSuccessfullyWithOutput` "6"
    it "Functions can capture parameters of outer functions" $
      "let foo = [x: Int]: ([Int] -> Int) -> [y: Int]: Int -> x * y; print⟨Int⟩[foo[3][5]];" `runsSuccessfullyWithOutput` "15"
    it "Functions can capture variable values" $
      "let cap = 3; let foo = []: Nil -> { print⟨Int⟩[cap]; }; foo[];" `runsSuccessfullyWithOutput` "3"
    it "The value of a captured variable is its value when it was captured, even if it changes later" $
      "let mut cap = 3; let foo = []: Nil -> { print⟨Int⟩[cap]; }; mut cap = 5; foo[];" `runsSuccessfullyWithOutput` "3"
    it "Captured variables can be used, even if the original variable no longer exists" $
      "let foo = []: ([] -> Int) -> { let cap = 3; return []: Int -> cap; }; print⟨Int⟩[foo[][]];" `runsSuccessfullyWithOutput` "3"
    it "When calling a function call expression, first the function expression is evaluated, then the arguments left-to-right, then the function call happens" $
      "let foo = []: ([Nil, Nil] -> Nil) -> { printLine⟨Int⟩[1]; return [x: Nil, y: Nil]: Nil -> { printLine⟨Int⟩[4]; }; }; foo[][{printLine⟨Int⟩[2];}, {printLine⟨Int⟩[3];}];" `runsSuccessfullyWithOutput` "1\n2\n3\n4\n"
    it "Functions bodies can shadow variables" $
      "let x = 1; let foo = []: Nil -> { let x = 2; printLine⟨Int⟩[x]; }; printLine⟨Int⟩[x]; foo[];" `runsSuccessfullyWithOutput` "1\n2\n"
    it "Functions bodies can shadow variables from before they are declared in a scope" $
      "let foo = []: Nil -> { let x = 2; printLine⟨Int⟩[x]; }; let x = 1; printLine⟨Int⟩[x]; foo[];" `runsSuccessfullyWithOutput` "1\n2\n"
  describe "Function statements:" $ do
    it "Functions can be created as statements and called" $
      "func foo = []: Int -> 5; print⟨Int⟩[foo[]];" `runsSuccessfullyWithOutput` "5"
    it "Functions defined by statement can be used in their scope before their definition" $
      "print⟨Int⟩[foo[]]; func foo = []: Int -> 5;" `runsSuccessfullyWithOutput` "5"
    it "Functions defined by statement can call other functions defined by statement" $
      "foo[4]; func foo = [x: Int]: Nil -> { print⟨Int⟩[plusOne[x]]; }; func  plusOne = [x: Int]: Int -> x + 1;" `runsSuccessfullyWithOutput` "5"
    it "A function defined by statement can call itself" $
      "print⟨Int⟩[fib[4]]; func fib = [x: Int]: Int -> if x <= 1 then x else fib[x - 1] + fib[x - 2];" `runsSuccessfullyWithOutput` "3"
    it "Functions defined by statement can call each other cyclically" $
      "foo[4]; func foo = [x: Int]: Nil -> { if x == 0 then { return; }; printLine⟨String⟩[\"foo\"]; bar[x - 1]; }; func bar = [x: Int]: Nil -> { if x == 0 then { return; }; printLine⟨String⟩[\"bar\"]; foo[x - 1]; };" `runsSuccessfullyWithOutput` "foo\nbar\nfoo\nbar\n"
    it "Functions defined by statement can capture variables" $
      "let cap = 5; print⟨Int⟩[foo[]]; func foo = []: Int -> cap;" `runsSuccessfullyWithOutput` "5"
    it "Functions defined by statement can capture variables defined later in scope, as long as they are defined by the first function call" $
      "func foo = []: Int -> cap; let cap = 5; print⟨Int⟩[foo[]];" `runsSuccessfullyWithOutput` "5"
    it "Variables captured by a function statement have their captured values set when the function is referenced" $
      "let mut cap = 1; printCap[]; mut cap = 2; printCap[]; func printCap = []: Nil -> { printLine⟨Int⟩[cap]; };" `runsSuccessfullyWithOutput` "1\n2\n"
    it "Variables can be transitively captured in function statements by inner function expressions" $
      "let cap = 1; foo[][]; func foo = []: ([] -> Nil) -> []: Nil -> { print⟨Int⟩[cap]; };" `runsSuccessfullyWithOutput` "1"
    it "Variables can be transitively captured in function statements by calling other function statements" $
      "let cap = 1; foo[]; func foo = []: Nil -> bar[]; func bar = []: Nil -> { print⟨Int⟩[cap]; };" `runsSuccessfullyWithOutput` "1"
    it "Functions defined by statement that call each other cyclically can capture variables transitively" $
      "let fooCap = \"foo\"; let barCap = \"bar\"; foo[4]; func foo = [x: Int]: Nil -> { if x == 0 then { return; }; printLine⟨String⟩[fooCap]; bar[x - 1]; }; func bar = [x: Int]: Nil -> { if x == 0 then { return; }; printLine⟨String⟩[barCap]; foo[x - 1]; };" `runsSuccessfullyWithOutput` "foo\nbar\nfoo\nbar\n"
  describe "Function errors:" $ do
    it "Functions cannot have multiple parameters with the same name" $
      "let foo = [x, y, x] -> x + y + x;" `failsToCompileWithError` conflictingParameterNamesError
    it "Functions parameters cannot shadow a variable in its declaration" $
      "let foo = [foo] -> 5;" `failsToCompileWithError` variableShadowedInDeclarationError
    it "Functions parameters cannot be mutated" $
      "let foo = [x] -> {mut x = x + 1;};" `failsToCompileWithError` mutatedNonVariableIdentifierError
    it "Variables captured by a function cannot be mutated" $
      "let mut cap = 10; let foo = []: Nil -> {mut cap = 0;};" `failsToCompileWithError` mutatedCapturedIdentifierError
    it "Variables captured by a function cannot be mutated even after they are referenced" $
      "let mut cap = 10; let foo = []: Nil -> {print⟨Int⟩[cap]; mut cap = 0;};" `failsToCompileWithError` mutatedCapturedIdentifierError
    it "Functions must have type annotations on all parameters" $
      "let foo = [x]: Int -> x + 1;" `failsToCompileWithError` functionMissingParameterTypeAnnotation
    it "Functions must have return type annotations" $
      "let foo = [x: Int] -> x + 1;" `failsToCompileWithError` functionMissingReturnTypeAnnotation
    it "Return statement values in a function must match the function return type" $
      "let foo = []: Char -> { return 5; };" `failsToCompileWithError` typeExpectationError
    it "If a function body never returns, its type must match the return type" $
      "let foo = []: Nil -> 5;" `failsToCompileWithError` typeExpectationError
    it "If a function body only sometimes returns, its type must match the return type" $
      "let foo = [x: Bool]: Int -> { if x then { return 0; }; };" `failsToCompileWithError` functionBodyTypeError
    it "Return statements not in a function must return Nil" $
      "let foo = 5; return foo;" `failsToCompileWithError` typeExpectationError
    it "Non-function values cannot be called" $
      "let foo = true; return foo[];" `failsToCompileWithError` functionCallExpressionNotAFunctionTypeError
    it "Function arguments must match their corresponding parameter type" $
      "let foo = [x: Int, y: Int]: Int -> x + y; foo[3, 'a'];" `failsToCompileWithError` typeExpectationError
    it "The number of arguments in a function call cannot be less than the number of parameters" $
      "let foo = [x: Int, y: Int]: Int -> x + y; foo[1];" `failsToCompileWithError` functionCallExpressionArityError
    it "The number of arguments in a function call cannot be more than the number of parameters" $
      "let foo = [x: Int, y: Int]: Int -> x + y; foo[1, 2, 3];" `failsToCompileWithError` functionCallExpressionArityError
  describe "Function type argument inference:" $ do
    it "Function type arguments can sometimes be inferred if the overall function type is known" $
      "func id = ⟨T⟩ => [value: T]: T -> value; let foo: [Int] -> Int = id;" `runsSuccessfullyWithOutput` ""
    it "Multiple function type arguments can sometimes be inferred if the overall function type is known" $
      "func foo = ⟨T, U, V⟩ => [t: T, u: U, v: V]: Nil -> {}; let bar: [Int, Float, String] -> Nil = foo;" `runsSuccessfullyWithOutput` ""
    it "Function type arguments can sometimes be inferred from the return type if the function is called directly" $
      "func id = ⟨T⟩ => [value: T]: T -> value; let foo: Int = id[5];" `runsSuccessfullyWithOutput` ""
    it "Function type arguments can sometimes be inferred from the first parameter type if the function is called directly as a method" $
      "func id = ⟨T⟩ => [value: T]: T -> value; let foo = 5>>id[];" `runsSuccessfullyWithOutput` ""
    it "Function type arguments can sometimes be inferred from the first parameter type and return type if the function is called directly as a method" $
      "func foo = ⟨T, U⟩ => [t: T, u: U]: U -> u; let bar: Char = 5>>foo['a'];" `runsSuccessfullyWithOutput` ""
    it "Type argument inference can deconstruct function types" $
      "func foo = ⟨T, U, V⟩ => [value: [T, U] -> V]: Nil -> {}; let bar: [[Int, Float] -> Nil] -> Nil = foo;" `runsSuccessfullyWithOutput` ""
    it "Type argument inference can deconstruct record types" $
      "rec A = ⟨T⟩ => []; rec B = ⟨T⟩ => []; func foo = ⟨T, U⟩ => [value: A⟨T⟩ | B⟨U⟩]: Nil -> {}; let bar: [A⟨Float⟩ | B⟨Bool⟩] -> Nil = foo;" `runsSuccessfullyWithOutput` ""
    it "Type argument inference can deconstruct type synonyms" $
      "type A = ⟨T, U⟩ => [T] -> U; func foo = ⟨T, U⟩ => [value: A⟨T, U⟩]: Nil -> {}; let bar: [[Int] -> String] -> Nil = foo;" `runsSuccessfullyWithOutput` ""
  describe "Built-in function type argument inference:" $ do
    it "The type arguments of print can be inferred from the overall type" $
      "let foo: [Int] -> Nil = print; foo[5];" `runsSuccessfullyWithOutput` "5"
    it "The type arguments of print can be inferred from the first parameter type" $
      "5>>print[]" `runsSuccessfullyWithOutput` "5"
    it "The type arguments of printLine can be inferred from the overall type" $
      "let foo: [Bool] -> Nil = print; foo[true];" `runsSuccessfullyWithOutput` "true"
    it "The type arguments of printLine can be inferred from the first parameter type" $
      "'a'>>print[]" `runsSuccessfullyWithOutput` "a"
    it "The type arguments of push can be inferred from the overall type" $
      "let foo: [mut List⟨String⟩, String] -> List⟨String⟩ = push;" `runsSuccessfullyWithOutput` ""
    it "The type arguments of push can be inferred from the return parameter type" $
      "let foo: List⟨Int⟩ = push[List[], 5];" `runsSuccessfullyWithOutput` ""
    it "The type arguments of push can be inferred from the first parameter type" $
      "let foo = mut List⟨Int⟩[1, 2, 3]; foo>>push[4];" `runsSuccessfullyWithOutput` ""
    it "The type arguments of pop can be inferred from the overall type" $
      "let foo: [mut List⟨String⟩] -> String = pop;" `runsSuccessfullyWithOutput` ""
    it "The type arguments of pop can be inferred from the return parameter type" $
      "let foo: Int = pop[List[1, 2, 3]];" `runsSuccessfullyWithOutput` ""
    it "The type arguments of pop can be inferred from the first parameter type" $
      "let foo = mut List⟨Int⟩[1, 2, 3]; foo>>pop[];" `runsSuccessfullyWithOutput` ""
    it "The type arguments of length can be inferred from the overall type" $
      "let foo: [List⟨String⟩] -> Int = length;" `runsSuccessfullyWithOutput` ""
    it "The type arguments of length can be inferred from the first parameter type" $
      "let foo = List⟨Int⟩[1, 2, 3]; foo>>length[];" `runsSuccessfullyWithOutput` ""
  describe "Type inference errors:" $ do
    it "Type arguments cannot be inferred if the type shapes don't match" $
      "func foo = ⟨T, U⟩ => [value: [T] -> U]: Nil -> {}; let bar: [String] -> Nil = foo;" `failsToCompileWithError` couldNotInferFunctionTypeArguments
    it "Type arguments cannot be inferred if the number of function parameters doesn't match" $
      "func foo = ⟨T, U⟩ => [value: T, u: U]: Nil -> {}; let bar: [String] -> Nil = foo;" `failsToCompileWithError` couldNotInferFunctionTypeArguments
    it "Type arguments cannot be inferred if a type parameter has conflicting values" $
      "func foo = ⟨T⟩ => [value: T]: T -> value; let bar: [String] -> Float = foo;" `failsToCompileWithError` couldNotInferFunctionTypeArguments
    it "Type arguments cannot be inferred from the return type if not all type parameters are used in the return type" $
      "func foo = ⟨T, U⟩ => [t: T, u: U]: U -> u; let bar: Int = foo['a', 5];" `failsToCompileWithError` couldNotInferFunctionTypeArguments
    it "Type arguments cannot be inferred from the first parameter if not all type parameters are used in the first parameter type" $
      "func foo = ⟨T, U⟩ => [t: T, u: U]: U -> u; let bar = 'a'>>foo[5];" `failsToCompileWithError` couldNotInferFunctionTypeArguments
    it "Type arguments can be inferred even when a simple type is substituted" $
      "func foo = ⟨T⟩ => [value: [T] -> Int]: Nil -> {}; let bar: [[Bool] -> Char] -> Nil = foo;" `failsToCompileWithError` typeExpectationError
    it "Type arguments can be inferred even when a list mutability doesn't match" $
      "func foo = ⟨T⟩ => [value: mut List⟨T⟩]: Nil -> {}; let bar: [List⟨Int⟩] -> Nil = foo;" `failsToCompileWithError` typeExpectationError
    it "Type arguments inference can fail on built-in functions" $
      "let bar: [Int, String] -> Nil = printLine;" `failsToCompileWithError` couldNotInferFunctionTypeArguments
    it "Type arguments inference can succeed on built-in functions even when a simple type is substituted" $
      "let bar: [List⟨Int⟩] -> Bool = length;" `failsToCompileWithError` typeExpectationError

conflictingParameterNamesError :: Error -> Bool
conflictingParameterNamesError (ConflictingParameterNamesError {}) = True
conflictingParameterNamesError _ = False

variableShadowedInDeclarationError :: Error -> Bool
variableShadowedInDeclarationError (VariableShadowedInDeclarationError {}) = True
variableShadowedInDeclarationError _ = False

mutatedNonVariableIdentifierError :: Error -> Bool
mutatedNonVariableIdentifierError (MutatedNonVariableIdentifierError {}) = True
mutatedNonVariableIdentifierError _ = False

mutatedCapturedIdentifierError :: Error -> Bool
mutatedCapturedIdentifierError (MutatedCapturedIdentifierError {}) = True
mutatedCapturedIdentifierError _ = False

functionMissingParameterTypeAnnotation :: Error -> Bool
functionMissingParameterTypeAnnotation (FunctionMissingParameterTypeAnnotation {}) = True
functionMissingParameterTypeAnnotation _ = False

functionMissingReturnTypeAnnotation :: Error -> Bool
functionMissingReturnTypeAnnotation (FunctionMissingReturnTypeAnnotation {}) = True
functionMissingReturnTypeAnnotation _ = False

functionBodyTypeError :: Error -> Bool
functionBodyTypeError (FunctionBodyTypeError {}) = True
functionBodyTypeError _ = False

typeExpectationError :: Error -> Bool
typeExpectationError (TypeExpectationError {}) = True
typeExpectationError _ = False

functionCallExpressionNotAFunctionTypeError :: Error -> Bool
functionCallExpressionNotAFunctionTypeError (FunctionCallExpressionNotAFunctionTypeError {}) = True
functionCallExpressionNotAFunctionTypeError _ = False

functionCallExpressionArityError :: Error -> Bool
functionCallExpressionArityError (FunctionCallExpressionArityError {}) = True
functionCallExpressionArityError _ = False

couldNotInferFunctionTypeArguments :: Error -> Bool
couldNotInferFunctionTypeArguments (CouldNotInferFunctionTypeArguments {}) = True
couldNotInferFunctionTypeArguments _ = False