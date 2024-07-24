module ControlFlowSpec
  ( testControlFlow,
  )
where

import Core.Errors
import EndToEnd
import Test.Hspec

testControlFlow :: Spec
testControlFlow = do
  describe "Scope expressions:" $ do
    it "Scope expressions evaluate to ()" $
      "print⟨Nil⟩[{}];" `runsSuccessfullyWithOutput` "nil"
    it "Scope expressions run their contents when evaluated" $
      "printLine⟨Nil⟩[{ printLine⟨String⟩[\"foo\"]; }];" `runsSuccessfullyWithOutput` "foo\nnil\n"
    it "Scope expressions can contain multiple statements" $
      "printLine⟨Nil⟩[{ printLine⟨String⟩[\"foo\"]; let bar = 5; printLine⟨Int⟩[bar]; }];" `runsSuccessfullyWithOutput` "foo\n5\nnil\n"
  describe "If expressions:" $ do
    it "If-then-else expressions evaluate to their then expression when the condition is true" $
      "printLine⟨String⟩[if true then \"foo\" else \"bar\"]; printLine⟨String⟩[\"baz\"];" `runsSuccessfullyWithOutput` "foo\nbaz\n"
    it "If-then-else expressions evaluate to their else expression when the condition is false" $
      "printLine⟨String⟩[if false then \"foo\" else \"bar\"]; printLine⟨String⟩[\"baz\"];" `runsSuccessfullyWithOutput` "bar\nbaz\n"
    it "If-then expression work with scope expressions" $ do
      "if true then { printLine⟨String⟩[\"foo\"]; }; printLine⟨String⟩[\"bar\"];" `runsSuccessfullyWithOutput` "foo\nbar\n"
      "if false then { printLine⟨String⟩[\"foo\"]; }; printLine⟨String⟩[\"bar\"];" `runsSuccessfullyWithOutput` "bar\n"
    it "If-then-else expression work with scope expressions" $ do
      "if true then { printLine⟨String⟩[\"foo\"]; } else { printLine⟨String⟩[\"bar\"]; }; printLine⟨String⟩[\"baz\"];" `runsSuccessfullyWithOutput` "foo\nbaz\n"
      "if false then { printLine⟨String⟩[\"foo\"]; } else { printLine⟨String⟩[\"bar\"]; }; printLine⟨String⟩[\"baz\"];" `runsSuccessfullyWithOutput` "bar\nbaz\n"
    it "If-then-else expressions work with complex condition expressions" $ do
      "if 2 + 2 == 4 then { printLine⟨String⟩[\"foo\"]; } else { printLine⟨String⟩[\"bar\"]; }; printLine⟨String⟩[\"baz\"];" `runsSuccessfullyWithOutput` "foo\nbaz\n"
      "if 1 + 1 == 3 || 2 + 2 == 5 then { printLine⟨String⟩[\"foo\"]; } else { printLine⟨String⟩[\"bar\"]; }; printLine⟨String⟩[\"baz\"];" `runsSuccessfullyWithOutput` "bar\nbaz\n"
    it "If-then-else expressions are parsed so that they bind as much as possible to the else expression" $ do
      "print⟨Int⟩[1 + if true then 2 else 3 + 4];" `runsSuccessfullyWithOutput` "3"
      "print⟨Int⟩[1 + if true then 2 else 3 * 4];" `runsSuccessfullyWithOutput` "3"
      "print⟨Int⟩[1 + if false then 2 else 3 * 4];" `runsSuccessfullyWithOutput` "13"
  describe "If expression errors:" $ do
    it "The condition of an if expression must have type boolean" $
      "print⟨String⟩[if 5 then \"foo\" else \"bar\"];" `failsToCompileWithError` typeExpectationError
    it "The branch of an if-then expression must have type Nil" $
      "print⟨String⟩[if true then \"foo\"];" `failsToCompileWithError` ifThenExpressionNilTypeError
    it "The branches of an if-then-else expression must have the same type" $
      "print⟨String⟩[if true then \"foo\" else 5];" `failsToCompileWithError` typeExpectationError
  describe "While loop statements:" $ do
    it "While loops work" $
      "let mut i = 3; while i > 0 loop { printLine⟨Int⟩[i]; mut i = i - 1; }; printLine⟨String⟩[\"blastoff!\"];" `runsSuccessfullyWithOutput` "3\n2\n1\nblastoff!\n"
  describe "While loop errors:" $ do
    it "The condition of an if expression must have type boolean" $
      "while \"foo\" loop { print⟨Int⟩[1]; };" `failsToCompileWithError` typeExpectationError

ifThenExpressionNilTypeError :: Error -> Bool
ifThenExpressionNilTypeError (IfThenExpressionNilTypeError {}) = True
ifThenExpressionNilTypeError _ = False

typeExpectationError :: Error -> Bool
typeExpectationError (TypeExpectationError {}) = True
typeExpectationError _ = False