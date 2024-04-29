module ControlFlowSpec
  ( testControlFlow,
  )
where

import EndToEnd
import Test.Hspec

testControlFlow :: Spec
testControlFlow = do
  describe "Scope expressions" $ do
    it "Scope expressions evaluate to ()" $
      "print {};" `runsSuccessfullyWithOutput` "()\n"
    it "Scope expressions run their contents when evaluated" $
      "print { print \"foo\"; };" `runsSuccessfullyWithOutput` "foo\n()\n"
    it "Scope expressions can contain multiple statements" $
      "print { print \"foo\"; let bar = 5; print bar; };" `runsSuccessfullyWithOutput` "foo\n5\n()\n"
  describe "If expressions" $ do
    it "If-then expressions evaluate to their then expression when the condition is true" $
      "print if true then \"foo\"; print \"bar\";" `runsSuccessfullyWithOutput` "foo\nbar\n"
    it "If-then expressions evaluate to () when the condition is false" $
      "print if false then \"foo\"; print \"bar\";" `runsSuccessfullyWithOutput` "()\nbar\n"
    it "If-then-else expressions evaluate to their then expression when the condition is true" $
      "print if true then \"foo\" else \"bar\"; print \"baz\";" `runsSuccessfullyWithOutput` "foo\nbaz\n"
    it "If-then-else expressions evaluate to their else expression when the condition is false" $
      "print if false then \"foo\" else \"bar\"; print \"baz\";" `runsSuccessfullyWithOutput` "bar\nbaz\n"
    it "If-then expression work with scope expressions" $ do
      "if true then { print \"foo\"; }; print \"bar\";" `runsSuccessfullyWithOutput` "foo\nbar\n"
      "if false then { print \"foo\"; }; print \"bar\";" `runsSuccessfullyWithOutput` "bar\n"
    it "If-then-else expression work with scope expressions" $ do
      "if true then { print \"foo\"; } else { print \"bar\"; }; print \"baz\";" `runsSuccessfullyWithOutput` "foo\nbaz\n"
      "if false then { print \"foo\"; } else { print \"bar\"; }; print \"baz\";" `runsSuccessfullyWithOutput` "bar\nbaz\n"
    it "If-then-else expressions work with complex condition expressions" $ do
      "if 2 + 2 == 4 then { print \"foo\"; } else { print \"bar\"; }; print \"baz\";" `runsSuccessfullyWithOutput` "foo\nbaz\n"
      "if 1 + 1 == 3 || 2 + 2 == 5 then { print \"foo\"; } else { print \"bar\"; }; print \"baz\";" `runsSuccessfullyWithOutput` "bar\nbaz\n"
    it "If-then-else expressions are parsed so that they bind as much as possible to the else expression" $ do
      "print 1 + if true then 2 else 3 + 4;" `runsSuccessfullyWithOutput` "3\n"
      "print 1 + if true then 2 else 3 * 4;" `runsSuccessfullyWithOutput` "3\n"
      "print 1 + if false then 2 else 3 * 4;" `runsSuccessfullyWithOutput` "13\n"
  describe "While loop statements" $ do
    it "While loops work" $
      "let i = 3; while i > 0 loop { print i; mut i = i - 1; }; print \"blastoff!\";" `runsSuccessfullyWithOutput` "3\n2\n1\nblastoff!\n"