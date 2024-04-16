module VariableSpec
  ( testVariables,
  )
where

import EndToEnd
import Test.Hspec

testVariables :: Spec
testVariables = do
  describe "Variables" $ do
    it "variables can be defined and used" $
      "let foo = 4; print foo;" `runsSuccessfullyWithOutput` "4\n"
    it "multiple variables can be defined and retrieved from the same scope" $ do
      "let foo = 4; let bar = true; print foo; print bar;" `runsSuccessfullyWithOutput` "4\ntrue\n"
      "let foo = false; let bar = 3.5; print bar; print foo;" `runsSuccessfullyWithOutput` "3.5\nfalse\n"
    it "variables can be mutated" $
      "let foo = 4; print foo; mut foo = 23; print foo;" `runsSuccessfullyWithOutput` "4\n23\n"