module ExpressionSpec
  ( testExpressions,
  )
where

import Core.Errors
import Data.Text (pack)
import Lib
import System.Exit
import Test.Hspec

runsSuccessfullyWithOutput :: IO (WithError VMResult) -> String -> Expectation
runsSuccessfullyWithOutput vmRun expected = do
  runResult <- vmRun
  case runResult of
    Error e -> expectationFailure $ "Compilation failed with error: " ++ show e
    Success (ExitFailure _, _, stdErr) -> expectationFailure $ "Rumtime failed with error: " ++ stdErr
    Success (ExitSuccess, stdOut, _) -> stdOut `shouldBe` expected

evaluatesTo :: (Show a) => String -> a -> Expectation
evaluatesTo expression value = runCode code `runsSuccessfullyWithOutput` output
  where
    code = pack $ "print " ++ expression ++ ";"
    output = (show value) ++ "\n"

testExpressions :: Spec
testExpressions = do
  describe "An expression" $ do
    it "negates" $
      "-5" `evaluatesTo` (-5)
    it "adds" $
      "17 + 5" `evaluatesTo` 22
    it "subtracts" $
      "17 - 5" `evaluatesTo` 12
    it "multiplies" $
      "17 * 5" `evaluatesTo` 85
    it "divides" $
      "17 / 5" `evaluatesTo` 3
    -- it "modulos" $
    --   "17 % 5" `evaluatesTo` 2
    it "left-associates addition and subtraction" $
      "17 - 5 + 8 - 4" `evaluatesTo` 16
    it "left-associates multiplication and division" $
      "18 / 3 * 5 / 2" `evaluatesTo` 15
    it "applies multiplication and division before addition and subtraction" $
      "1 + 2 * 7 - 2" `evaluatesTo` 13
    it "respects parentheses" $
      "(1 + 2) * (7 - 2)" `evaluatesTo` 15
    it "applies negation before addition" $
      "8 + -3" `evaluatesTo` 5
    it "applies negation before multiplication" $
      "3 * -5" `evaluatesTo` (-15)
    it "respects nested parentheses" $
      "7 * (18 - (3 * 5))" `evaluatesTo` 21