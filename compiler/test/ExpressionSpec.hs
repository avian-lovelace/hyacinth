{-# OPTIONS_GHC -Wno-type-defaults #-}

module ExpressionSpec
  ( testExpressions,
  )
where

import Data.Text (pack)
import EndToEnd
import Test.Hspec

evaluatesTo :: String -> String -> Expectation
evaluatesTo expression value = code `runsSuccessfullyWithOutput` output
  where
    code = pack $ "print " ++ expression ++ ";"
    output = value ++ "\n"

testExpressions :: Spec
testExpressions = do
  describe "Expressions:" $ do
    it "can be nil" $
      "nil" `evaluatesTo` "nil"
    it "negates" $
      "-5" `evaluatesTo` "-5"
    it "adds" $
      "17 + 5" `evaluatesTo` "22"
    it "subtracts" $
      "17 - 5" `evaluatesTo` "12"
    it "multiplies" $
      "17 * 5" `evaluatesTo` "85"
    it "divides" $
      "17 / 5" `evaluatesTo` "3"
    it "modulos" $
      "17 % 5" `evaluatesTo` "2"
    it "nots true" $
      "!true" `evaluatesTo` "false"
    it "nots false" $
      "!false" `evaluatesTo` "true"
    it "ands to true" $
      "true && true" `evaluatesTo` "true"
    it "ands to false" $
      "true && false" `evaluatesTo` "false"
    it "ors to true" $
      "true || false" `evaluatesTo` "true"
    it "or to false" $
      "false || false" `evaluatesTo` "false"
    it "checks equality to true" $
      "1 + 1 == 2" `evaluatesTo` "true"
    it "checks equality to false" $
      "1 + 1 == 3" `evaluatesTo` "false"
    it "checks inequality to true" $
      "1 + 1 != 3" `evaluatesTo` "true"
    it "checks inequality to false" $
      "1 + 1 != 2" `evaluatesTo` "false"
    it "checks equality of strings (true)" $
      "\"foo\" == \"foo\"" `evaluatesTo` "true"
    it "checks equality of strings (false)" $
      "\"foo\" == \"bar\"" `evaluatesTo` "false"
    it "checks greater to true" $
      "2 * 2 > 3" `evaluatesTo` "true"
    it "checks greater to false" $
      "2 * 2 > 4" `evaluatesTo` "false"
    it "checks less to true" $
      "2 * 2 < 5" `evaluatesTo` "true"
    it "checks less to false" $
      "2 * 2 < 4" `evaluatesTo` "false"
    it "checks greater or euqal to true" $
      "2 * 2 >= 4" `evaluatesTo` "true"
    it "checks greater or equal to false" $
      "2 * 2 >= 5" `evaluatesTo` "false"
    it "checks less or equal to true" $
      "2 * 2 <= 4" `evaluatesTo` "true"
    it "checks less or equal to false" $
      "2 * 2 <= 3" `evaluatesTo` "false"
    it "left-associates addition and subtraction" $
      "17 - 5 + 8 - 4" `evaluatesTo` "16"
    it "left-associates multiplication and division" $
      "18 / 3 * 5 / 2" `evaluatesTo` "15"
    it "applies multiplication and division before addition and subtraction" $
      "1 + 2 * 7 - 2" `evaluatesTo` "13"
    it "respects parentheses" $
      "(1 + 2) * (7 - 2)" `evaluatesTo` "15"
    it "applies negation before addition" $
      "8 + -3" `evaluatesTo` "5"
    it "applies negation before multiplication" $
      "3 * -5" `evaluatesTo` "-15"
    it "respects nested parentheses" $
      "7 * (18 - (3 * 5))" `evaluatesTo` "21"
    it "calculates with floats" $
      "3.5 + 2.0" `evaluatesTo` "5.5"
    it "can be a statement" $
      "2 + 2; print 5;" `runsSuccessfullyWithOutput` "5\n"