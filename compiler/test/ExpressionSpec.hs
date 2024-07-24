{-# OPTIONS_GHC -Wno-type-defaults #-}

module ExpressionSpec
  ( testExpressions,
  )
where

import Data.Text (pack)
import EndToEnd
import Test.Hspec

evaluatesTo :: String -> String -> String -> Expectation
evaluatesTo expression valueType value = code `runsSuccessfullyWithOutput` output
  where
    code = pack $ "print⟨" ++ valueType ++ "⟩[" ++ expression ++ "];"
    output = value

testExpressions :: Spec
testExpressions = do
  describe "Expressions:" $ do
    it "can be nil" $
      evaluatesTo "nil" "Nil" "nil"
    it "negates" $
      evaluatesTo "-5" "Int" "-5"
    it "adds" $
      evaluatesTo "17 + 5" "Int" "22"
    it "subtracts" $
      evaluatesTo "17 - 5" "Int" "12"
    it "multiplies" $
      evaluatesTo "17 * 5" "Int" "85"
    it "divides" $
      evaluatesTo "17 / 5" "Int" "3"
    it "modulos" $
      evaluatesTo "17 % 5" "Int" "2"
    it "nots true" $
      evaluatesTo "!true" "Bool" "false"
    it "nots false" $
      evaluatesTo "!false" "Bool" "true"
    it "ands to true" $
      evaluatesTo "true && true" "Bool" "true"
    it "ands to false" $
      evaluatesTo "true && false" "Bool" "false"
    it "ors to true" $
      evaluatesTo "true || false" "Bool" "true"
    it "or to false" $
      evaluatesTo "false || false" "Bool" "false"
    it "checks equality to true" $
      evaluatesTo "1 + 1 == 2" "Bool" "true"
    it "checks equality to false" $
      evaluatesTo "1 + 1 == 3" "Bool" "false"
    it "checks inequality to true" $
      evaluatesTo "1 + 1 != 3" "Bool" "true"
    it "checks inequality to false" $
      evaluatesTo "1 + 1 != 2" "Bool" "false"
    it "checks equality of strings (true)" $
      evaluatesTo "\"foo\" == \"foo\"" "Bool" "true"
    it "checks equality of strings (false)" $
      evaluatesTo "\"foo\" == \"bar\"" "Bool" "false"
    it "checks greater to true" $
      evaluatesTo "2 * 2 > 3" "Bool" "true"
    it "checks greater to false" $
      evaluatesTo "2 * 2 > 4" "Bool" "false"
    it "checks less to true" $
      evaluatesTo "2 * 2 < 5" "Bool" "true"
    it "checks less to false" $
      evaluatesTo "2 * 2 < 4" "Bool" "false"
    it "checks greater or euqal to true" $
      evaluatesTo "2 * 2 >= 4" "Bool" "true"
    it "checks greater or equal to false" $
      evaluatesTo "2 * 2 >= 5" "Bool" "false"
    it "checks less or equal to true" $
      evaluatesTo "2 * 2 <= 4" "Bool" "true"
    it "checks less or equal to false" $
      evaluatesTo "2 * 2 <= 3" "Bool" "false"
    it "left-associates addition and subtraction" $
      evaluatesTo "17 - 5 + 8 - 4" "Int" "16"
    it "left-associates multiplication and division" $
      evaluatesTo "18 / 3 * 5 / 2" "Int" "15"
    it "applies multiplication and division before addition and subtraction" $
      evaluatesTo "1 + 2 * 7 - 2" "Int" "13"
    it "respects parentheses" $
      evaluatesTo "(1 + 2) * (7 - 2)" "Int" "15"
    it "applies negation before addition" $
      evaluatesTo "8 + -3" "Int" "5"
    it "applies negation before multiplication" $
      evaluatesTo "3 * -5" "Int" "-15"
    it "respects nested parentheses" $
      evaluatesTo "7 * (18 - (3 * 5))" "Int" "21"
    it "calculates with floats" $
      evaluatesTo "3.5 + 2.0" "Float" "5.5"
    it "can be a statement" $
      "2 + 2; print⟨Int⟩[5];" `runsSuccessfullyWithOutput` "5"