import ExpressionSpec
import StringSpec
import Test.Hspec
import VariableSpec

main :: IO ()
main = hspec $ do
  -- testLexer
  testExpressions
  testVariables
  testStrings