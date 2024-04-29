import ControlFlowSpec
import ExpressionSpec
import FunctionSpec
import StringSpec
import Test.Hspec
import VariableSpec

main :: IO ()
main = hspec $ do
  -- testLexer
  testExpressions
  testVariables
  testStrings
  testControlFlow
  testFunctions