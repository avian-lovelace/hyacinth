import ControlFlowSpec
import ExpressionSpec
import FunctionSpec
import RecordSpec
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
  testRecords