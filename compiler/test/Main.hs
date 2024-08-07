import CommentSpec
import ControlFlowSpec
import ExpressionSpec
import FunctionSpec
import ListSpec
import MethodSpec
import RecordSpec
import StringSpec
import Test.Hspec
import VariableSpec
import VarianceSpec

main :: IO ()
main = hspec $ do
  testExpressions
  testVariables
  testStrings
  testControlFlow
  testFunctions
  testRecords
  testComments
  testLists
  testVariance
  testMethods