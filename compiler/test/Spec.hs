import ExpressionSpec
-- import LexerSpec
import Test.Hspec
import VariableSpec

main :: IO ()
main = hspec $ do
  -- testLexer
  testExpressions
  testVariables