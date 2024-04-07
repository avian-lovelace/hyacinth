import ExpressionSpec
import LexerSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  testLexer
  testExpressions