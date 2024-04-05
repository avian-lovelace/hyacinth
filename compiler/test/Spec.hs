import Test.Hspec

import ParserSpec
import LexerSpec
import SectionerSpec

main :: IO ()
main = hspec $ do
  testLexer
  testSectioner
  -- testParser