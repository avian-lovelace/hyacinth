module LexerSpec where

--   ( testLexer,
--   )
-- where

-- import Core.Errors
-- import Core.Utils
-- import Data.Sequence (fromList)
-- import Lexing.Lexer
-- import Lexing.Tokens
-- import Test.Hspec
-- import TokenTestHelpers

-- testLexer :: Spec
-- testLexer = do
--   describe "lexText" $ do
--     describe "Keywords" $ do
--       it "lexes type" $
--         let input = "type"
--             expected = fromList [TypeToken (startRange 4)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes let" $
--         let input = "let"
--             expected = fromList [LetToken (startRange 3)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes if" $
--         let input = "if"
--             expected = fromList [Token IfToken (startRange 2)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes else" $
--         let input = "else"
--             expected = fromList [Token ElseToken (startRange 4)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes fn" $
--         let input = "fn"
--             expected = fromList [Token FnToken (startRange 2)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes match" $
--         let input = "match"
--             expected = fromList [Token MatchToken (startRange 5)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes of" $
--         let input = "of"
--             expected = fromList [Token OfToken (startRange 2)]
--          in lexText input `shouldBe` (Success expected)

--     describe "Separators" $ do
--       it "lexes ;" $
--         let input = ";"
--             expected = fromList [Token SemicolonToken (startRange 1)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes =" $
--         let input = "="
--             expected = fromList [Token EqualsToken (startRange 1)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes |" $
--         let input = "|"
--             expected = fromList [Token PipeToken (startRange 1)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes ->" $
--         let input = "->"
--             expected = fromList [Token SingleRightArrowToken (startRange 2)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes =>" $
--         let input = "=>"
--             expected = fromList [Token DoubleRightArrowToken (startRange 2)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes ," $
--         let input = ","
--             expected = fromList [Token CommaToken (startRange 1)]
--          in lexText input `shouldBe` (Success expected)

--     describe "Grouping" $ do
--       it "lexes (" $
--         let input = "("
--             expected = fromList [Token LeftParenToken (startRange 1)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes )" $
--         let input = ")"
--             expected = fromList [Token RightParenToken (startRange 1)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes {" $
--         let input = "{"
--             expected = fromList [Token LeftBraceToken (startRange 1)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes }" $
--         let input = "}"
--             expected = fromList [Token RightBraceToken (startRange 1)]
--          in lexText input `shouldBe` (Success expected)

--     describe "Primitive types" $ do
--       it "lexes Int" $
--         let input = "Int"
--             expected = fromList [Token IntToken (startRange 3)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes Double" $
--         let input = "Double"
--             expected = fromList [Token DoubleToken (startRange 6)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes Char" $
--         let input = "Char"
--             expected = fromList [Token CharToken (startRange 4)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes String" $
--         let input = "String"
--             expected = fromList [Token StringToken (startRange 6)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes Bool" $
--         let input = "Bool"
--             expected = fromList [Token BoolToken (startRange 4)]
--          in lexText input `shouldBe` (Success expected)

--     describe "Operators" $ do
--       it "lexes +" $
--         let input = "+"
--             expected = fromList [Token PlusToken (startRange 1)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes -" $
--         let input = "-"
--             expected = fromList [Token MinusToken (startRange 1)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes *" $
--         let input = "*"
--             expected = fromList [Token StarToken (startRange 1)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes /" $
--         let input = "/"
--             expected = fromList [Token SlashToken (startRange 1)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes !" $
--         let input = "!"
--             expected = fromList [Token BangToken (startRange 1)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes &&" $
--         let input = "&&"
--             expected = fromList [Token AndToken (startRange 2)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes ||" $
--         let input = "||"
--             expected = fromList [Token OrToken (startRange 2)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes ++" $
--         let input = "++"
--             expected = fromList [Token PlusPlusToken (startRange 2)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes >" $
--         let input = ">"
--             expected = fromList [Token GreaterToken (startRange 1)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes <" $
--         let input = "<"
--             expected = fromList [Token LessToken (startRange 1)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes >=" $
--         let input = ">="
--             expected = fromList [Token GreaterEqualToken (startRange 2)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes <=" $
--         let input = "<="
--             expected = fromList [Token LessEqualToken (startRange 2)]
--          in lexText input `shouldBe` (Success expected)

--     describe "Int literals" $ do
--       it "lexes 0" $
--         let input = "0"
--             expected = fromList [Token (IntLiteralToken 0) (startRange 1)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes 0" $
--         let input = "0"
--             expected = fromList [Token (IntLiteralToken 0) (startRange 1)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes 1234567890" $
--         let input = "1234567890"
--             expected = fromList [Token (IntLiteralToken 1234567890) (startRange 10)]
--          in lexText input `shouldBe` (Success expected)

--     describe "Double literals" $ do
--       it "lexes 0." $
--         let input = "0."
--             expected = fromList [Token (DoubleLiteralToken 0) (startRange 2)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes 0.1" $
--         let input = "0.1"
--             expected = fromList [Token (DoubleLiteralToken 0.1) (startRange 3)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes 12.34" $
--         let input = "12.34"
--             expected = fromList [Token (DoubleLiteralToken 12.34) (startRange 5)]
--          in lexText input `shouldBe` (Success expected)

--     describe "Char literals" $ do
--       it "lexes 'a'" $
--         let input = "'a'"
--             expected = fromList [Token (CharLiteralToken 'a') (startRange 3)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes '.'" $
--         let input = "'.'"
--             expected = fromList [Token (CharLiteralToken '.') (startRange 3)]
--          in lexText input `shouldBe` (Success expected)

--       it "throws UnterminatedCharError on '" $
--         let input = "'"
--             expected = UnterminatedCharError initPosition
--          in lexText input `shouldBe` (Error expected)

--       it "throws UnterminatedCharError on 'a" $
--         let input = "'a"
--             expected = UnterminatedCharError initPosition
--          in lexText input `shouldBe` (Error expected)

--       it "throws InvalidCharLiteralError on ''" $
--         let input = "''"
--             expected = InvalidCharLiteralError initPosition
--          in lexText input `shouldBe` (Error expected)

--       it "throws InvalidCharLiteralError on 'ab'" $
--         let input = "'ab'"
--             expected = InvalidCharLiteralError initPosition
--          in lexText input `shouldBe` (Error expected)

--     describe "String literals" $ do
--       it "lexes \"\"" $
--         let input = "\"\""
--             expected = fromList [Token (StringLiteralToken "") (startRange 2)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes \"foo\"" $
--         let input = "\"foo\""
--             expected = fromList [Token (StringLiteralToken "foo") (startRange 5)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes \"{a:1, b:2}\"" $
--         let input = "\"{a:1, b:2}\""
--             expected = fromList [Token (StringLiteralToken "{a:1, b:2}") (startRange 12)]
--          in lexText input `shouldBe` (Success expected)

--       it "throws UnterminatedStringError on \"" $
--         let input = "\""
--             expected = UnterminatedStringError initPosition
--          in lexText input `shouldBe` (Error expected)

--       it "throws UnterminatedStringError on \"foo" $
--         let input = "\"foo"
--             expected = UnterminatedStringError initPosition
--          in lexText input `shouldBe` (Error expected)

--     describe "Bool literals" $ do
--       it "lexes true" $
--         let input = "true"
--             expected = fromList [Token (BoolLiteralToken True) (startRange 4)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes false" $
--         let input = "false"
--             expected = fromList [Token (BoolLiteralToken False) (startRange 5)]
--          in lexText input `shouldBe` (Success expected)

--     describe "Identifiers" $ do
--       it "lexes identifierWITHCAPS_1234567890" $
--         let input = "identifierWITHCAPS_1234567890"
--             expected = fromList [Token (IdentifierToken "identifierWITHCAPS_1234567890") (startRange 29)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes official" $
--         let input = "official"
--             expected = fromList [Token (IdentifierToken "official") (startRange 8)]
--          in lexText input `shouldBe` (Success expected)

--       it "lexes Integral" $
--         let input = "Integral"
--             expected = fromList [Token (IdentifierToken "Integral") (startRange 8)]
--          in lexText input `shouldBe` (Success expected)

-- -- -- Test multiple tokens
-- -- it "lexes +-" $ let
-- --   input = "+-"
-- --   plusPosition = Range Position {line = 0, col = 0} Position {line = 0, col = 1}
-- --   minusPosition = Range Position {line = 0, col = 1} Position {line = 0, col = 2}
-- --   expected = fromList [PlusToken plusPosition, MinusToken minusPosition]
-- --   in lexText input `shouldBe` (Success expected)

-- -- -- Test whitespace handling
-- -- it "lexes + -" $ let
-- --   input = "+ -"
-- --   plusPosition = Range Position {line = 0, col = 0} Position {line = 0, col = 1}
-- --   minusPosition = Range Position {line = 0, col = 2} Position {line = 0, col = 3}
-- --   expected = fromList [PlusToken plusPosition, MinusToken minusPosition]
-- --   in lexText input `shouldBe` (Success expected)

-- -- it "lexes +\\n-" $ let
-- --   input = "+\n-"
-- --   plusPosition = Range Position {line = 0, col = 0} Position {line = 0, col = 1}
-- --   minusPosition = Range Position {line = 1, col = 0} Position {line = 1, col = 1}
-- --   expected = fromList [PlusToken plusPosition, MinusToken minusPosition]
-- --   in lexText input `shouldBe` (Success expected)

-- -- it "lexes + \\n-" $ let
-- --   input = "+\n-"
-- --   plusPosition = Range Position {line = 0, col = 0} Position {line = 0, col = 1}
-- --   minusPosition = Range Position {line = 1, col = 0} Position {line = 1, col = 1}
-- --   expected = fromList [PlusToken plusPosition, MinusToken minusPosition]
-- --   in lexText input `shouldBe` (Success expected)

-- -- it "lexes +\\n -" $ let
-- --   input = "+\n -"
-- --   plusPosition = Range Position {line = 0, col = 0} Position {line = 0, col = 1}
-- --   minusPosition = Range Position {line = 1, col = 1} Position {line = 1, col = 2}
-- --   expected = fromList [PlusToken plusPosition, MinusToken minusPosition]
-- --   in lexText input `shouldBe` (Success expected)

-- -- it "lexes +\\n\\n-" $ let
-- --   input = "+\n\n-"
-- --   plusPosition = Range Position {line = 0, col = 0} Position {line = 0, col = 1}
-- --   minusPosition = Range Position {line = 2, col = 0} Position {line = 2, col = 1}
-- --   expected = fromList [PlusToken plusPosition, MinusToken minusPosition]
-- --   in lexText input `shouldBe` (Success expected)