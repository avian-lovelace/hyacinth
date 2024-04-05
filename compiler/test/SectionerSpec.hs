module SectionerSpec (
  testSectioner
) where

import Test.Hspec

import Lexing.Tokens
import Lexing.Lexer
import TokenTestHelpers
import Core.Errors
import Sectioning.Sectioner
import Sectioning.Section

import Data.Sequence(fromList)

testSectioner :: Spec
testSectioner = do
  describe "parseSections" $ do
    it "sections empty string" $ let
      input = ""
      result = do
        tokens <- lexText input
        parseSections tokens
      expected = fromList []
      in result `shouldBe` Success expected
    
    it "sections let" $ let
      input = "let"
      result = do
        tokens <- lexText input
        parseSections tokens
      token = Token{ value = LetToken, range = simpleRange 1 4 }
      tokenSection = TokenSection token
      statementSection = NoSemicolonSection (fromList [tokenSection]) (simpleRange 1 4)
      expected = fromList [statementSection]
      in result `shouldBe` Success expected
    
    it "sections let" $ let
      input = "let foo = 0"
      result = do
        tokens <- lexText input
        parseSections tokens
      token1 = Token{ value = LetToken, range = simpleRange 1 4 }
      token2 = Token{ value = IdentifierToken "foo", range = simpleRange 5 8 }
      token3 = Token{ value = EqualsToken, range = simpleRange 9 10 }
      token4 = Token{ value = IntLiteralToken 0, range = simpleRange 11 12 }
      tokenSection1 = TokenSection token1
      tokenSection2 = TokenSection token2
      tokenSection3 = TokenSection token3
      tokenSection4 = TokenSection token4
      statementSection = NoSemicolonSection (fromList [ tokenSection1, tokenSection2, tokenSection3, tokenSection4]) (simpleRange 1 12)
      expected = fromList [statementSection]
      in result `shouldBe` Success expected
    
    it "sections ()" $ let
      input = "()"
      result = do
        tokens <- lexText input
        parseSections tokens
      expected = fromList [ NoSemicolonSection (fromList [ParenSection (fromList []) (simpleRange 1 3)]) (simpleRange 1 3)]
      in result `shouldBe` Success expected
    
    it "sections {}" $ let
      input = "{}"
      result = do
        tokens <- lexText input
        parseSections tokens
      expected = fromList [ NoSemicolonSection (fromList [BracketSection (fromList []) (simpleRange 1 3)]) (simpleRange 1 3)]
      in result `shouldBe` Success expected
    
    it "sections foo {}" $ let
      input = "foo {}"
      result = do
        tokens <- lexText input
        parseSections tokens
      fooToken = Token{ value = IdentifierToken "foo", range = simpleRange 1 4}
      fooSection = TokenSection fooToken
      bracketSection = BracketSection (fromList []) (simpleRange 5 7)
      statement = NoSemicolonSection (fromList [fooSection, bracketSection]) (simpleRange 1 7)
      expected = fromList [statement]
      in result `shouldBe` Success expected
    
    it "sections (); {}" $ let
      input = "(); {}"
      result = do
        tokens <- lexText input
        parseSections tokens
      statement1 = SemicolonSection (fromList [ParenSection (fromList []) (simpleRange 1 3)]) (simpleRange 1 4)
      statement2 = NoSemicolonSection (fromList [BracketSection (fromList []) (simpleRange 5 7)]) (simpleRange 5 7)
      expected = fromList [ statement1, statement2 ]
      in result `shouldBe` Success expected

    it "fails to section ;" $ let
      input = ";"
      result = do
        tokens <- lexText input
        parseSections tokens
      expectedError = EmptyStatementSectionError (simpleRange 1 2)
      in result `shouldBe` Error expectedError
    
    it "fails to section }" $ let
      input = "}"
      result = do
        tokens <- lexText input
        parseSections tokens
      token = Token{ value = RightBraceToken, range = simpleRange 1 2 }
      expectedError = MismatchedGroupingError Nothing (Just token)
      in result `shouldBe` Error expectedError
    
    it "fails to section )" $ let
      input = ")"
      result = do
        tokens <- lexText input
        parseSections tokens
      token = Token{ value = RightParenToken, range = simpleRange 1 2 }
      expectedError = MismatchedGroupingError Nothing (Just token)
      in result `shouldBe` Error expectedError


  describe "parseBracketSection" $ do
    it "sections {foo}" $ let
      input = "{foo}"
      result = do
        tokens <- lexText input
        parseSections tokens
      fooToken = Token{ value = IdentifierToken "foo", range = simpleRange 2 5 }
      tokenSection = TokenSection fooToken
      statementSection = NoSemicolonSection (fromList [tokenSection]) (simpleRange 2 5)
      bracketSection = BracketSection (fromList [statementSection]) (simpleRange 1 6)
      expected = fromList [ NoSemicolonSection (fromList [bracketSection]) (simpleRange 1 6)]
      in result `shouldBe` Success expected
    
    it "sections {foo; bar}" $ let
      input = "{foo; bar}"
      result = do
        tokens <- lexText input
        parseSections tokens
      fooToken = Token{ value = IdentifierToken "foo", range = simpleRange 2 5 }
      fooSection = TokenSection fooToken
      fooStatement = SemicolonSection (fromList [fooSection]) (simpleRange 2 6)
      barToken = Token{ value = IdentifierToken "bar", range = simpleRange 7 10 }
      barSection = TokenSection barToken
      barStatement = NoSemicolonSection (fromList [barSection]) (simpleRange 7 10)
      bracketSection = BracketSection (fromList [fooStatement, barStatement]) (simpleRange 1 11)
      expected = fromList [ NoSemicolonSection (fromList [bracketSection]) (simpleRange 1 11)]
      in result `shouldBe` Success expected
    
    it "sections {foo; bar;}" $ let
      input = "{foo; bar;}"
      result = do
        tokens <- lexText input
        parseSections tokens
      fooToken = Token{ value = IdentifierToken "foo", range = simpleRange 2 5 }
      fooSection = TokenSection fooToken
      fooStatement = SemicolonSection (fromList [fooSection]) (simpleRange 2 6)
      barToken = Token{ value = IdentifierToken "bar", range = simpleRange 7 10 }
      barSection = TokenSection barToken
      barStatement = SemicolonSection (fromList [barSection]) (simpleRange 7 11)
      bracketSection = BracketSection (fromList [fooStatement, barStatement]) (simpleRange 1 12)
      expected = fromList [ NoSemicolonSection (fromList [bracketSection]) (simpleRange 1 12)]
      in result `shouldBe` Success expected
    
    it "sections {foo bar}" $ let
      input = "{foo bar}"
      result = do
        tokens <- lexText input
        parseSections tokens
      fooToken = Token{ value = IdentifierToken "foo", range = simpleRange 2 5 }
      barToken = Token{ value = IdentifierToken "bar", range = simpleRange 6 9 }
      fooSection = TokenSection fooToken
      barSection = TokenSection barToken
      statementSection = NoSemicolonSection (fromList [fooSection, barSection]) (simpleRange 2 9)
      bracketSection = BracketSection (fromList [statementSection]) (simpleRange 1 10)
      expected = fromList [ NoSemicolonSection (fromList [bracketSection]) (simpleRange 1 10)]
      in result `shouldBe` Success expected
    
    it "sections {foo bar; baz}" $ let
      input = "{foo bar; baz}"
      result = do
        tokens <- lexText input
        parseSections tokens
      fooToken = Token{ value = IdentifierToken "foo", range = simpleRange 2 5 }
      barToken = Token{ value = IdentifierToken "bar", range = simpleRange 6 9 }
      bazToken = Token{ value = IdentifierToken "baz", range = simpleRange 11 14 }
      fooSection = TokenSection fooToken
      barSection = TokenSection barToken
      bazSection = TokenSection bazToken
      statement1 = SemicolonSection (fromList [fooSection, barSection]) (simpleRange 2 10)
      statement2 = NoSemicolonSection (fromList [bazSection]) (simpleRange 11 14)
      bracketSection = BracketSection (fromList [statement1, statement2]) (simpleRange 1 15)
      expected = fromList [ NoSemicolonSection (fromList [bracketSection]) (simpleRange 1 15)]
      in result `shouldBe` Success expected
    
    it "sections {{}}" $ let
      input = "{{}}"
      result = do
        tokens <- lexText input
        parseSections tokens
      innerBracketSection = BracketSection (fromList []) (simpleRange 2 4)
      statementSection = NoSemicolonSection (fromList [innerBracketSection]) (simpleRange 2 4)
      bracketSection = BracketSection (fromList [statementSection]) (simpleRange 1 5)
      expected = fromList [ NoSemicolonSection (fromList [bracketSection]) (simpleRange 1 5)]
      in result `shouldBe` Success expected
    
    it "sections {()}" $ let
      input = "{()}"
      result = do
        tokens <- lexText input
        parseSections tokens
      parenSection = ParenSection (fromList []) (simpleRange 2 4)
      statementSection = NoSemicolonSection (fromList [parenSection]) (simpleRange 2 4)
      bracketSection = BracketSection (fromList [statementSection]) (simpleRange 1 5)
      expected = fromList [ NoSemicolonSection (fromList [bracketSection]) (simpleRange 1 5)]
      in result `shouldBe` Success expected

    it "fails to section {;}" $ let
      input = "{;}"
      result = do
        tokens <- lexText input
        parseSections tokens
      expectedError = EmptyStatementSectionError (simpleRange 2 3)
      in result `shouldBe` Error expectedError
    
    it "fails to section {)}" $ let
      input = "{)}"
      result = do
        tokens <- lexText input
        parseSections tokens
      leftBraceToken = Token{ value = LeftBraceToken, range = simpleRange 1 2}
      rightParenToken = Token{ value = RightParenToken, range = simpleRange 2 3}
      expectedError = MismatchedGroupingError (Just leftBraceToken) (Just rightParenToken)
      in result `shouldBe` Error expectedError


  describe "parseParenSection" $ do
    it "sections (foo)" $ let
      input = "(foo)"
      result = do
        tokens <- lexText input
        parseSections tokens
      fooToken = Token{ value = IdentifierToken "foo", range = simpleRange 2 5 }
      tokenSection = TokenSection fooToken
      parenSection = ParenSection (fromList [tokenSection]) (simpleRange 1 6)
      expected = fromList [ NoSemicolonSection (fromList [parenSection]) (simpleRange 1 6)]
      in result `shouldBe` Success expected
    
    it "sections (foo bar)" $ let
      input = "(foo bar)"
      result = do
        tokens <- lexText input
        parseSections tokens
      fooToken = Token{ value = IdentifierToken "foo", range = simpleRange 2 5 }
      barToken = Token{ value = IdentifierToken "bar", range = simpleRange 6 9 }
      fooSection = TokenSection fooToken
      barSection = TokenSection barToken
      parenSection = ParenSection (fromList [fooSection, barSection]) (simpleRange 1 10)
      expected = fromList [ NoSemicolonSection (fromList [parenSection]) (simpleRange 1 10)]
      in result `shouldBe` Success expected
    
    it "sections (())" $ let
      input = "(())"
      result = do
        tokens <- lexText input
        parseSections tokens
      innerParenSection = ParenSection (fromList []) (simpleRange 2 4)
      parenSection = ParenSection (fromList [innerParenSection]) (simpleRange 1 5)
      expected = fromList [ NoSemicolonSection (fromList [parenSection]) (simpleRange 1 5)]
      in result `shouldBe` Success expected
    
    it "sections ({})" $ let
      input = "({})"
      result = do
        tokens <- lexText input
        parseSections tokens
      bracketSection = BracketSection (fromList []) (simpleRange 2 4)
      parenSection = ParenSection (fromList [bracketSection]) (simpleRange 1 5)
      expected = fromList [ NoSemicolonSection (fromList [parenSection]) (simpleRange 1 5)]
      in result `shouldBe` Success expected

    it "fails to section (;)" $ let
      input = "(;)"
      result = do
        tokens <- lexText input
        parseSections tokens
      leftParenToken = Token{ value = LeftParenToken, range = simpleRange 1 2}
      semicolonToken = Token{ value = SemicolonToken, range = simpleRange 2 3}
      expectedError = MismatchedGroupingError (Just leftParenToken) (Just semicolonToken)
      in result `shouldBe` Error expectedError
    
    it "fails to section (foo;)" $ let
      input = "(foo;)"
      result = do
        tokens <- lexText input
        parseSections tokens
      leftParenToken = Token{ value = LeftParenToken, range = simpleRange 1 2}
      semicolonToken = Token{ value = SemicolonToken, range = simpleRange 5 6}
      expectedError = MismatchedGroupingError (Just leftParenToken) (Just semicolonToken)
      in result `shouldBe` Error expectedError

    it "fails to section (})" $ let
      input = "(})"
      result = do
        tokens <- lexText input
        parseSections tokens
      leftParenToken = Token{ value = LeftParenToken, range = simpleRange 1 2}
      rightBraceToken = Token{ value = RightBraceToken, range = simpleRange 2 3}
      expectedError = MismatchedGroupingError (Just leftParenToken) (Just rightBraceToken)
      in result `shouldBe` Error expectedError