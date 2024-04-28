{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module Sectioning.Sectioner where

import Core.Errors
import Core.FilePositions
import Data.Sequence (Seq (Empty, (:<|)), (<|))
import Lexing.Tokens
import Sectioning.Sectioning

sectionFile :: Seq Token -> WithErrors (Seq Section)
sectionFile tokens = case runParser fileSectionParser tokens of
  (_, Error e) -> Error e
  (Empty, Success sections) -> Success sections
  (_, Success _) -> singleError $ ShouldNotGetHereError "Sectioning halted before reaching file end"

{- Should consume all of the input tokens and output the corresponding sections
-}
fileSectionParser :: SectioningParser (Seq Section)
fileSectionParser = SectioningParser $ \tokens -> case tokens of
  Empty -> (Empty, Success Empty)
  currentToken :<| restTokens -> case currentToken of
    (RightParenToken _) -> (restTokens, singleError $ UnmatchEndGroupingError currentToken)
    (RightCurlyBraceToken _) -> (restTokens, singleError $ UnmatchEndGroupingError currentToken)
    (RightSquareBracketToken _) -> (restTokens, singleError $ UnmatchEndGroupingError currentToken)
    (LeftParenToken _) ->
      let parensParser = do
            nestedParenSection <- parenSectionParser currentToken
            endSections <- fileSectionParser
            return $ nestedParenSection <| endSections
       in runParser parensParser restTokens
    (LeftCurlyBraceToken _) ->
      let curlyBracesParser = do
            nestedCurlyBraceSection <- curlyBraceSectionParser currentToken
            endSections <- fileSectionParser
            return $ nestedCurlyBraceSection <| endSections
       in runParser curlyBracesParser restTokens
    (LeftSquareBracketToken _) ->
      let squareBracketsParser = do
            nestedSquareBracketSection <- squareBracketSectionParser currentToken
            endSections <- fileSectionParser
            return $ nestedSquareBracketSection <| endSections
       in runParser squareBracketsParser restTokens
    _ ->
      let continueParser = do
            endSections <- fileSectionParser
            return $ TokenSection currentToken <| endSections
       in runParser continueParser restTokens

{- This parser should be invoked after hitting a left parenthesis. It consumes tokens until it hits the matching right
parenthesis. Then it outputs a ParenSection with the contents of the parentheses.
-}
parenSectionParser :: Token -> SectioningParser Section
parenSectionParser leftParen = do
  (innerSections, rightParen) <- parenSectionHelperParser leftParen
  return $ ParenSection (getRange (leftParen, rightParen)) innerSections

parenSectionHelperParser :: Token -> SectioningParser (Seq Section, Token)
parenSectionHelperParser leftParen = SectioningParser $ \tokens -> case tokens of
  Empty -> (Empty, singleError $ UnmatchedStartGroupingError leftParen)
  currentToken :<| restTokens -> case currentToken of
    (RightParenToken _) -> (restTokens, Success (Empty, currentToken))
    (RightCurlyBraceToken _) -> (restTokens, singleError $ MismatchedGroupingEndsError leftParen currentToken)
    (RightSquareBracketToken _) -> (restTokens, singleError $ MismatchedGroupingEndsError leftParen currentToken)
    (LeftParenToken _) ->
      let nestedParensParser = do
            nestedParenSection <- parenSectionParser currentToken
            (endSections, rightParen) <- parenSectionHelperParser leftParen
            return (nestedParenSection <| endSections, rightParen)
       in runParser nestedParensParser restTokens
    (LeftCurlyBraceToken _) ->
      let nestedCurlyBracesParser = do
            nestedCurlyBraceSection <- curlyBraceSectionParser currentToken
            (endSections, rightParen) <- parenSectionHelperParser leftParen
            return (nestedCurlyBraceSection <| endSections, rightParen)
       in runParser nestedCurlyBracesParser restTokens
    (LeftSquareBracketToken _) ->
      let nestedSquareBracketsParser = do
            nestedSquareBracketSection <- squareBracketSectionParser currentToken
            (endSections, rightParen) <- parenSectionHelperParser leftParen
            return (nestedSquareBracketSection <| endSections, rightParen)
       in runParser nestedSquareBracketsParser restTokens
    _ ->
      let continueParser = do
            (endSections, rightParen) <- parenSectionHelperParser leftParen
            return (TokenSection currentToken <| endSections, rightParen)
       in runParser continueParser restTokens

{- This parser should be invoked after hitting a left curly brace. It consumes tokens until it hits the matching right
curly brace. Then it outputs a CurlyBraceSection with the contents of the curly braces.
-}
curlyBraceSectionParser :: Token -> SectioningParser Section
curlyBraceSectionParser leftCurlyBrace = do
  (innerSections, rightCurlyBrace) <- curlyBraceSectionHelperParser leftCurlyBrace
  return $ CurlyBraceSection (getRange (leftCurlyBrace, rightCurlyBrace)) innerSections

curlyBraceSectionHelperParser :: Token -> SectioningParser (Seq Section, Token)
curlyBraceSectionHelperParser leftCurlyBrace = SectioningParser $ \tokens -> case tokens of
  Empty -> (Empty, singleError $ UnmatchedStartGroupingError leftCurlyBrace)
  currentToken :<| restTokens -> case currentToken of
    (RightCurlyBraceToken _) -> (restTokens, Success (Empty, currentToken))
    (RightParenToken _) -> (restTokens, singleError $ MismatchedGroupingEndsError leftCurlyBrace currentToken)
    (RightSquareBracketToken _) -> (restTokens, singleError $ MismatchedGroupingEndsError leftCurlyBrace currentToken)
    (LeftParenToken _) ->
      let nestedParensParser = do
            nestedParenSection <- parenSectionParser currentToken
            (endSections, rightCurlyBrace) <- curlyBraceSectionHelperParser leftCurlyBrace
            return (nestedParenSection <| endSections, rightCurlyBrace)
       in runParser nestedParensParser restTokens
    (LeftCurlyBraceToken _) ->
      let nestedCurlyBracesParser = do
            nestedCurlyBraceSection <- curlyBraceSectionParser currentToken
            (endSections, rightCurlyBrace) <- curlyBraceSectionHelperParser leftCurlyBrace
            return (nestedCurlyBraceSection <| endSections, rightCurlyBrace)
       in runParser nestedCurlyBracesParser restTokens
    (LeftSquareBracketToken _) ->
      let nestedSquareBracketsParser = do
            nestedSquareBracketSection <- squareBracketSectionParser currentToken
            (endSections, rightCurlyBrace) <- curlyBraceSectionHelperParser leftCurlyBrace
            return (nestedSquareBracketSection <| endSections, rightCurlyBrace)
       in runParser nestedSquareBracketsParser restTokens
    _ ->
      let continueParser = do
            (endSections, rightCurlyBrace) <- curlyBraceSectionHelperParser leftCurlyBrace
            return (TokenSection currentToken <| endSections, rightCurlyBrace)
       in runParser continueParser restTokens

{- This parser should be invoked after hitting a left square bracket. It consumes tokens until it hits the matching
right square bracket. Then it outputs a SquareBracketSection with the contents of the square brackets.
-}
squareBracketSectionParser :: Token -> SectioningParser Section
squareBracketSectionParser leftSquareBracket = do
  (innerSections, rightSquareBracket) <- squareBracketSectionHelperParser leftSquareBracket
  return $ SquareBracketSection (getRange (leftSquareBracket, rightSquareBracket)) innerSections

squareBracketSectionHelperParser :: Token -> SectioningParser (Seq Section, Token)
squareBracketSectionHelperParser leftSquareBracket = SectioningParser $ \tokens -> case tokens of
  Empty -> (Empty, singleError $ UnmatchedStartGroupingError leftSquareBracket)
  currentToken :<| restTokens -> case currentToken of
    (RightSquareBracketToken _) -> (restTokens, Success (Empty, currentToken))
    (RightParenToken _) -> (restTokens, singleError $ MismatchedGroupingEndsError leftSquareBracket currentToken)
    (RightCurlyBraceToken _) -> (restTokens, singleError $ MismatchedGroupingEndsError leftSquareBracket currentToken)
    (LeftParenToken _) ->
      let nestedParensParser = do
            nestedParenSection <- parenSectionParser currentToken
            (endSections, rightSquareBracket) <- squareBracketSectionHelperParser leftSquareBracket
            return (nestedParenSection <| endSections, rightSquareBracket)
       in runParser nestedParensParser restTokens
    (LeftCurlyBraceToken _) ->
      let nestedCurlyBracesParser = do
            nestedCurlyBraceSection <- curlyBraceSectionParser currentToken
            (endSections, rightSquareBracket) <- squareBracketSectionHelperParser leftSquareBracket
            return (nestedCurlyBraceSection <| endSections, rightSquareBracket)
       in runParser nestedCurlyBracesParser restTokens
    (LeftSquareBracketToken _) ->
      let nestedSquareBracketsParser = do
            nestedSquareBracketSection <- squareBracketSectionParser currentToken
            (endSections, rightSquareBracket) <- squareBracketSectionHelperParser leftSquareBracket
            return (nestedSquareBracketSection <| endSections, rightSquareBracket)
       in runParser nestedSquareBracketsParser restTokens
    _ ->
      let continueParser = do
            (endSections, rightSquareBracket) <- squareBracketSectionHelperParser leftSquareBracket
            return (TokenSection currentToken <| endSections, rightSquareBracket)
       in runParser continueParser restTokens