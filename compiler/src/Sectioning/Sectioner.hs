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
    (RightBraceToken _) -> (restTokens, singleError $ UnmatchEndGroupingError currentToken)
    (LeftParenToken _) ->
      let parensParser = do
            nestedParenSection <- parenSectionParser currentToken
            endSections <- fileSectionParser
            return $ nestedParenSection <| endSections
       in runParser parensParser restTokens
    (LeftBraceToken _) ->
      let bracesParser = do
            nestedBraceSection <- braceSectionParser currentToken
            endSections <- fileSectionParser
            return $ nestedBraceSection <| endSections
       in runParser bracesParser restTokens
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
    (RightBraceToken _) -> (restTokens, singleError $ MismatchedGroupingEndsError leftParen currentToken)
    (LeftParenToken _) ->
      let nestedParensParser = do
            nestedParenSection <- parenSectionParser currentToken
            (endSections, rightParen) <- parenSectionHelperParser leftParen
            return (nestedParenSection <| endSections, rightParen)
       in runParser nestedParensParser restTokens
    (LeftBraceToken _) ->
      let nestedBracesParser = do
            nestedBraceSection <- braceSectionParser currentToken
            (endSections, rightParen) <- parenSectionHelperParser leftParen
            return (nestedBraceSection <| endSections, rightParen)
       in runParser nestedBracesParser restTokens
    _ ->
      let continueParser = do
            (endSections, rightParen) <- parenSectionHelperParser leftParen
            return (TokenSection currentToken <| endSections, rightParen)
       in runParser continueParser restTokens

{- This parser should be invoked after hitting a left brace. It consumes tokens until it hits the matching right brace.
Then it outputs a BraceSection with the contents of the braces.
-}
braceSectionParser :: Token -> SectioningParser Section
braceSectionParser leftBrace = do
  (innerSections, rightBrace) <- braceSectionHelperParser leftBrace
  return $ BraceSection (getRange (leftBrace, rightBrace)) innerSections

braceSectionHelperParser :: Token -> SectioningParser (Seq Section, Token)
braceSectionHelperParser leftBrace = SectioningParser $ \tokens -> case tokens of
  Empty -> (Empty, singleError $ UnmatchedStartGroupingError leftBrace)
  currentToken :<| restTokens -> case currentToken of
    (RightBraceToken _) -> (restTokens, Success (Empty, currentToken))
    (RightParenToken _) -> (restTokens, singleError $ MismatchedGroupingEndsError leftBrace currentToken)
    (LeftParenToken _) ->
      let nestedParensParser = do
            nestedParenSection <- parenSectionParser currentToken
            (endSections, rightBrace) <- braceSectionHelperParser leftBrace
            return (nestedParenSection <| endSections, rightBrace)
       in runParser nestedParensParser restTokens
    (LeftBraceToken _) ->
      let nestedBracesParser = do
            nestedBraceSection <- braceSectionParser currentToken
            (endSections, rightBrace) <- braceSectionHelperParser leftBrace
            return (nestedBraceSection <| endSections, rightBrace)
       in runParser nestedBracesParser restTokens
    _ ->
      let continueParser = do
            (endSections, rightBrace) <- braceSectionHelperParser leftBrace
            return (TokenSection currentToken <| endSections, rightBrace)
       in runParser continueParser restTokens
