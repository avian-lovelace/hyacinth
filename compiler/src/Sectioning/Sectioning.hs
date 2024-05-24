{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module Sectioning.Sectioning
  ( Section (TokenSection, ParenSection, CurlyBraceSection, SquareBracketSection, AngleBracketSection),
    Sectioner,
    getNextToken,
    Grouping (Parentheses, CurlyBraces, SquareBrackets, AngleBrackets),
    Side (Opening, Closing),
    tokenGroupingInfo,
    makeGroupingSection,
  )
where

import Core.ErrorState
import Core.FilePositions
import Core.Utils
import Data.Sequence (Seq (..))
import Lexing.Tokens

data Section
  = TokenSection Token
  | ParenSection Range (Seq Section)
  | CurlyBraceSection Range (Seq Section)
  | SquareBracketSection Range (Seq Section)
  | AngleBracketSection Range (Seq Section)
  deriving (Show)

instance WithRange Section where
  getRange (TokenSection token) = getRange token
  getRange (ParenSection range _) = range
  getRange (CurlyBraceSection range _) = range
  getRange (SquareBracketSection range _) = range
  getRange (AngleBracketSection range _) = range

instance Pretty Section where
  pretty (TokenSection token) = pretty token
  pretty (ParenSection _ inner) = "( " ++ foldMap (\section -> pretty section ++ " ") inner ++ " )"
  pretty (CurlyBraceSection _ inner) = "{ " ++ foldMap (\section -> pretty section ++ " ") inner ++ " }"
  pretty (SquareBracketSection _ inner) = "[ " ++ foldMap (\section -> pretty section ++ " ") inner ++ " ]"
  pretty (AngleBracketSection _ inner) = "⟨ " ++ foldMap (\section -> pretty section ++ " ") inner ++ " ⟩"

type Sectioner = ErrorState (Seq Token)

getNextToken :: Sectioner (Maybe Token)
getNextToken = do
  tokens <- getState
  case tokens of
    Empty -> return Nothing
    nextToken :<| restTokens -> do
      setState restTokens
      return $ Just nextToken

data Grouping = Parentheses | CurlyBraces | SquareBrackets | AngleBrackets deriving (Eq)

data Side = Opening | Closing

tokenGroupingInfo :: Token -> Maybe (Side, Grouping)
tokenGroupingInfo (LeftParenToken _) = Just (Opening, Parentheses)
tokenGroupingInfo (RightParenToken _) = Just (Closing, Parentheses)
tokenGroupingInfo (LeftCurlyBraceToken _) = Just (Opening, CurlyBraces)
tokenGroupingInfo (RightCurlyBraceToken _) = Just (Closing, CurlyBraces)
tokenGroupingInfo (LeftSquareBracketToken _) = Just (Opening, SquareBrackets)
tokenGroupingInfo (RightSquareBracketToken _) = Just (Closing, SquareBrackets)
tokenGroupingInfo (LeftAngleBracketToken _) = Just (Opening, AngleBrackets)
tokenGroupingInfo (RightAngleBracketToken _) = Just (Closing, AngleBrackets)
tokenGroupingInfo _ = Nothing

makeGroupingSection :: Range -> Grouping -> Seq Section -> Section
makeGroupingSection range Parentheses innerSections = ParenSection range innerSections
makeGroupingSection range CurlyBraces innerSections = CurlyBraceSection range innerSections
makeGroupingSection range SquareBrackets innerSections = SquareBracketSection range innerSections
makeGroupingSection range AngleBrackets innerSections = AngleBracketSection range innerSections