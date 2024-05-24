module Sectioning.Sectioner where

import Core.ErrorState
import Core.Errors
import Core.FilePositions
import Data.Sequence (Seq (..), (<|))
import Lexing.Tokens
import Sectioning.Sectioning

sectionFile :: Seq Token -> WithErrors (Seq Section)
sectionFile tokens = case runErrorState fileSectionParser tokens of
  (_, Error e) -> Error e
  (Empty, Success sections) -> Success sections
  (_, Success _) -> singleError $ ShouldNotGetHereError "Sectioning halted before reaching file end"

fileSectionParser :: Sectioner (Seq Section)
fileSectionParser = do
  maybeNextToken <- getNextToken
  case maybeNextToken of
    Nothing -> return Empty
    Just nextToken -> case tokenGroupingInfo nextToken of
      Nothing -> do
        restSections <- fileSectionParser
        return $ TokenSection nextToken <| restSections
      Just (Opening, newGroupingType) -> do
        nestedGroupingSection <- groupingSectionParser nextToken newGroupingType
        restSections <- fileSectionParser
        return $ nestedGroupingSection <| restSections
      Just (Closing, _) -> throwError $ UnmatchEndGroupingError nextToken

groupingSectionParser :: Token -> Grouping -> Sectioner Section
groupingSectionParser groupingStartToken currentGroupingType = do
  (nestedGroupingSectionRange, nestedGroupingSectionContents) <- groupingSectionContentsParser groupingStartToken currentGroupingType
  return $ makeGroupingSection nestedGroupingSectionRange currentGroupingType nestedGroupingSectionContents

groupingSectionContentsParser :: Token -> Grouping -> Sectioner (Range, Seq Section)
groupingSectionContentsParser groupingStartToken currentGroupingType = do
  maybeNextToken <- getNextToken
  case maybeNextToken of
    Nothing -> throwError $ UnmatchedStartGroupingError groupingStartToken
    Just nextToken -> case tokenGroupingInfo nextToken of
      Nothing -> do
        (groupingRange, restSections) <- groupingSectionContentsParser groupingStartToken currentGroupingType
        return (groupingRange, TokenSection nextToken <| restSections)
      Just (Opening, newGroupingType) -> do
        nestedGroupingSection <- groupingSectionParser nextToken newGroupingType
        (currentGroupingSectionRange, currentGroupingSectionContents) <- groupingSectionContentsParser groupingStartToken currentGroupingType
        return (currentGroupingSectionRange, nestedGroupingSection <| currentGroupingSectionContents)
      Just (Closing, closedGroupingType) ->
        if currentGroupingType == closedGroupingType
          then return (getRange groupingStartToken <> getRange nextToken, Empty)
          else throwError $ MismatchedGroupingEndsError groupingStartToken nextToken