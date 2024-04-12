module Parsing.Parser
  ( parseFile,
    -- statementParser,
    expressionParser,
    additionLevelExpressionParser,
    multiplicationLevelExpressionParser,
    unaryExpressionParser,
    primaryExpressionParser,
    literalExpressionParser,
  )
where

import Control.Applicative
import Core.Errors
import Core.FilePositions
import Core.Utils
import Data.Foldable
import Data.Sequence (Seq (Empty, (:<|)), breakl, singleton, (<|))
import Lexing.Tokens
import Parsing.Parsing
import Parsing.SyntaxTree
import Sectioning.Sectioning (Section (ParenSection, TokenSection))

parseFile :: ParseFunction FileScope
parseFile sections = FileScope <$> foldrWithErrors (<|) Empty (parseStatements sections)

parseStatements :: Seq Section -> Seq (WithErrors Statement)
parseStatements Empty = Empty
parseStatements sections = case breakl matchSemicolon sections of
  (statementSections, Empty) -> singleton $ Error [ExpectedToEndWithSemicolonError $ getUnionRange (seqHead statementSections, seqTail statementSections)]
  (statementSections, _semicolon :<| restTokens) -> parseStatement statementSections <| parseStatements restTokens

matchSemicolon :: Section -> Bool
matchSemicolon (TokenSection (SemicolonToken _)) = True
matchSemicolon _ = False

-- Statements

parseStatement :: ParseFunction Statement
parseStatement sections = case sections of
  TokenSection (PrintToken range) :<| tailSections -> parsePrintStatement range tailSections
  _ -> singleError $ ShouldNotGetHereError "To be implemented"

parsePrintStatement :: Range -> ParseFunction Statement
parsePrintStatement printTokenRange expressionSections = case expressionSections of
  Empty -> singleError $ PrintStatementEmptyExpressionError printTokenRange
  _ -> PrintStatement statementRange <$> expressionOrErrors
    where
      statementRange = getUnionRange (printTokenRange, seqTail expressionSections)
      expressionRange = getUnionRange (seqHead expressionSections, seqTail expressionSections)
      expressionOrErrors = catchUnboundError (PrintStatementInvalidExpressionError expressionRange) $ runParserToEnd expressionParser expressionSections

-- Expressions

expressionParser :: Parser Expression
expressionParser = logicalLevelExpressionParser

-- Logical level

logicalLevelExpressionParser :: Parser Expression
logicalLevelExpressionParser = do
  leftExpression <- equalityLevelExpressionParser
  rightExpressions <- pZeroOrMore $ do
    operator <- pNext <&&> toLogicalExpression
    rightExpression <- equalityLevelExpressionParser
    return (operator, rightExpression)
  return $ foldl' makeExpression leftExpression rightExpressions
  where
    makeExpression left (operator, right) = operator (getUnionRange (left, right)) left right

toLogicalExpression :: Section -> Maybe (Range -> Expression -> Expression -> Expression)
toLogicalExpression (TokenSection (AndToken _)) = Just AndExpression
toLogicalExpression (TokenSection (OrToken _)) = Just OrExpression
toLogicalExpression _ = Nothing

-- Equality level

equalityLevelExpressionParser :: Parser Expression
equalityLevelExpressionParser = do
  leftExpression <- comparisonLevelExpressionParser
  rightSide <- pZeroOrOne $ do
    operator <- pNext <&&> toEqualityExpression
    rightExpression <- comparisonLevelExpressionParser
    return (operator, rightExpression)
  return $ case rightSide of
    Nothing -> leftExpression
    Just (operator, rightExpression) -> operator (getUnionRange (leftExpression, rightExpression)) leftExpression rightExpression

toEqualityExpression :: Section -> Maybe (Range -> Expression -> Expression -> Expression)
toEqualityExpression (TokenSection (EqualEqualToken _)) = Just EqualExpression
toEqualityExpression (TokenSection (NotEqualToken _)) = Just NotEqualExpression
toEqualityExpression _ = Nothing

-- Comparison level

comparisonLevelExpressionParser :: Parser Expression
comparisonLevelExpressionParser = do
  leftExpression <- additionLevelExpressionParser
  rightSide <- pZeroOrOne $ do
    operator <- pNext <&&> toComparisonExpression
    rightExpression <- additionLevelExpressionParser
    return (operator, rightExpression)
  return $ case rightSide of
    Nothing -> leftExpression
    Just (operator, rightExpression) -> operator (getUnionRange (leftExpression, rightExpression)) leftExpression rightExpression

toComparisonExpression :: Section -> Maybe (Range -> Expression -> Expression -> Expression)
toComparisonExpression (TokenSection (GreaterToken _)) = Just GreaterExpression
toComparisonExpression (TokenSection (LessToken _)) = Just LessExpression
toComparisonExpression (TokenSection (GreaterEqualToken _)) = Just GreaterEqualExpression
toComparisonExpression (TokenSection (LessEqualToken _)) = Just LessEqualExpression
toComparisonExpression _ = Nothing

-- Addition/subtraction level

additionLevelExpressionParser :: Parser Expression
additionLevelExpressionParser = do
  leftExpression <- multiplicationLevelExpressionParser
  rightExpressions <- pZeroOrMore $ do
    operator <- pNext <&&> toAddSubtractExpression
    rightExpression <- multiplicationLevelExpressionParser
    return (operator, rightExpression)
  return $ foldl' makeExpression leftExpression rightExpressions
  where
    makeExpression left (operator, right) = operator (getUnionRange (left, right)) left right

toAddSubtractExpression :: Section -> Maybe (Range -> Expression -> Expression -> Expression)
toAddSubtractExpression (TokenSection (PlusToken _)) = Just AddExpression
toAddSubtractExpression (TokenSection (MinusToken _)) = Just SubtractExpression
toAddSubtractExpression _ = Nothing

-- Multiplication/division/modulo level

multiplicationLevelExpressionParser :: Parser Expression
multiplicationLevelExpressionParser = do
  leftExpression <- unaryExpressionParser
  rightExpressions <- pZeroOrMore $ do
    operator <- pNext <&&> toMultiplyDivideExpression
    rightExpression <- unaryExpressionParser
    return (operator, rightExpression)
  return $ foldl' makeExpression leftExpression rightExpressions
  where
    makeExpression left (operator, right) = operator (getUnionRange (left, right)) left right

toMultiplyDivideExpression :: Section -> Maybe (Range -> Expression -> Expression -> Expression)
toMultiplyDivideExpression (TokenSection (StarToken _)) = Just MultiplyExpression
toMultiplyDivideExpression (TokenSection (SlashToken _)) = Just DivideExpression
toMultiplyDivideExpression (TokenSection (PercentToken _)) = Just ModuloExpression
toMultiplyDivideExpression _ = Nothing

-- Unary level

-- unaryExpressionParser :: Parser Expression
unaryExpressionParser :: Parser Expression
unaryExpressionParser = do
  operators <- pZeroOrMore $ pNext <&&> toUnaryExpressionAndRange
  primary <- primaryExpressionParser
  return $ foldr makeExpression primary operators
  where
    makeExpression (operator, range) innerExpression = operator (range <> getRange innerExpression) innerExpression

toUnaryExpressionAndRange :: Section -> Maybe (Range -> Expression -> Expression, Range)
toUnaryExpressionAndRange (TokenSection (MinusToken range)) = Just (NegateExpression, range)
toUnaryExpressionAndRange (TokenSection (BangToken range)) = Just (NotExpression, range)
toUnaryExpressionAndRange _ = Nothing

-- Primary level

primaryExpressionParser :: Parser Expression
primaryExpressionParser = literalExpressionParser <|> parenthesesExpressionParser

literalExpressionParser :: Parser Expression
literalExpressionParser = pNext <&&> toLiteralExpression

toLiteralExpression :: Section -> Maybe Expression
toLiteralExpression (TokenSection (IntLiteralToken range value)) = Just $ IntLiteralExpression range value
toLiteralExpression (TokenSection (DoubleLiteralToken range value)) = Just $ DoubleLiteralExpression range value
toLiteralExpression (TokenSection (BoolLiteralToken range value)) = Just $ BoolLiteralExpression range value
toLiteralExpression _ = Nothing

parenthesesExpressionParser :: Parser Expression
parenthesesExpressionParser = do
  (range, innerSections) <- pNext <&&> matchParenSection
  innerExpression <- returnWithErrors $ catchUnboundError (ExpectedExpressionInParensError range) $ runParserToEnd expressionParser innerSections
  return innerExpression

matchParenSection :: Section -> Maybe (Range, Seq Section)
matchParenSection (ParenSection range innerSections) = Just (range, innerSections)
matchParenSection _ = Nothing