module Parsing.Parser
  ( fileParser,
    statementParser,
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
import Core.Utils
import Data.Foldable
import Lexing.Tokens
import Parsing.Parsing
import Parsing.SyntaxTree

fileParser :: Parser FileScope
fileParser = do
  statements <- pZeroOrMore statementParser
  pEnd
  return $ FileScope statements

-- Statements

statementParser :: Parser Statement
statementParser = printStatementParser

printStatementParser :: Parser Statement
printStatementParser = do
  printRange <- pNext <&&> matchPrint
  expression <- expressionParser
  semicolonRange <- pNext <&&> matchSemicolon
  return $ PrintStatement (printRange <> semicolonRange) expression

matchPrint :: Token -> WithError Range
matchPrint (Token PrintToken range) = Success range
matchPrint _ = Error DummyError

matchSemicolon :: Token -> WithError Range
matchSemicolon (Token SemicolonToken range) = Success range
matchSemicolon _ = Error DummyError

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
    makeExpression left (operator, right) = operator (getUnionRange [left, right]) left right

toLogicalExpression :: Token -> WithError (Range -> Expression -> Expression -> Expression)
toLogicalExpression (Token AndToken _) = Success AndExpression
toLogicalExpression (Token OrToken _) = Success OrExpression
toLogicalExpression _ = Error DummyError

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
    Just (operator, rightExpression) -> operator (getUnionRange [leftExpression, rightExpression]) leftExpression rightExpression

toEqualityExpression :: Token -> WithError (Range -> Expression -> Expression -> Expression)
toEqualityExpression (Token EqualEqualToken _) = Success EqualExpression
toEqualityExpression (Token NotEqualToken _) = Success NotEqualExpression
toEqualityExpression _ = Error DummyError

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
    Just (operator, rightExpression) -> operator (getUnionRange [leftExpression, rightExpression]) leftExpression rightExpression

toComparisonExpression :: Token -> WithError (Range -> Expression -> Expression -> Expression)
toComparisonExpression (Token GreaterToken _) = Success GreaterExpression
toComparisonExpression (Token LessToken _) = Success LessExpression
toComparisonExpression (Token GreaterEqualToken _) = Success GreaterEqualExpression
toComparisonExpression (Token LessEqualToken _) = Success LessEqualExpression
toComparisonExpression _ = Error DummyError

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
    makeExpression left (operator, right) = operator (getUnionRange [left, right]) left right

toAddSubtractExpression :: Token -> WithError (Range -> Expression -> Expression -> Expression)
toAddSubtractExpression (Token PlusToken _) = Success AddExpression
toAddSubtractExpression (Token MinusToken _) = Success SubtractExpression
toAddSubtractExpression _ = Error DummyError

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
    makeExpression left (operator, right) = operator (getUnionRange [left, right]) left right

toMultiplyDivideExpression :: Token -> WithError (Range -> Expression -> Expression -> Expression)
toMultiplyDivideExpression (Token StarToken _) = Success MultiplyExpression
toMultiplyDivideExpression (Token SlashToken _) = Success DivideExpression
toMultiplyDivideExpression (Token PercentToken _) = Success ModuloExpression
toMultiplyDivideExpression _ = Error DummyError

-- Unary level

-- unaryExpressionParser :: Parser Expression
unaryExpressionParser :: Parser Expression
unaryExpressionParser = do
  operators <- pZeroOrMore $ pNext <&&> toUnaryExpressionAndRange
  primary <- primaryExpressionParser
  return $ foldr makeExpression primary operators
  where
    makeExpression (operator, range) innerExpression = operator (range <> getRange innerExpression) innerExpression

toUnaryExpressionAndRange :: Token -> WithError (Range -> Expression -> Expression, Range)
toUnaryExpressionAndRange (Token MinusToken range) = Success (NegateExpression, range)
toUnaryExpressionAndRange (Token BangToken range) = Success (NotExpression, range)
toUnaryExpressionAndRange _ = Error DummyError

-- Primary level

primaryExpressionParser :: Parser Expression
primaryExpressionParser = literalExpressionParser <|> parenthesesExpressionParser

literalExpressionParser :: Parser Expression
literalExpressionParser = pNext <&&> toLiteralExpression

toLiteralExpression :: Token -> WithError Expression
toLiteralExpression (Token (IntLiteralToken value) range) = Success $ IntLiteralExpression range value
toLiteralExpression (Token (DoubleLiteralToken value) range) = Success $ DoubleLiteralExpression range value
toLiteralExpression (Token (BoolLiteralToken value) range) = Success $ BoolLiteralExpression range value
toLiteralExpression _ = Error DummyError

parenthesesExpressionParser :: Parser Expression
parenthesesExpressionParser = do
  pNext <&&> matchLeftParenthesis
  innerExpression <- expressionParser
  pNext <&&> matchRightParenthesis
  return innerExpression

matchLeftParenthesis :: Token -> WithError ()
matchLeftParenthesis (Token LeftParenToken _) = Success ()
matchLeftParenthesis _ = Error DummyError

matchRightParenthesis :: Token -> WithError ()
matchRightParenthesis (Token RightParenToken _) = Success ()
matchRightParenthesis _ = Error DummyError

-- IntLiteralExpressionParser = pNext <&&> toIntLiteralExpression
