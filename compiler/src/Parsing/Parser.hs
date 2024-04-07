module Parsing.Parser
  ( fileParser,
    statementParser,
    expressionParser,
    additionLevelExpressionParser,
    multiplicationLevelExpressionParser,
    unaryExpressionParser,
    primaryExpressionParser,
    intLiteralExpressionParser,
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
matchPrint (Token (PrintToken) range) = Success range
matchPrint _ = Error DummyError

matchSemicolon :: Token -> WithError Range
matchSemicolon (Token (SemicolonToken) range) = Success range
matchSemicolon _ = Error DummyError

-- Expressions

expressionParser :: Parser Expression
expressionParser = additionLevelExpressionParser

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
toAddSubtractExpression (Token (PlusToken) _) = Success AddExpression
toAddSubtractExpression (Token (MinusToken) _) = Success SubtractExpression
toAddSubtractExpression _ = Error DummyError

-- Multiplication/division/module level

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
toMultiplyDivideExpression (Token (StarToken) _) = Success MultiplyExpression
toMultiplyDivideExpression (Token (SlashToken) _) = Success DivideExpression
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
toUnaryExpressionAndRange (Token (MinusToken) range) = Success (NegateExpression, range)
toUnaryExpressionAndRange _ = Error DummyError

-- Primary level

primaryExpressionParser :: Parser Expression
primaryExpressionParser = intLiteralExpressionParser <|> parenthesesExpressionParser

intLiteralExpressionParser :: Parser Expression
intLiteralExpressionParser = pNext <&&> toIntLiteralExpression

toIntLiteralExpression :: Token -> WithError Expression
toIntLiteralExpression (Token (IntLiteralToken value) range) = Success $ IntLiteralExpression range value
toIntLiteralExpression _ = Error DummyError

parenthesesExpressionParser :: Parser Expression
parenthesesExpressionParser = do
  leftRange <- pNext <&&> matchLeftParenthesis
  innerExpression <- expressionParser
  rightRange <- pNext <&&> matchRightParenthesis
  return $ ParenthesesExpression (leftRange <> rightRange) innerExpression

matchLeftParenthesis :: Token -> WithError Range
matchLeftParenthesis (Token (LeftParenToken) range) = Success range
matchLeftParenthesis _ = Error DummyError

matchRightParenthesis :: Token -> WithError Range
matchRightParenthesis (Token (RightParenToken) range) = Success range
matchRightParenthesis _ = Error DummyError

-- IntLiteralExpressionParser = pNext <&&> toIntLiteralExpression
