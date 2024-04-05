module Parsing.Parser (
  fileParser,
  statementParser,
  expressionParser,
  addSubtractExpressionParser,
  multiplyDivideExpressionParser,
  unaryExpressionParser,
  primaryExpressionParser,
  intLiteralExpressionParser
) where

import Core.Utils
import Core.Errors
import Lexing.Tokens
import Parsing.SyntaxTree
import Data.Foldable
import Control.Applicative

import Parsing.Parsing


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

matchPrint (Token (PrintToken) range) = Success range
matchPrint _ = Error DummyError

matchSemicolon (Token (SemicolonToken) range) = Success range
matchSemicolon _ = Error DummyError

-- Expressions

expressionParser :: Parser Expression
expressionParser = addSubtractExpressionParser

-- Addition/subtraction level

addSubtractExpressionParser :: Parser Expression
addSubtractExpressionParser = do
  leftExpression <- multiplyDivideExpressionParser
  rightExpressions <- pZeroOrMore $ do
    operator <- pNext <&&> toAddSubtractExpression
    rightExpression <- multiplyDivideExpressionParser
    return (operator, rightExpression)
  return $ foldl' makeExpression leftExpression rightExpressions
  where makeExpression left (operator, right) = operator (getUnionRange [left, right]) left right

toAddSubtractExpression (Token (PlusToken) _) = Success AddExpression
toAddSubtractExpression (Token (MinusToken) _) = Success SubtractExpression
toAddSubtractExpression _ = Error DummyError



-- Multiplication/division level

multiplyDivideExpressionParser :: Parser Expression
multiplyDivideExpressionParser = do
  leftExpression <- unaryExpressionParser
  rightExpressions <- pZeroOrMore $ do
    operator <- pNext <&&> toMultiplyDivideExpression
    rightExpression <- unaryExpressionParser
    return (operator, rightExpression)
  return $ foldl' makeExpression leftExpression rightExpressions
  where makeExpression left (operator, right) = operator (getUnionRange [left, right]) left right

toMultiplyDivideExpression (Token (StarToken) _) = Success MultiplyExpression
toMultiplyDivideExpression (Token (SlashToken) _) = Success DivideExpression
toMultiplyDivideExpression _ = Error DummyError

-- Unary level

-- unaryExpressionParser :: Parser Expression
unaryExpressionParser = do
  operators <- pZeroOrMore $ pNext <&&> toUnaryExpressionAndRange
  primary <- primaryExpressionParser
  return $ foldr makeExpression primary operators
  where
    makeExpression (operator, range) innerExpression = operator (range <> getRange innerExpression) innerExpression


toUnaryExpressionAndRange (Token (MinusToken) range) = Success (NegateExpression, range)
toUnaryExpressionAndRange _ = Error DummyError

-- Primary level

primaryExpressionParser :: Parser Expression
primaryExpressionParser = intLiteralExpressionParser <|> parenthesesExpressionParser

intLiteralExpressionParser :: Parser Expression
intLiteralExpressionParser = pNext <&&> toIntLiteralExpression

toIntLiteralExpression (Token (IntLiteralToken value) range) = Success $ IntLiteralExpression range value
toIntLiteralExpression _ = Error DummyError

parenthesesExpressionParser :: Parser Expression
parenthesesExpressionParser = do
  leftRange <- pNext <&&> matchLeftParenthesis
  innerExpression <- expressionParser
  rightRange <- pNext <&&> matchRightParenthesis
  return $ ParenthesesExpression (leftRange <> rightRange) innerExpression

matchLeftParenthesis (Token (LeftParenToken) range) = Success range
matchLeftParenthesis _ = Error DummyError

matchRightParenthesis (Token (RightParenToken) range) = Success range
matchRightParenthesis _ = Error DummyError

-- IntLiteralExpressionParser = pNext <&&> toIntLiteralExpression

