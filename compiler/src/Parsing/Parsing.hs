module Parsing.Parsing
  ( Parser (runParser),
    (<&&>),
    pNext,
    pEnd,
    pTry,
    pOneOrMore,
    pZeroOrMore,
    pZeroOrOne,
    --   pTryAll,
    --   pTryOr
  )
where

import Control.Applicative
import Core.Errors
import Data.Sequence (Seq (Empty, (:<|)))
import Lexing.Tokens

newtype Parser a = Parser {runParser :: (Seq Token -> (Seq Token, WithError a))}

instance Functor Parser where
  -- test
  fmap f parser = Parser $ \tokens ->
    case runParser parser tokens of
      (restTokens, Success a) -> (restTokens, Success $ f a)
      (restTokens, Error e) -> (restTokens, Error e)

instance Applicative Parser where
  pure a = Parser $ \tokens -> (tokens, Success a)
  (<*>) parserF parserA = Parser $ \tokens ->
    case runParser parserF tokens of
      (restTokens, Success f) -> runParser (fmap f parserA) restTokens
      (restTokens, Error e) -> (restTokens, Error e)

instance Monad Parser where
  parserA >>= makeParserB = Parser $ \tokens ->
    case runParser parserA tokens of
      (restTokens, Success a) -> runParser (makeParserB a) restTokens
      (restTokens, Error e) -> (restTokens, Error e)

instance Alternative Parser where
  empty = Parser $ \tokens -> (tokens, Error DummyError)
  parser1 <|> parser2 = Parser $ \tokens ->
    case runParser parser1 tokens of
      (restTokens, Success a) -> (restTokens, Success a)
      (_, Error _) -> runParser parser2 tokens

(<&&>) :: Parser a -> (a -> WithError b) -> Parser b
parser <&&> f = Parser $ \tokens ->
  case runParser parser tokens of
    (restTokens, Success a) -> (restTokens, f a)
    (restTokens, Error e) -> (restTokens, Error e)

--  |test
pNext :: Parser Token
pNext = Parser $ \tokens ->
  case tokens of
    nextToken :<| restTokens -> (restTokens, Success nextToken)
    Empty -> (Empty, Error DummyError)

pEnd :: Parser ()
pEnd = Parser $ \tokens ->
  case tokens of
    Empty -> (Empty, Success ())
    _ -> (tokens, Error DummyError)

-- pNextSatisfies :: (Token -> Bool) -> Parser Token
-- pNextSatisfies condition = Parser $ \tokens ->
--   case tokens of
--     nextToken :<| restTokens -> if condition nextToken
--       then (restTokens, Success nextToken)
--       else (tokens, Error DummyError)
--     Empty -> (Empty, Error DummyError)

-- pNextMatches :: Token -> Parser Token
-- pNextMatches token = pNextSatisfies (==token)

-- test
pTry :: Parser a -> Parser a
pTry parser = Parser $ \tokens ->
  case runParser parser tokens of
    (restTokens, Success a) -> (restTokens, Success a)
    (_, Error e) -> (tokens, Error e)

-- pTryAll :: [Parser a] -> Parser a
-- pTryAll [] = Parser $ \tokens -> (tokens, Error DummyError)
-- pTryAll (parser : restParsers) = Parser $ \tokens ->
--   case runParser parser tokens of
--     (restTokens, Success a) -> (restTokens, Success a)
--     (_, Error e) -> runParser (pTryAll restParsers) tokens

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore parser = Parser $ \tokens ->
  case runParser parser tokens of
    (restTokens, Success a) -> runParser (fmap (a :) (pZeroOrMore parser)) restTokens
    (_, Error _) -> (tokens, Success [])

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore parser = do
  firstResult <- parser
  restResults <- pZeroOrMore parser
  return (firstResult : restResults)

pZeroOrOne :: Parser a -> Parser (Maybe a)
pZeroOrOne parser = Parser $ \tokens ->
  case runParser parser tokens of
    (restTokens, Success a) -> (restTokens, Success $ Just a)
    (_, Error _) -> (tokens, Success Nothing)