module Parsing.Parsing
  ( Parser (Parser, runParser),
    (<&&>),
    pNext,
    pRest,
    pTry,
    pOneOrMore,
    pZeroOrMore,
    pZeroOrOne,
    catchUnboundError,
    --   pTryAll,
    --   pTryOr,
    runParserToEnd,
    returnWithErrors,
    ParseState (ParseSuccess, BoundErrors, UnboundError),
    ParseFunction,
    toParseState,
  )
where

import Control.Applicative
import Core.Errors
import Data.Sequence (Seq (Empty, (:<|)), (<|))
import Sectioning.Sectioning (Section)

{- ParseFunction is used in situations where we have a bounded section of code, and we want to parse the whole chunk
into a certain output. The main advantage of using ParseFunction over Parser is that it enables throwing useful errors that
are bound to a certain section of code. However, it is harder to parse complex syntax.
-}
type ParseFunction a = Seq Section -> WithErrors a

-- ParseState
data ParseState a
  = ParseSuccess a
  | BoundErrors (Seq Error)
  | UnboundError

instance Functor ParseState where
  fmap f (ParseSuccess a) = ParseSuccess $ f a
  fmap _ (BoundErrors es) = BoundErrors es
  fmap _ UnboundError = UnboundError

toParseState :: WithErrors a -> ParseState a
toParseState (Success a) = ParseSuccess a
toParseState (Error es) = BoundErrors es

{- The Parser type is used in situation where we want to part something starting at a certain point in code, but not
necessarily knowing where it ends. The main advantage of using Parser over Parsefunction is that Parsers can be combined
flexibly, so they can more easily parse complex syntax. However, it is hard to output useful errors.
-}
newtype Parser a = Parser {runParser :: Seq Section -> (Seq Section, ParseState a)}

instance Functor Parser where
  fmap f parser = Parser $ \sections ->
    let (restSections, parseState) = runParser parser sections
     in (restSections, f <$> parseState)

instance Applicative Parser where
  pure a = Parser $ \sections -> (sections, ParseSuccess a)
  (<*>) parserF parserA = Parser $ \sections ->
    case runParser parserF sections of
      (restSections, ParseSuccess f) -> runParser (f <$> parserA) restSections
      (restSections, BoundErrors es) -> (restSections, BoundErrors es)
      (restSections, UnboundError) -> (restSections, UnboundError)

instance Monad Parser where
  parserA >>= makeParserB = Parser $ \sections ->
    case runParser parserA sections of
      (restSections, ParseSuccess a) -> runParser (makeParserB a) restSections
      (restSections, BoundErrors es) -> (restSections, BoundErrors es)
      (restSections, UnboundError) -> (restSections, UnboundError)

instance Alternative Parser where
  empty = Parser $ \sections -> (sections, UnboundError)
  parser1 <|> parser2 = Parser $ \sections ->
    case runParser parser1 sections of
      (restSections, ParseSuccess a) -> (restSections, ParseSuccess a)
      (_, UnboundError) -> runParser parser2 sections
      (restSections, BoundErrors es) -> case runParser parser2 sections of
        -- We always try to return bound errors over unbound errors, as they are probably more helpful
        (_, UnboundError) -> (restSections, BoundErrors es)
        result -> result

(<&&>) :: Parser a -> (a -> Maybe b) -> Parser b
parser <&&> f = Parser $ \sections ->
  case runParser parser sections of
    (restSections, ParseSuccess a) -> case f a of
      Just b -> (restSections, ParseSuccess b)
      Nothing -> (restSections, UnboundError)
    (restSections, BoundErrors es) -> (restSections, BoundErrors es)
    (restSections, UnboundError) -> (restSections, UnboundError)

pNext :: Parser Section
pNext = Parser $ \sections ->
  case sections of
    nextSection :<| restSections -> (restSections, ParseSuccess nextSection)
    Empty -> (Empty, UnboundError)

pRest :: Parser (Seq Section)
pRest = Parser $ \sections -> (Empty, ParseSuccess sections)

pTry :: Parser a -> Parser a
pTry parser = Parser $ \sections ->
  case runParser parser sections of
    (restSections, ParseSuccess a) -> (restSections, ParseSuccess a)
    (_, UnboundError) -> (sections, UnboundError)
    (_, BoundErrors es) -> (sections, BoundErrors es)

pZeroOrMore :: Parser a -> Parser (Seq a)
pZeroOrMore parser = Parser $ \sections ->
  case runParser parser sections of
    (restSections, ParseSuccess a) -> runParser ((a <|) <$> pZeroOrMore parser) restSections
    _ -> (sections, ParseSuccess [])

pOneOrMore :: Parser a -> Parser (Seq a)
pOneOrMore parser = do
  firstResult <- parser
  restResults <- pZeroOrMore parser
  return (firstResult <| restResults)

pZeroOrOne :: Parser a -> Parser (Maybe a)
pZeroOrOne parser = Parser $ \sections ->
  case runParser parser sections of
    (restSections, ParseSuccess a) -> (restSections, ParseSuccess $ Just a)
    _ -> (sections, ParseSuccess Nothing)

catchUnboundError :: Error -> ParseState a -> WithErrors a
catchUnboundError replacementError parseState = case parseState of
  ParseSuccess a -> Success a
  BoundErrors es -> Error es
  UnboundError -> singleError replacementError

runParserToEnd :: Parser a -> Seq Section -> ParseState a
runParserToEnd parser sections = case runParser parser sections of
  (Empty, ParseSuccess a) -> ParseSuccess a
  (_, ParseSuccess _) -> UnboundError
  (_, err) -> err

returnWithErrors :: WithErrors a -> Parser a
returnWithErrors withErrors = Parser $ \sections -> (sections, toParseState withErrors)