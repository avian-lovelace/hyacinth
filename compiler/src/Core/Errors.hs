module Core.Errors
  ( Error
      ( DummyError,
        LexError,
        UnclosedParenError,
        UnterminatedStringError,
        UnterminatedCharError,
        InvalidCharLiteralError,
        ExpectedTokenError,
        MismatchedGroupingError,
        EmptyStatementSectionError,
        NoSemicolonError,
        EmptyStatementError,
        StatementInvalidFirstTokenError,
        LetMissingEqualsError,
        LetEmptyTypeError,
        AssignableParseError
      ),
    WithError (Error, Success),
    maybeToWithError,
  )
where

import Core.Utils
import Data.List (intercalate)
import Lexing.Tokens

data Error
  = DummyError
  | LexError Position
  | UnclosedParenError Position
  | UnterminatedStringError Position
  | UnterminatedCharError Position
  | InvalidCharLiteralError Position
  | ExpectedTokenError [TokenValue] (Maybe TokenValue) Position
  | MismatchedGroupingError (Maybe Token) (Maybe Token)
  | EmptyStatementSectionError Range
  | NoSemicolonError Range
  | EmptyStatementError Range
  | StatementInvalidFirstTokenError Range
  | LetMissingEqualsError Range
  | LetEmptyTypeError Range
  | AssignableParseError Range
  deriving (Show, Eq)

instance Pretty Error where
  pretty err = case err of
    LexError position -> "Encountered unexpected character at " ++ pretty position
    UnclosedParenError position -> "Missing ) at " ++ pretty position
    UnterminatedStringError position -> "Encountered unteriminated string at " ++ pretty position
    UnterminatedCharError position -> "Encountered unteriminated char at " ++ pretty position
    InvalidCharLiteralError position -> "Char literals must contain exactly one character " ++ pretty position
    ExpectedTokenError expecteds maybeActual position ->
      ( "Expected "
          ++ (printList $ pretty <$> expecteds)
          ++ " but saw "
          ++ (printMaybe $ pretty <$> maybeActual)
          ++ " at "
          ++ (pretty position)
      )
    where
      printMaybe :: Maybe String -> String
      printMaybe Nothing = "nothing"
      printMaybe (Just s) = s

printList :: [String] -> String
printList items =
  if
    | length items == 0 -> "nothing"
    | length items == 1 -> items !! 0
    | length items == 2 -> items !! 0 ++ "or" ++ items !! 1
    | otherwise -> intercalate ", " (init items) ++ ", or" ++ last items

data WithError a
  = Error Error
  | Success a
  deriving (Eq, Show, Functor)

instance Applicative WithError where
  pure = Success
  (<*>) (Error e) _ = Error e
  (<*>) _ (Error e) = Error e
  (<*>) (Success f) (Success x) = Success (f x)

instance Monad WithError where
  (>>=) m f = case m of
    Error e -> Error e
    Success r -> f r

maybeToWithError :: Error -> Maybe a -> WithError a
maybeToWithError err maybeValue = case maybeValue of
  Nothing -> Error err
  Just value -> Success value