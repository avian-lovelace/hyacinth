module Lexing.Lexer
  ( lexText,
  )
where

import Control.Monad.State (State, get, put, runState)
import Core.Errors
import Core.Utils
import Data.Char (isAlpha, isDigit, isSpace)
import Data.Foldable (asum)
import Data.Sequence (Seq (Empty), singleton, (><))
import qualified Data.Text as Text
import Lexing.TextWalker
import Lexing.Tokens

type Lexer = State FileState (Maybe (WithError (Seq Token)))

lexText :: Text.Text -> WithError (Seq Token)
lexText text = fst $ runState (lexHelper Empty) (initFileState text)

lexHelper :: Seq Token -> State FileState (WithError (Seq Token))
lexHelper tokens = do
  fileState <- get
  if
    | (text fileState == Text.empty) -> return $ Success tokens
    | otherwise -> do
        lexResult <- combinedLexer
        case lexResult of
          Error _ -> return lexResult
          Success lexedTokens -> lexHelper (tokens >< lexedTokens)

combinedLexer :: State FileState (WithError (Seq Token))
combinedLexer = do
  state <- get
  position <- getPosition
  let runResults = (\lexer -> runState lexer state) <$> lexers
  let unwrappedResults = unwrap <$> runResults
  let firstSuccess = asum unwrappedResults
  case firstSuccess of
    Just (lexerResult, updatedState) -> do
      put updatedState
      return lexerResult
    Nothing -> return $ Error $ LexError position
  where
    unwrap (result, state) = case result of
      Just innerResult -> Just (innerResult, state)
      Nothing -> Nothing

lexers :: [Lexer]
lexers =
  [ lexWhiteSpace,
    -- Multiple character symbols
    lexMultiCharSymbol "->" SingleRightArrowToken,
    lexMultiCharSymbol "=>" DoubleRightArrowToken,
    lexMultiCharSymbol "&&" AndToken,
    lexMultiCharSymbol "||" OrToken,
    lexMultiCharSymbol "++" PlusPlusToken,
    lexMultiCharSymbol ">=" GreaterEqualToken,
    lexMultiCharSymbol "<=" LessEqualToken,
    -- Symbols
    lexSymbol ';' SemicolonToken,
    lexSymbol ':' ColonToken,
    lexSymbol '=' EqualsToken,
    lexSymbol '|' PipeToken,
    lexSymbol ',' CommaToken,
    -- Grouping
    lexSymbol '(' LeftParenToken,
    lexSymbol ')' RightParenToken,
    lexSymbol '{' LeftBraceToken,
    lexSymbol '}' RightBraceToken,
    -- Operators
    lexSymbol '+' PlusToken,
    lexSymbol '-' MinusToken,
    lexSymbol '*' StarToken,
    lexSymbol '/' SlashToken,
    lexSymbol '!' BangToken,
    lexSymbol '>' GreaterToken,
    lexSymbol '<' LessToken,
    -- Alphanumeric
    lexNumericLiteral,
    lexStringLiteral,
    lexCharLiteral,
    lexKeywordOrIdentifier
  ]

lexWhiteSpace :: Lexer
lexWhiteSpace = do
  whiteSpace <- consumeWhile isSpace
  if
    | whiteSpace == Text.empty -> return Nothing
    | otherwise -> return $ Just $ Success $ Empty

lexSymbol :: Char -> TokenValue -> Lexer
lexSymbol char tokenValue = do
  start <- getPosition
  consumeResult <- consumeIf (== char)
  case consumeResult of
    Consumed (Just _) -> do
      end <- getPosition
      let token = Token {value = tokenValue, range = Range {start, end}}
      return $ Just $ Success $ singleton token
    _ -> return Nothing

lexMultiCharSymbol :: String -> TokenValue -> Lexer
lexMultiCharSymbol keyword tokenValue = do
  start <- getPosition
  consumeResult <- consumeString keyword nextIsNotIdentifierChar
  case consumeResult of
    Just _ -> do
      end <- getPosition
      let token = Token {value = tokenValue, range = Range {start, end}}
      return $ Just $ Success $ singleton token
    _ -> return Nothing
  where
    nextIsNotIdentifierChar maybeNext = case maybeNext of
      Nothing -> True
      Just next -> not $ isIdentifierChar next

lexNumericLiteral :: Lexer
lexNumericLiteral = do
  start <- getPosition
  intText <- consumeWhile isDigit
  if
    | intText == Text.empty -> return Nothing
    | otherwise -> do
        consumePeriodResult <- consumeIf (== '.')
        case consumePeriodResult of
          Consumed (Just _) -> do
            decimalText <- consumeWhile isDigit
            let decimalText' = if Text.length decimalText == 0 then "0" else decimalText
            end <- getPosition
            let doubleValue = read $ Text.unpack (intText <> "." <> decimalText')
            let token = Token {value = DoubleLiteralToken doubleValue, range = Range {start, end}}
            return $ Just $ Success $ singleton token
          _ -> do
            end <- getPosition
            let intValue = read $ Text.unpack intText
            let token = Token {value = IntLiteralToken intValue, range = Range {start, end}}
            return $ Just $ Success $ singleton token

lexStringLiteral :: Lexer
lexStringLiteral = do
  start <- getPosition
  consumeQuoteResult <- consumeIf (== '"')
  case consumeQuoteResult of
    Consumed (Just _) -> do
      consumeBodyResult <- consumeUntil (== '"')
      case consumeBodyResult of
        HitEOF -> return $ Just $ Error $ UnterminatedStringError start
        Consumed stringText -> do
          end <- getPosition
          let token = Token {value = StringLiteralToken stringText, range = Range {start, end}}
          return $ Just $ Success $ singleton token
    _ -> return Nothing

lexCharLiteral :: Lexer
lexCharLiteral = do
  start <- getPosition
  consumeQuoteResult <- consumeIf (== '\'')
  case consumeQuoteResult of
    Consumed (Just _) -> do
      consumeBodyResult <- consumeUntil (== '\'')
      case consumeBodyResult of
        HitEOF -> return $ Just $ Error $ UnterminatedCharError start
        Consumed stringText -> do
          end <- getPosition
          if
            | Text.length stringText == 1 -> do
                let token = Token {value = CharLiteralToken $ Text.head stringText, range = Range {start, end}}
                return $ Just $ Success $ singleton token
            | otherwise -> do
                return $ Just $ Error $ InvalidCharLiteralError start
    _ -> return Nothing

lexKeywordOrIdentifier :: Lexer
lexKeywordOrIdentifier = do
  start <- getPosition
  matchResult <- matchNext isIdentifierStartChar
  case matchResult of
    Consumed True -> do
      identifierText <- consumeWhile isIdentifierChar
      end <- getPosition
      let tokenValue =
            ( case identifierText of
                "type" -> TypeToken
                "let" -> LetToken
                "if" -> IfToken
                "else" -> ElseToken
                "fn" -> FnToken
                "match" -> MatchToken
                "of" -> OfToken
                "print" -> PrintToken
                "Int" -> IntToken
                "Double" -> DoubleToken
                "Char" -> CharToken
                "String" -> StringToken
                "Bool" -> BoolToken
                "true" -> BoolLiteralToken True
                "false" -> BoolLiteralToken False
                _ -> IdentifierToken identifierText
            )
      let token = Token {value = tokenValue, range = Range {start, end}}
      return $ Just $ Success $ singleton token
    _ -> return Nothing

isIdentifierStartChar :: Char -> Bool
isIdentifierStartChar = isAlpha

isIdentifierChar :: Char -> Bool
isIdentifierChar char = isAlpha char || isDigit char || char == '_'