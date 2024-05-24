module Lexing.Lexer
  ( lexText,
  )
where

import Control.Monad.State (State, get, put, runState)
import Core.Errors
import Core.FilePositions
import Data.Char (isAlpha, isDigit, isSpace)
import Data.Foldable (asum)
import Data.Sequence (Seq (Empty), singleton, (><))
import qualified Data.Text as Text
import Lexing.TextWalker
import Lexing.Tokens

type Lexer = State FileState (Maybe (WithErrors (Seq Token)))

lexText :: Text.Text -> WithErrors (Seq Token)
lexText text = fst $ runState (lexHelper Empty) (initFileState text)

lexHelper :: Seq Token -> State FileState (WithErrors (Seq Token))
lexHelper tokens = do
  fileState <- get
  if
    | (text fileState == Text.empty) -> return $ Success tokens
    | otherwise -> do
        lexResult <- combinedLexer
        case lexResult of
          Error _ -> return lexResult
          Success lexedTokens -> lexHelper (tokens >< lexedTokens)

combinedLexer :: State FileState (WithErrors (Seq Token))
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
    Nothing -> return $ singleError $ LexError position
  where
    unwrap (result, state) = case result of
      Just innerResult -> Just (innerResult, state)
      Nothing -> Nothing

lexers :: [Lexer]
lexers =
  [ lexWhiteSpace,
    -- Multiple character symbols
    lexMultiCharSymbol "->" SingleRightArrowToken,
    lexMultiCharSymbol "=>" FloatRightArrowToken,
    lexMultiCharSymbol "&&" AndToken,
    lexMultiCharSymbol "||" OrToken,
    lexMultiCharSymbol "++" PlusPlusToken,
    lexMultiCharSymbol "==" EqualEqualToken,
    lexMultiCharSymbol "!=" NotEqualToken,
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
    lexSymbol '{' LeftCurlyBraceToken,
    lexSymbol '}' RightCurlyBraceToken,
    lexSymbol '[' LeftSquareBracketToken,
    lexSymbol ']' RightSquareBracketToken,
    lexSymbol '⟨' LeftAngleBracketToken,
    lexSymbol '⟩' RightAngleBracketToken,
    -- Operators
    lexSymbol '+' PlusToken,
    lexSymbol '-' MinusToken,
    lexSymbol '*' StarToken,
    lexSymbol '/' SlashToken,
    lexSymbol '%' PercentToken,
    lexSymbol '!' BangToken,
    lexSymbol '>' GreaterToken,
    lexSymbol '<' LessToken,
    lexSymbol '.' DotToken,
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

lexSymbol :: Char -> (Range -> Token) -> Lexer
lexSymbol char makeToken = do
  start <- getPosition
  consumeResult <- consumeIf (== char)
  case consumeResult of
    Consumed (Just _) -> do
      end <- getPosition
      let token = makeToken $ Range {start, end}
      return $ Just $ Success $ singleton token
    _ -> return Nothing

lexMultiCharSymbol :: String -> (Range -> Token) -> Lexer
lexMultiCharSymbol keyword makeToken = do
  start <- getPosition
  consumeResult <- consumeString keyword nextIsNotIdentifierChar
  case consumeResult of
    Just _ -> do
      end <- getPosition
      let token = makeToken $ Range {start, end}
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
            let floatValue = read $ Text.unpack (intText <> "." <> decimalText')
            let token = FloatLiteralToken (Range {start, end}) floatValue
            return $ Just $ Success $ singleton token
          _ -> do
            end <- getPosition
            let intValue = read $ Text.unpack intText
            let token = IntLiteralToken (Range {start, end}) intValue
            return $ Just $ Success $ singleton token

lexStringLiteral :: Lexer
lexStringLiteral = do
  start <- getPosition
  consumeQuoteResult <- consumeIf (== '"')
  case consumeQuoteResult of
    Consumed (Just _) -> do
      consumeBodyResult <- consumeUntil (== '"')
      case consumeBodyResult of
        HitEOF -> return $ Just $ singleError $ UnterminatedStringError start
        Consumed stringText -> do
          end <- getPosition
          let token = StringLiteralToken (Range {start, end}) stringText
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
        HitEOF -> return $ Just $ singleError $ UnterminatedCharError start
        Consumed stringText -> do
          end <- getPosition
          if
            | Text.length stringText == 1 -> do
                let token = CharLiteralToken (Range {start, end}) (Text.head stringText)
                return $ Just $ Success $ singleton token
            | otherwise -> do
                return $ Just $ singleError $ InvalidCharLiteralError start
    _ -> return Nothing

lexKeywordOrIdentifier :: Lexer
lexKeywordOrIdentifier = do
  start <- getPosition
  matchResult <- matchNext isIdentifierStartChar
  case matchResult of
    Consumed True -> do
      identifierText <- consumeWhile isIdentifierChar
      end <- getPosition
      let tokenConstructor =
            ( case identifierText of
                "type" -> TypeToken
                "let" -> LetToken
                "mut" -> MutToken
                "if" -> IfToken
                "then" -> ThenToken
                "else" -> ElseToken
                "func" -> FuncToken
                "case" -> CaseToken
                "of" -> OfToken
                "print" -> PrintToken
                "while" -> WhileToken
                "loop" -> LoopToken
                "return" -> ReturnToken
                "rec" -> RecToken
                "Int" -> IntToken
                "Float" -> FloatToken
                "Char" -> CharToken
                "String" -> StringToken
                "Bool" -> BoolToken
                "Nil" -> NilToken
                "true" -> \range -> BoolLiteralToken range True
                "false" -> \range -> BoolLiteralToken range False
                "nil" -> NilLiteralToken
                _ -> \range -> IdentifierToken range identifierText
            )
      let token = tokenConstructor Range {start, end}
      return $ Just $ Success $ singleton token
    _ -> return Nothing

isIdentifierStartChar :: Char -> Bool
isIdentifierStartChar = isAlpha

isIdentifierChar :: Char -> Bool
isIdentifierChar char = isAlpha char || isDigit char || char == '_'