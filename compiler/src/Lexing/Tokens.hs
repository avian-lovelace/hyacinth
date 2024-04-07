module Lexing.Tokens
  ( Token
      ( Token,
        value,
        range
      ),
    TokenValue
      ( TypeToken,
        LetToken,
        IfToken,
        ElseToken,
        FnToken,
        MatchToken,
        OfToken,
        PrintToken,
        SemicolonToken,
        ColonToken,
        EqualsToken,
        PipeToken,
        SingleRightArrowToken,
        DoubleRightArrowToken,
        CommaToken,
        LeftParenToken,
        RightParenToken,
        LeftBraceToken,
        RightBraceToken,
        IdentifierToken,
        IntToken,
        DoubleToken,
        CharToken,
        StringToken,
        BoolToken,
        IntLiteralToken,
        DoubleLiteralToken,
        CharLiteralToken,
        StringLiteralToken,
        BoolLiteralToken,
        PlusToken,
        MinusToken,
        StarToken,
        SlashToken,
        BangToken,
        AndToken,
        OrToken,
        PlusPlusToken,
        GreaterToken,
        LessToken,
        GreaterEqualToken,
        LessEqualToken
      ),
  )
where

import Core.Utils
import qualified Data.Text as Text

data Token = Token {value :: TokenValue, range :: Range}
  deriving (Show, Eq)

instance Pretty Token where
  pretty Token {value, range} = "Token{ " ++ pretty value ++ " " ++ pretty range ++ " }"

data TokenValue
  = -- Keywords
    TypeToken
  | LetToken
  | IfToken
  | ElseToken
  | FnToken
  | MatchToken
  | OfToken
  | PrintToken
  | -- Separators
    SemicolonToken
  | ColonToken
  | EqualsToken
  | PipeToken
  | SingleRightArrowToken
  | DoubleRightArrowToken
  | CommaToken
  | -- Grouping
    LeftParenToken
  | RightParenToken
  | LeftBraceToken
  | RightBraceToken
  | -- Identifiers
    IdentifierToken Identifier
  | -- Primitive types
    IntToken
  | DoubleToken
  | CharToken
  | StringToken
  | BoolToken
  | -- Literals
    IntLiteralToken Int
  | DoubleLiteralToken Double
  | CharLiteralToken Char
  | StringLiteralToken Text.Text
  | BoolLiteralToken Bool
  | -- Operators
    PlusToken
  | MinusToken
  | StarToken
  | SlashToken
  | BangToken
  | AndToken
  | OrToken
  | PlusPlusToken
  | GreaterToken
  | LessToken
  | GreaterEqualToken
  | LessEqualToken
  deriving (Show, Eq)

instance Pretty TokenValue where
  pretty TypeToken = "type"
  pretty LetToken = "let"
  pretty IfToken = "if"
  pretty ElseToken = "else"
  pretty FnToken = "fn"
  pretty MatchToken = "match"
  pretty OfToken = "of"
  pretty PrintToken = "print"
  pretty SemicolonToken = ";"
  pretty ColonToken = ":"
  pretty EqualsToken = "="
  pretty PipeToken = "|"
  pretty SingleRightArrowToken = "->"
  pretty DoubleRightArrowToken = "=>"
  pretty CommaToken = ","
  pretty LeftParenToken = "("
  pretty RightParenToken = ")"
  pretty LeftBraceToken = "{"
  pretty RightBraceToken = "}"
  pretty (IdentifierToken identifier) = "identifier:" ++ show identifier
  pretty IntToken = "Int"
  pretty DoubleToken = "Double"
  pretty CharToken = "Char"
  pretty StringToken = "String"
  pretty BoolToken = "Bool"
  pretty (IntLiteralToken value) = "intLiteral:" ++ show value
  pretty (DoubleLiteralToken value) = "doubleLiteral:" ++ show value
  pretty (CharLiteralToken value) = "charLiteral:" ++ show value
  pretty (StringLiteralToken value) = "stringLiteral:" ++ show value
  pretty (BoolLiteralToken value) = "boolLiteral:" ++ show value
  pretty PlusToken = "+"
  pretty MinusToken = "-"
  pretty StarToken = "*"
  pretty SlashToken = "/"
  pretty BangToken = "!"
  pretty AndToken = "&&"
  pretty OrToken = "||"
  pretty PlusPlusToken = "++"
  pretty GreaterToken = ">"
  pretty LessToken = "<"
  pretty GreaterEqualToken = ">="
  pretty LessEqualToken = "<="