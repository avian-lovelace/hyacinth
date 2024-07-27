module Lexing.Tokens
  ( Token
      ( TypeToken,
        LetToken,
        MutToken,
        IfToken,
        ThenToken,
        ElseToken,
        FuncToken,
        CaseToken,
        OfToken,
        WhileToken,
        LoopToken,
        ReturnToken,
        RecToken,
        ListToken,
        SemicolonToken,
        ColonToken,
        EqualsToken,
        PipeToken,
        SingleRightArrowToken,
        DoubleRightArrowToken,
        CommaToken,
        LeftParenToken,
        RightParenToken,
        LeftCurlyBraceToken,
        RightCurlyBraceToken,
        LeftSquareBracketToken,
        RightSquareBracketToken,
        LeftAngleBracketToken,
        RightAngleBracketToken,
        IdentifierToken,
        IntToken,
        FloatToken,
        CharToken,
        StringToken,
        BoolToken,
        NilToken,
        IntLiteralToken,
        FloatLiteralToken,
        CharLiteralToken,
        StringLiteralToken,
        BoolLiteralToken,
        NilLiteralToken,
        PlusToken,
        MinusToken,
        StarToken,
        SlashToken,
        PercentToken,
        BangToken,
        AndToken,
        OrToken,
        PlusPlusToken,
        EqualEqualToken,
        NotEqualToken,
        GreaterToken,
        LessToken,
        GreaterEqualToken,
        LessEqualToken,
        DotToken,
        HashToken
      ),
  )
where

import Core.FilePositions
import Core.Utils
import Data.Text (Text)

data Token
  = -- Keywords
    TypeToken Range
  | LetToken Range
  | MutToken Range
  | IfToken Range
  | ThenToken Range
  | ElseToken Range
  | FuncToken Range
  | CaseToken Range
  | OfToken Range
  | WhileToken Range
  | LoopToken Range
  | ReturnToken Range
  | RecToken Range
  | ListToken Range
  | -- Separators
    SemicolonToken Range
  | ColonToken Range
  | EqualsToken Range
  | PipeToken Range
  | SingleRightArrowToken Range
  | DoubleRightArrowToken Range
  | CommaToken Range
  | -- Grouping
    LeftParenToken Range
  | RightParenToken Range
  | LeftCurlyBraceToken Range
  | RightCurlyBraceToken Range
  | LeftSquareBracketToken Range
  | RightSquareBracketToken Range
  | LeftAngleBracketToken Range
  | RightAngleBracketToken Range
  | -- Identifiers
    IdentifierToken Range Text
  | -- Primitive types
    IntToken Range
  | FloatToken Range
  | CharToken Range
  | StringToken Range
  | BoolToken Range
  | NilToken Range
  | -- Literals
    IntLiteralToken Range Int
  | FloatLiteralToken Range Double
  | CharLiteralToken Range Char
  | StringLiteralToken Range Text
  | BoolLiteralToken Range Bool
  | NilLiteralToken Range
  | -- Operators
    PlusToken Range
  | MinusToken Range
  | StarToken Range
  | SlashToken Range
  | PercentToken Range
  | BangToken Range
  | AndToken Range
  | OrToken Range
  | PlusPlusToken Range
  | EqualEqualToken Range
  | NotEqualToken Range
  | GreaterToken Range
  | LessToken Range
  | GreaterEqualToken Range
  | LessEqualToken Range
  | DotToken Range
  | HashToken Range
  deriving (Show, Eq)

instance Pretty Token where
  pretty (TypeToken _) = "type"
  pretty (LetToken _) = "let"
  pretty (MutToken _) = "mut"
  pretty (IfToken _) = "if"
  pretty (ThenToken _) = "then"
  pretty (ElseToken _) = "else"
  pretty (FuncToken _) = "fn"
  pretty (CaseToken _) = "case"
  pretty (OfToken _) = "of"
  pretty (WhileToken _) = "while"
  pretty (LoopToken _) = "loop"
  pretty (ReturnToken _) = "return"
  pretty (RecToken _) = "rec"
  pretty (ListToken _) = "List"
  pretty (SemicolonToken _) = ";"
  pretty (ColonToken _) = ":"
  pretty (EqualsToken _) = "="
  pretty (PipeToken _) = "|"
  pretty (SingleRightArrowToken _) = "->"
  pretty (DoubleRightArrowToken _) = "=>"
  pretty (CommaToken _) = ","
  pretty (LeftParenToken _) = "("
  pretty (RightParenToken _) = ")"
  pretty (LeftCurlyBraceToken _) = "{"
  pretty (RightCurlyBraceToken _) = "}"
  pretty (LeftSquareBracketToken _) = "["
  pretty (RightSquareBracketToken _) = "]"
  pretty (LeftAngleBracketToken _) = "⟨"
  pretty (RightAngleBracketToken _) = "⟩"
  pretty (IdentifierToken _ identifier) = "identifier:" ++ show identifier
  pretty (IntToken _) = "Int"
  pretty (FloatToken _) = "Float"
  pretty (CharToken _) = "Char"
  pretty (StringToken _) = "String"
  pretty (BoolToken _) = "Bool"
  pretty (NilToken _) = "Nil"
  pretty (IntLiteralToken _ value) = "intLiteral:" ++ show value
  pretty (FloatLiteralToken _ value) = "floatLiteral:" ++ show value
  pretty (CharLiteralToken _ value) = "charLiteral:" ++ show value
  pretty (StringLiteralToken _ value) = "stringLiteral:" ++ show value
  pretty (BoolLiteralToken _ value) = "boolLiteral:" ++ show value
  pretty (NilLiteralToken _) = "nil"
  pretty (PlusToken _) = "+"
  pretty (MinusToken _) = "-"
  pretty (StarToken _) = "*"
  pretty (SlashToken _) = "/"
  pretty (PercentToken _) = "%"
  pretty (BangToken _) = "!"
  pretty (AndToken _) = "&&"
  pretty (OrToken _) = "||"
  pretty (PlusPlusToken _) = "++"
  pretty (EqualEqualToken _) = "=="
  pretty (NotEqualToken _) = "!="
  pretty (GreaterToken _) = ">"
  pretty (LessToken _) = "<"
  pretty (GreaterEqualToken _) = ">="
  pretty (LessEqualToken _) = "<="
  pretty (DotToken _) = "."
  pretty (HashToken _) = "#"

instance WithRange Token where
  getRange (TypeToken range) = range
  getRange (LetToken range) = range
  getRange (MutToken range) = range
  getRange (IfToken range) = range
  getRange (ThenToken range) = range
  getRange (ElseToken range) = range
  getRange (FuncToken range) = range
  getRange (CaseToken range) = range
  getRange (OfToken range) = range
  getRange (WhileToken range) = range
  getRange (LoopToken range) = range
  getRange (ReturnToken range) = range
  getRange (RecToken range) = range
  getRange (ListToken range) = range
  getRange (SemicolonToken range) = range
  getRange (ColonToken range) = range
  getRange (EqualsToken range) = range
  getRange (PipeToken range) = range
  getRange (SingleRightArrowToken range) = range
  getRange (DoubleRightArrowToken range) = range
  getRange (CommaToken range) = range
  getRange (LeftParenToken range) = range
  getRange (RightParenToken range) = range
  getRange (LeftCurlyBraceToken range) = range
  getRange (RightCurlyBraceToken range) = range
  getRange (LeftSquareBracketToken range) = range
  getRange (RightSquareBracketToken range) = range
  getRange (LeftAngleBracketToken range) = range
  getRange (RightAngleBracketToken range) = range
  getRange (IdentifierToken range _) = range
  getRange (IntToken range) = range
  getRange (FloatToken range) = range
  getRange (CharToken range) = range
  getRange (StringToken range) = range
  getRange (BoolToken range) = range
  getRange (NilToken range) = range
  getRange (IntLiteralToken range _) = range
  getRange (FloatLiteralToken range _) = range
  getRange (CharLiteralToken range _) = range
  getRange (StringLiteralToken range _) = range
  getRange (BoolLiteralToken range _) = range
  getRange (NilLiteralToken range) = range
  getRange (PlusToken range) = range
  getRange (MinusToken range) = range
  getRange (StarToken range) = range
  getRange (SlashToken range) = range
  getRange (PercentToken range) = range
  getRange (BangToken range) = range
  getRange (AndToken range) = range
  getRange (OrToken range) = range
  getRange (PlusPlusToken range) = range
  getRange (EqualEqualToken range) = range
  getRange (NotEqualToken range) = range
  getRange (GreaterToken range) = range
  getRange (LessToken range) = range
  getRange (GreaterEqualToken range) = range
  getRange (LessEqualToken range) = range
  getRange (DotToken range) = range
  getRange (HashToken range) = range