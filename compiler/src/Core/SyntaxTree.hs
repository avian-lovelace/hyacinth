{-# LANGUAGE TypeFamilies #-}

module Core.SyntaxTree
  ( FileScope (FileScope),
    FileScopeData,
    Statement (PrintStatement, VariableDeclarationStatement, VariableMutationStatement),
    PrintStatementData,
    VariableDeclarationStatementData,
    VariableMutationStatementData,
    VariableName (VariableName),
    VariableNameData,
    Identifier,
    Expression
      ( IntLiteralExpression,
        DoubleLiteralExpression,
        CharLiteralExpression,
        StringLiteralExpression,
        BoolLiteralExpression,
        NegateExpression,
        AddExpression,
        SubtractExpression,
        MultiplyExpression,
        DivideExpression,
        ModuloExpression,
        NotExpression,
        AndExpression,
        OrExpression,
        EqualExpression,
        NotEqualExpression,
        GreaterExpression,
        LessExpression,
        GreaterEqualExpression,
        LessEqualExpression,
        VariableExpression
      ),
    IntLiteralExpressionData,
    DoubleLiteralExpressionData,
    CharLiteralExpressionData,
    StringLiteralExpressionData,
    BoolLiteralExpressionData,
    NegateExpressionData,
    AddExpressionData,
    SubtractExpressionData,
    MultiplyExpressionData,
    DivideExpressionData,
    ModuloExpressionData,
    NotExpressionData,
    AndExpressionData,
    OrExpressionData,
    EqualExpressionData,
    NotEqualExpressionData,
    GreaterExpressionData,
    LessExpressionData,
    GreaterEqualExpressionData,
    LessEqualExpressionData,
    VariableExpressionData,
  )
where

import Data.Sequence (Seq)
import Data.Text (Text)

data FileScope phase = FileScope (FileScopeData phase) (Seq (Statement phase))

type family FileScopeData phase

data Statement phase
  = PrintStatement (PrintStatementData phase) (Expression phase)
  | VariableDeclarationStatement (VariableDeclarationStatementData phase) (VariableName phase) (Expression phase)
  | VariableMutationStatement (VariableMutationStatementData phase) (VariableName phase) (Expression phase)

type family PrintStatementData phase

type family VariableDeclarationStatementData phase

type family VariableMutationStatementData phase

--     AlgebraicDataTypeStatement TypeVariableName [TypeVariableName] [(ProductTypeName, [ProperType])]
--   | TypeAssignmentStatement TypeVariableName Type
--   | ValueAssignmentStatement VariableName (Maybe ProperType) Expression

-- data ProperType =
--     IntType
--   | DoubleType
--   | CharType
--   | StringType
--   | BoolType
--   | FunctionType Type Type
--   | VariableType TypeVariableName
--   | TypeApplication Type Type

-- data Type =
--     ProperType ProperType
--   | GenericType TypeVariableName Type

data VariableName phase = VariableName (VariableNameData phase) (Identifier phase)

type family VariableNameData phase

type family Identifier phase

-- data TypeVariableName = TypeVariableName Identifier

-- data ProductTypeName = ProductTypeName Identifier

-- data Scope = Scope [Statement] Expression

data Expression phase
  = IntLiteralExpression (IntLiteralExpressionData phase) Int
  | DoubleLiteralExpression (DoubleLiteralExpressionData phase) Double
  | CharLiteralExpression (CharLiteralExpressionData phase) Char
  | StringLiteralExpression (StringLiteralExpressionData phase) Text
  | BoolLiteralExpression (BoolLiteralExpressionData phase) Bool
  | VariableExpression (VariableExpressionData phase) (VariableName phase)
  | NegateExpression (NegateExpressionData phase) (Expression phase)
  | AddExpression (AddExpressionData phase) (Expression phase) (Expression phase)
  | SubtractExpression (SubtractExpressionData phase) (Expression phase) (Expression phase)
  | MultiplyExpression (MultiplyExpressionData phase) (Expression phase) (Expression phase)
  | DivideExpression (DivideExpressionData phase) (Expression phase) (Expression phase)
  | ModuloExpression (ModuloExpressionData phase) (Expression phase) (Expression phase)
  | NotExpression (NotExpressionData phase) (Expression phase)
  | AndExpression (AndExpressionData phase) (Expression phase) (Expression phase)
  | OrExpression (OrExpressionData phase) (Expression phase) (Expression phase)
  | EqualExpression (EqualExpressionData phase) (Expression phase) (Expression phase)
  | NotEqualExpression (NotEqualExpressionData phase) (Expression phase) (Expression phase)
  | GreaterExpression (GreaterExpressionData phase) (Expression phase) (Expression phase)
  | LessExpression (LessExpressionData phase) (Expression phase) (Expression phase)
  | GreaterEqualExpression (GreaterEqualExpressionData phase) (Expression phase) (Expression phase)
  | LessEqualExpression (LessEqualExpressionData phase) (Expression phase) (Expression phase)

type family IntLiteralExpressionData phase

type family DoubleLiteralExpressionData phase

type family CharLiteralExpressionData phase

type family StringLiteralExpressionData phase

type family BoolLiteralExpressionData phase

type family VariableExpressionData phase

type family NegateExpressionData phase

type family AddExpressionData phase

type family SubtractExpressionData phase

type family MultiplyExpressionData phase

type family DivideExpressionData phase

type family ModuloExpressionData phase

type family NotExpressionData phase

type family AndExpressionData phase

type family OrExpressionData phase

type family EqualExpressionData phase

type family NotEqualExpressionData phase

type family GreaterExpressionData phase

type family LessExpressionData phase

type family GreaterEqualExpressionData phase

type family LessEqualExpressionData phase
