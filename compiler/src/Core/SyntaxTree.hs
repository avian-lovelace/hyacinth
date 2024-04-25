{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Core.SyntaxTree
  ( FileScope (FileScope),
    FileScopeData,
    Statement
      ( PrintStatement,
        VariableDeclarationStatement,
        VariableMutationStatement,
        ExpressionStatement,
        WhileLoopStatement
      ),
    PrintStatementData,
    VariableDeclarationStatementData,
    VariableMutationStatementData,
    ExpressionStatementData,
    WhileLoopStatementData,
    VariableName (VariableName),
    VariableNameData,
    Identifier,
    Expression
      ( IntLiteralExpression,
        DoubleLiteralExpression,
        CharLiteralExpression,
        StringLiteralExpression,
        BoolLiteralExpression,
        NilExpression,
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
        VariableExpression,
        IfThenElseExpression,
        ScopeExpression
      ),
    IntLiteralExpressionData,
    DoubleLiteralExpressionData,
    CharLiteralExpressionData,
    StringLiteralExpressionData,
    BoolLiteralExpressionData,
    NilExpressionData,
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
    IfThenElseExpressionData,
    ScopeExpressionData,
  )
where

import Core.Utils
import Data.Sequence (Seq)
import Data.Text (Text)

data FileScope phase = FileScope (FileScopeData phase) (Seq (Statement phase))

type family FileScopeData phase

instance (Show (Identifier a)) => Pretty (FileScope a) where
  pretty (FileScope _ statements) = "(FileScope " ++ pretty statements ++ ")"

data Statement phase
  = PrintStatement (PrintStatementData phase) (Expression phase)
  | VariableDeclarationStatement (VariableDeclarationStatementData phase) (VariableName phase) (Expression phase)
  | VariableMutationStatement (VariableMutationStatementData phase) (VariableName phase) (Expression phase)
  | ExpressionStatement (ExpressionStatementData phase) (Expression phase)
  | WhileLoopStatement (WhileLoopStatementData phase) (Expression phase) (Statement phase)

instance (Show (Identifier a)) => Pretty (Statement a) where
  pretty (PrintStatement _ expression) = "(PrintStatement " ++ pretty expression ++ ")"
  pretty (VariableDeclarationStatement _ variableName value) = "(VariableDeclarationStatement " ++ pretty variableName ++ " " ++ pretty value ++ ")"
  pretty (VariableMutationStatement _ variableName value) = "(VariableMutationStatement " ++ pretty variableName ++ " " ++ pretty value ++ ")"
  pretty (ExpressionStatement _ expression) = "(ExpressionStatement " ++ pretty expression ++ ")"
  pretty (WhileLoopStatement _ condition statement) = "(WhileLoopStatement " ++ pretty condition ++ " " ++ pretty statement ++ ")"

type family PrintStatementData phase

type family VariableDeclarationStatementData phase

type family VariableMutationStatementData phase

type family ExpressionStatementData phase

type family WhileLoopStatementData phase

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

instance (Show (Identifier a)) => Pretty (VariableName a) where
  pretty (VariableName _ name) = "(VariableName " ++ show name ++ ")"

-- data TypeVariableName = TypeVariableName Identifier

-- data ProductTypeName = ProductTypeName Identifier

-- data Scope = Scope [Statement] Expression

data Expression phase
  = IntLiteralExpression (IntLiteralExpressionData phase) Int
  | DoubleLiteralExpression (DoubleLiteralExpressionData phase) Double
  | CharLiteralExpression (CharLiteralExpressionData phase) Char
  | StringLiteralExpression (StringLiteralExpressionData phase) Text
  | BoolLiteralExpression (BoolLiteralExpressionData phase) Bool
  | NilExpression (NilExpressionData phase)
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
  | IfThenElseExpression (IfThenElseExpressionData phase) (Expression phase) (Expression phase) (Maybe (Expression phase))
  | ScopeExpression (ScopeExpressionData phase) (Seq (Statement phase))

instance (Show (Identifier a)) => Pretty (Expression a) where
  pretty (IntLiteralExpression _ value) = "(IntLiteralExpression " ++ show value ++ ")"
  pretty (DoubleLiteralExpression _ value) = "(DoubleLiteralExpression " ++ show value ++ ")"
  pretty (CharLiteralExpression _ value) = "(CharLiteralExpression " ++ show value ++ ")"
  pretty (StringLiteralExpression _ value) = "(StringLiteralExpression " ++ show value ++ ")"
  pretty (BoolLiteralExpression _ value) = "(BoolLiteralExpression " ++ show value ++ ")"
  pretty (NilExpression _) = "(NilExpression)"
  pretty (VariableExpression _ variableName) = "(VariableExpression " ++ pretty variableName ++ ")"
  pretty (NegateExpression _ inner) = "(NegateExpression " ++ pretty inner ++ ")"
  pretty (AddExpression _ left right) = "(AddExpression " ++ pretty left ++ " " ++ pretty right ++ ")"
  pretty (SubtractExpression _ left right) = "(SubtractExpression " ++ pretty left ++ " " ++ pretty right ++ ")"
  pretty (MultiplyExpression _ left right) = "(MultiplyExpression " ++ pretty left ++ " " ++ pretty right ++ ")"
  pretty (DivideExpression _ left right) = "(DivideExpression " ++ pretty left ++ " " ++ pretty right ++ ")"
  pretty (ModuloExpression _ left right) = "(ModuloExpression " ++ pretty left ++ " " ++ pretty right ++ ")"
  pretty (NotExpression _ inner) = "(NotExpression " ++ pretty inner ++ ")"
  pretty (AndExpression _ left right) = "(AndExpression " ++ pretty left ++ " " ++ pretty right ++ ")"
  pretty (OrExpression _ left right) = "(OrExpression " ++ pretty left ++ " " ++ pretty right ++ ")"
  pretty (EqualExpression _ left right) = "(EqualExpression " ++ pretty left ++ " " ++ pretty right ++ ")"
  pretty (NotEqualExpression _ left right) = "(NotEqualExpression " ++ pretty left ++ " " ++ pretty right ++ ")"
  pretty (GreaterExpression _ left right) = "(GreaterExpression " ++ pretty left ++ " " ++ pretty right ++ ")"
  pretty (LessExpression _ left right) = "(LessExpression " ++ pretty left ++ " " ++ pretty right ++ ")"
  pretty (GreaterEqualExpression _ left right) = "(GreaterEqualExpression " ++ pretty left ++ " " ++ pretty right ++ ")"
  pretty (LessEqualExpression _ left right) = "(LessEqualExpression " ++ pretty left ++ " " ++ pretty right ++ ")"
  pretty (IfThenElseExpression _ condition trueExpression Nothing) =
    "(IfThenElseExpression " ++ pretty condition ++ " " ++ pretty trueExpression ++ ")"
  pretty (IfThenElseExpression _ condition trueExpression (Just falseExpression)) =
    "(IfThenElseExpression " ++ pretty condition ++ " " ++ pretty trueExpression ++ " " ++ pretty falseExpression ++ ")"
  pretty (ScopeExpression _ statements) = "(VariableExpression " ++ pretty statements ++ ")"

type family IntLiteralExpressionData phase

type family DoubleLiteralExpressionData phase

type family CharLiteralExpressionData phase

type family StringLiteralExpressionData phase

type family BoolLiteralExpressionData phase

type family NilExpressionData phase

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

type family IfThenElseExpressionData phase

type family ScopeExpressionData phase