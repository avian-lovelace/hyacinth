module IntermediateCodeGeneration.IntermediateCode
  ( Mod (Mod),
    MainFunc (MainFunc),
    SubFunc (SubFunc),
    Stmt
      ( VariableDeclarationStmt,
        VariableMutationStmt,
        ExpressionStmt,
        WhileLoopStmt,
        ReturnStmt
      ),
    Expr
      ( LiteralExpr,
        IdentifierExpr,
        BuiltInFunctionExpr,
        IfThenElseExpr,
        ScopeExpr,
        FunctionExpr,
        CallExpr,
        RecordExpr,
        FieldExpr
      ),
    FieldIndex,
    LiteralValue
      ( IntLiteral,
        FloatLiteral,
        CharLiteral,
        StringLiteral,
        BoolLiteral,
        NilLiteral
      ),
    BuiltInFunction
      ( NegateFn,
        AddFn,
        SubtractFn,
        MultiplyFn,
        DivideFn,
        ModuloFn,
        NotFn,
        EqualFn,
        NotEqualFn,
        GreaterFn,
        LessFn,
        GreaterEqualFn,
        LessEqualFn,
        PrintFn
      ),
  )
where

import Core.SyntaxTree
import Data.Sequence (Seq)
import Data.Text (Text)
import IdentifierBinding.SyntaxTree

-- Module
data Mod = Mod MainFunc (Seq SubFunc)

data MainFunc = MainFunc (Seq Stmt)

data SubFunc = SubFunc (Seq ValueIdentifierIndex) Expr

-- Statement
data Stmt
  = VariableDeclarationStmt ValueIdentifierIndex Expr
  | VariableMutationStmt ValueIdentifierIndex Expr
  | ExpressionStmt Expr
  | WhileLoopStmt Expr Expr
  | ReturnStmt Expr

-- Expression
data Expr
  = LiteralExpr LiteralValue
  | IdentifierExpr ValueIdentifierIndex
  | BuiltInFunctionExpr BuiltInFunction (Seq Expr)
  | IfThenElseExpr Expr Expr Expr
  | ScopeExpr (Seq Stmt)
  | FunctionExpr FunctionIndex (Seq ValueIdentifierIndex)
  | CallExpr Expr (Seq Expr)
  | RecordExpr RecordIndex (Seq Expr)
  | FieldExpr Expr FieldIndex

type FieldIndex = Int

data LiteralValue
  = IntLiteral Int
  | FloatLiteral Double
  | CharLiteral Char
  | StringLiteral Text
  | BoolLiteral Bool
  | NilLiteral

data BuiltInFunction
  = NegateFn
  | AddFn
  | SubtractFn
  | MultiplyFn
  | DivideFn
  | ModuloFn
  | NotFn
  | EqualFn
  | NotEqualFn
  | GreaterFn
  | LessFn
  | GreaterEqualFn
  | LessEqualFn
  | PrintFn