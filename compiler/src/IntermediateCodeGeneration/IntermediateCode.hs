module IntermediateCodeGeneration.IntermediateCode
  ( Mod (Mod),
    MainFunc (MainFunc),
    SubFunc (SubFunc),
    Stmt (..),
    Expr (..),
    FieldIndex,
    LiteralValue (..),
    BuiltInFn (..),
  )
where

import Data.Sequence (Seq)
import Data.Text (Text)
import IdentifierBinding.SyntaxTree

-- Module
data Mod = Mod MainFunc (Seq SubFunc)
  deriving (Show)

newtype MainFunc = MainFunc (Seq Stmt)
  deriving (Show)

data SubFunc = SubFunc (Seq ValueIdentifierIndex) Expr
  deriving (Show)

-- Statement
data Stmt
  = VariableDeclarationStmt ValueIdentifierIndex Expr
  | VariableMutationStmt ValueIdentifierIndex Expr
  | FieldMutationStmt Expr FieldIndex Expr
  | IndexMutationStmt Expr Expr Expr
  | ExpressionStmt Expr
  | WhileLoopStmt Expr Expr
  | ReturnStmt Expr
  deriving (Show)

-- Expression
data Expr
  = LiteralExpr LiteralValue
  | IdentifierExpr ValueIdentifierIndex
  | BuiltInFunctionExpr BuiltInFn
  | IfThenElseExpr Expr Expr Expr
  | ScopeExpr (Seq Stmt)
  | FunctionExpr FunctionIndex (Seq ValueIdentifierIndex)
  | CallExpr Expr (Seq Expr)
  | RecordExpr RecordIndex (Seq Expr)
  | FieldExpr Expr FieldIndex
  | CaseExpr Expr (Seq (RecordIndex, ValueIdentifierIndex, Expr))
  | ListExpr (Seq Expr)
  | IndexExpr Expr Expr
  deriving (Show)

type FieldIndex = Int

data LiteralValue
  = IntLiteral Int
  | FloatLiteral Double
  | CharLiteral Char
  | StringLiteral Text
  | BoolLiteral Bool
  | NilLiteral
  deriving (Show)

data BuiltInFn
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
  | PrintLineFn
  | ReadLineFn
  | PushFn
  | PopFn
  | LengthFn
  deriving (Show)