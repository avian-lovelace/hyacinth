module Parsing.SyntaxTree
  ( FileScope (FileScope),
    Statement (PrintStatement),
    -- Statement(AlgebraicDataTypeStatement, TypeAssignmentStatement, ValueAssignmentStatement),
    -- ProperType(IntType, DoubleType, CharType, StringType, BoolType, FunctionType, VariableType, TypeApplication),
    -- Type(ProperType, GenericType),
    -- VariableName(VariableName),
    -- TypeVariableName(TypeVariableName),
    -- Scope(Scope),
    Expression
      ( IntLiteralExpression,
        -- DoubleLiteralExpression,
        -- CharLiteralExpression,
        -- StringLiteralExpression,
        -- BoolLiteralExpression,
        NegateExpression,
        AddExpression,
        SubtractExpression,
        MultiplyExpression,
        DivideExpression,
        -- NotExpression,
        -- AndExpression,
        -- OrExpression,
        -- ConcatExpression,
        -- GreaterExpression,
        -- LessExpression,
        -- GreaterEqualExpression,
        -- LessEqualExpression,
        -- IfExpression,
        -- FunctionExpression,
        -- ApplicationExpression,
        -- MatchExpression,
        ParenthesesExpression
        -- ScopeExpression
      ),
  )
where

import Core.Utils

data FileScope = FileScope [Statement]
  deriving (Show)

data Statement
  = PrintStatement Range Expression
  deriving (Show)

--     AlgebraicDataTypeStatement TypeVariableName [TypeVariableName] [(ProductTypeName, [ProperType])]
--   | TypeAssignmentStatement TypeVariableName Type
--   | ValueAssignmentStatement VariableName (Maybe ProperType) Expression

instance WithRange Statement where
  getRange (PrintStatement range _) = range

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

-- data VariableName = VariableName Identifier

-- data TypeVariableName = TypeVariableName Identifier

-- data ProductTypeName = ProductTypeName Identifier

-- data Scope = Scope [Statement] Expression

data Expression
  = IntLiteralExpression Range Int
  | -- | DoubleLiteralExpression Double
    -- | CharLiteralExpression Char
    -- | StringLiteralExpression String
    -- | BoolLiteralExpression Bool
    NegateExpression Range Expression
  | AddExpression Range Expression Expression
  | SubtractExpression Range Expression Expression
  | MultiplyExpression Range Expression Expression
  | DivideExpression Range Expression Expression
  | -- | NotExpression
    -- | AndExpression
    -- | OrExpression
    -- | ConcatExpression
    -- | GreaterExpression
    -- | LessExpression
    -- | GreaterEqualExpression
    -- | LessEqualExpression
    -- | IfExpression [(Expression, Expression)] Expression
    -- | FunctionExpression [VariableName] Expression
    -- | ApplicationExpression Expression Expression
    -- | MatchExpression Expression [(ProductTypeName, [VariableName], Expression)]
    ParenthesesExpression Range Expression
  deriving
    ( -- | ScopeExpression Scope
      -- | VariableExpression VariableName
      Show
    )

instance WithRange Expression where
  getRange expression = case expression of
    IntLiteralExpression range _ -> range
    NegateExpression range _ -> range
    AddExpression range _ _ -> range
    SubtractExpression range _ _ -> range
    MultiplyExpression range _ _ -> range
    DivideExpression range _ _ -> range
    ParenthesesExpression range _ -> range