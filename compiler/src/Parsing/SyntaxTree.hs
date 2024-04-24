{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Parsing.SyntaxTree
  ( ParsingPhase,
    PFileScope,
    PStatement,
    PVariableName,
    PExpression,
    UnboundIdentifier,
  )
where

import Core.FilePositions
import Core.SyntaxTree
import Data.Text (Text)

data ParsingPhase

-- File Scope
type PFileScope = FileScope ParsingPhase

type instance FileScopeData ParsingPhase = ()

-- Statement
type PStatement = Statement ParsingPhase

type instance PrintStatementData ParsingPhase = Range

type instance VariableDeclarationStatementData ParsingPhase = Range

type instance VariableMutationStatementData ParsingPhase = Range

type instance ExpressionStatementData ParsingPhase = Range

-- Variable Name
type PVariableName = VariableName ParsingPhase

type instance VariableNameData ParsingPhase = Range

type UnboundIdentifier = Text

type instance Identifier ParsingPhase = UnboundIdentifier

-- Expression
type PExpression = Expression ParsingPhase

type instance IntLiteralExpressionData ParsingPhase = Range

type instance DoubleLiteralExpressionData ParsingPhase = Range

type instance CharLiteralExpressionData ParsingPhase = Range

type instance StringLiteralExpressionData ParsingPhase = Range

type instance BoolLiteralExpressionData ParsingPhase = Range

type instance NilExpressionData ParsingPhase = Range

type instance VariableExpressionData ParsingPhase = Range

type instance NegateExpressionData ParsingPhase = Range

type instance AddExpressionData ParsingPhase = Range

type instance SubtractExpressionData ParsingPhase = Range

type instance MultiplyExpressionData ParsingPhase = Range

type instance DivideExpressionData ParsingPhase = Range

type instance ModuloExpressionData ParsingPhase = Range

type instance NotExpressionData ParsingPhase = Range

type instance AndExpressionData ParsingPhase = Range

type instance OrExpressionData ParsingPhase = Range

type instance EqualExpressionData ParsingPhase = Range

type instance NotEqualExpressionData ParsingPhase = Range

type instance GreaterExpressionData ParsingPhase = Range

type instance LessExpressionData ParsingPhase = Range

type instance GreaterEqualExpressionData ParsingPhase = Range

type instance LessEqualExpressionData ParsingPhase = Range

type instance IfThenElseExpressionData ParsingPhase = Range

type instance ScopeExpressionData ParsingPhase = Range

instance WithRange PExpression where
  getRange expression = case expression of
    IntLiteralExpression range _ -> range
    DoubleLiteralExpression range _ -> range
    CharLiteralExpression range _ -> range
    StringLiteralExpression range _ -> range
    BoolLiteralExpression range _ -> range
    NilExpression range -> range
    VariableExpression range _ -> range
    NegateExpression range _ -> range
    AddExpression range _ _ -> range
    SubtractExpression range _ _ -> range
    MultiplyExpression range _ _ -> range
    DivideExpression range _ _ -> range
    ModuloExpression range _ _ -> range
    NotExpression range _ -> range
    AndExpression range _ _ -> range
    OrExpression range _ _ -> range
    EqualExpression range _ _ -> range
    NotEqualExpression range _ _ -> range
    GreaterExpression range _ _ -> range
    LessExpression range _ _ -> range
    GreaterEqualExpression range _ _ -> range
    LessEqualExpression range _ _ -> range
    IfThenElseExpression range _ _ _ -> range
    ScopeExpression range _ -> range