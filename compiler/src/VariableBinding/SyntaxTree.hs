{-# LANGUAGE TypeFamilies #-}

module VariableBinding.SyntaxTree
  ( VariableBindingPhase,
    VBFileScope,
    VBStatement,
    VBVariableName,
    VBExpression,
    BoundIdentifier,
  )
where

import Core.FilePositions
import Core.SyntaxTree

data VariableBindingPhase

-- File Scope
type VBFileScope = FileScope VariableBindingPhase

type instance FileScopeData VariableBindingPhase = ()

-- Statement
type VBStatement = Statement VariableBindingPhase

type instance PrintStatementData VariableBindingPhase = Range

type instance VariableDeclarationStatementData VariableBindingPhase = Range

type instance VariableMutationStatementData VariableBindingPhase = Range

-- Variable Name
type VBVariableName = VariableName VariableBindingPhase

type instance VariableNameData VariableBindingPhase = Range

type BoundIdentifier = Int

type instance Identifier VariableBindingPhase = BoundIdentifier

-- Expression
type VBExpression = Expression VariableBindingPhase

type instance IntLiteralExpressionData VariableBindingPhase = Range

type instance DoubleLiteralExpressionData VariableBindingPhase = Range

type instance BoolLiteralExpressionData VariableBindingPhase = Range

type instance VariableExpressionData VariableBindingPhase = Range

type instance NegateExpressionData VariableBindingPhase = Range

type instance AddExpressionData VariableBindingPhase = Range

type instance SubtractExpressionData VariableBindingPhase = Range

type instance MultiplyExpressionData VariableBindingPhase = Range

type instance DivideExpressionData VariableBindingPhase = Range

type instance ModuloExpressionData VariableBindingPhase = Range

type instance NotExpressionData VariableBindingPhase = Range

type instance AndExpressionData VariableBindingPhase = Range

type instance OrExpressionData VariableBindingPhase = Range

type instance EqualExpressionData VariableBindingPhase = Range

type instance NotEqualExpressionData VariableBindingPhase = Range

type instance GreaterExpressionData VariableBindingPhase = Range

type instance LessExpressionData VariableBindingPhase = Range

type instance GreaterEqualExpressionData VariableBindingPhase = Range

type instance LessEqualExpressionData VariableBindingPhase = Range

instance WithRange VBExpression where
  getRange expression = case expression of
    IntLiteralExpression range _ -> range
    DoubleLiteralExpression range _ -> range
    BoolLiteralExpression range _ -> range
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