{-# LANGUAGE TypeFamilies #-}

module IdentifierBinding.SyntaxTree
  ( IdentifierBindingPhase,
    IBModule,
    IBModuleContent (IBModuleContent),
    IBStatement,
    IBIdentifier,
    IBExpression,
    BoundIdentifier,
    IBMainFunctionDefinition,
    IBFunctionDefinition,
    FunctionIndex,
    IBFunctionExpressionContent (IBFunctionExpressionContent),
  )
where

import Core.FilePositions
import Core.SyntaxTree
import Core.Utils
import Data.Sequence (Seq)

data IdentifierBindingPhase

-- Module
type IBModule = Module IdentifierBindingPhase

type instance ModuleData IdentifierBindingPhase = ()

data IBModuleContent = IBModuleContent IBMainFunctionDefinition (Seq IBFunctionDefinition)

instance Pretty IBModuleContent where
  pretty (IBModuleContent mainFunctionDefinition subFunctionDefinitions) = pretty mainFunctionDefinition ++ "(" ++ pretty subFunctionDefinitions ++ ")"

type instance ModuleContent IdentifierBindingPhase = IBModuleContent

type IBMainFunctionDefinition = MainFunctionDefinition IdentifierBindingPhase

type instance MainFunctionDefinitionData IdentifierBindingPhase = ()

type IBFunctionDefinition = FunctionDefinition IdentifierBindingPhase

type instance FunctionDefinitionData IdentifierBindingPhase = Range

-- Statement
type IBStatement = Statement IdentifierBindingPhase

type instance PrintStatementData IdentifierBindingPhase = Range

type instance VariableDeclarationStatementData IdentifierBindingPhase = Range

type instance VariableMutationStatementData IdentifierBindingPhase = Range

type instance ExpressionStatementData IdentifierBindingPhase = Range

type instance WhileLoopStatementData IdentifierBindingPhase = Range

type instance ReturnStatementData IdentifierBindingPhase = Range

-- Identifier
type IBIdentifier = Identifier IdentifierBindingPhase

type instance IdentifierData IdentifierBindingPhase = Range

type BoundIdentifier = Int

type instance IdentifierContent IdentifierBindingPhase = BoundIdentifier

-- Expression
type IBExpression = Expression IdentifierBindingPhase

type instance IntLiteralExpressionData IdentifierBindingPhase = Range

type instance DoubleLiteralExpressionData IdentifierBindingPhase = Range

type instance CharLiteralExpressionData IdentifierBindingPhase = Range

type instance StringLiteralExpressionData IdentifierBindingPhase = Range

type instance BoolLiteralExpressionData IdentifierBindingPhase = Range

type instance NilExpressionData IdentifierBindingPhase = Range

type instance VariableExpressionData IdentifierBindingPhase = Range

type instance NegateExpressionData IdentifierBindingPhase = Range

type instance AddExpressionData IdentifierBindingPhase = Range

type instance SubtractExpressionData IdentifierBindingPhase = Range

type instance MultiplyExpressionData IdentifierBindingPhase = Range

type instance DivideExpressionData IdentifierBindingPhase = Range

type instance ModuloExpressionData IdentifierBindingPhase = Range

type instance NotExpressionData IdentifierBindingPhase = Range

type instance AndExpressionData IdentifierBindingPhase = Range

type instance OrExpressionData IdentifierBindingPhase = Range

type instance EqualExpressionData IdentifierBindingPhase = Range

type instance NotEqualExpressionData IdentifierBindingPhase = Range

type instance GreaterExpressionData IdentifierBindingPhase = Range

type instance LessExpressionData IdentifierBindingPhase = Range

type instance GreaterEqualExpressionData IdentifierBindingPhase = Range

type instance LessEqualExpressionData IdentifierBindingPhase = Range

type instance IfThenElseExpressionData IdentifierBindingPhase = Range

type instance ScopeExpressionData IdentifierBindingPhase = Range

type instance FunctionExpressionData IdentifierBindingPhase = Range

type FunctionIndex = Int

data IBFunctionExpressionContent = IBFunctionExpressionContent FunctionIndex (Seq IBIdentifier)

instance Pretty IBFunctionExpressionContent where
  pretty (IBFunctionExpressionContent functionIndex capturedIdentifiers) = show functionIndex ++ "(" ++ pretty capturedIdentifiers ++ ")"

type instance FunctionExpressionContent IdentifierBindingPhase = IBFunctionExpressionContent

type instance FunctionCallExpressionData IdentifierBindingPhase = Range

instance WithRange IBExpression where
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
    FunctionExpression range _ -> range
    FunctionCallExpression range _ _ -> range