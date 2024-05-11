{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module IdentifierBinding.SyntaxTree
  ( IdentifierBindingPhase,
    IBModule,
    IBStatement,
    IBIdentifier,
    IBValueIdentifier,
    IBFunctionIdentifier,
    BoundValueIdentifier (BoundValueIdentifier),
    BoundFunctionIdentifier (BoundFunctionIdentifier),
    IBExpression,
    IBMainFunction,
    IBSubFunction,
    IBTypeExpression,
    IBWithTypeAnnotation,
    IBScope,
    IBNonPositionalStatement,
    IBFunctionDefinition,
    IBFunctionDefinitionData (IBFunctionDefinitionData, ibFunctionDefinitionCapturedIdentifiers, ibFunctionDefinitionRange),
  )
where

import Core.FilePositions
import Core.SyntaxTree
import Core.Utils
import Data.Set (Set)
import Parsing.SyntaxTree

data IdentifierBindingPhase

-- Module
type IBModule = Module IdentifierBindingPhase

type instance ModuleData IdentifierBindingPhase = ()

-- instance Pretty IBModuleContent where
--   pretty (IBModuleContent mainFunction subFunctions) = pretty mainFunction ++ "(" ++ pretty subFunctions ++ ")"

type instance ModuleContent IdentifierBindingPhase = IBMainFunction

type IBMainFunction = MainFunction IdentifierBindingPhase

type instance MainFunctionData IdentifierBindingPhase = ()

type IBSubFunction = SubFunction IdentifierBindingPhase

type instance SubFunctionData IdentifierBindingPhase = Range

-- Scope
type IBScope = Scope IdentifierBindingPhase

type instance ScopeData IdentifierBindingPhase = ()

-- Statement
type IBStatement = Statement IdentifierBindingPhase

type instance StatementData IdentifierBindingPhase = Range

type IBFunctionDefinition = FunctionDefinition IdentifierBindingPhase

type instance FunctionDefinitionData IdentifierBindingPhase = IBFunctionDefinitionData

data IBFunctionDefinitionData = IBFunctionDefinitionData
  { ibFunctionDefinitionRange :: Range,
    ibFunctionDefinitionCapturedIdentifiers :: Set IBIdentifier
  }

type instance FunctionStatementContent IdentifierBindingPhase = IBFunctionDefinition

-- Non-positional statement

type IBNonPositionalStatement = NonPositionalStatement IdentifierBindingPhase

type instance NonPositionalStatementData IdentifierBindingPhase = Range

-- Identifier
type IBIdentifier = Identifier IdentifierBindingPhase

type instance Identifier IdentifierBindingPhase = Either IBValueIdentifier IBFunctionIdentifier

type IBValueIdentifier = ValueIdentifier IdentifierBindingPhase

type instance ValueIdentifier IdentifierBindingPhase = BoundValueIdentifier

data BoundValueIdentifier = BoundValueIdentifier ValueIdentifierIndex UnboundIdentifier
  deriving (Eq, Ord)

instance Pretty BoundValueIdentifier where
  pretty (BoundValueIdentifier index name) = "(BoundValueIdentifier " ++ show index ++ " " ++ pretty name ++ ")"

type ValueIdentifierIndex = Int

type IBFunctionIdentifier = FunctionIdentifier IdentifierBindingPhase

type instance FunctionIdentifier IdentifierBindingPhase = BoundFunctionIdentifier

data BoundFunctionIdentifier = BoundFunctionIdentifier FunctionIndex UnboundIdentifier
  deriving (Eq, Ord)

instance Pretty BoundFunctionIdentifier where
  pretty (BoundFunctionIdentifier index name) = "(BoundFunctionIdentifier " ++ show index ++ " " ++ pretty name ++ ")"

-- Expression
type IBExpression = Expression IdentifierBindingPhase

type instance ExpressionData IdentifierBindingPhase = Range

type instance FunctionExpressionContent IdentifierBindingPhase = IBFunctionDefinition

instance WithRange IBExpression where
  getRange = getExpressionData

-- Type annotation
type IBWithTypeAnnotation = WithTypeAnnotation IdentifierBindingPhase

type instance TypeAnnotation IdentifierBindingPhase = Maybe IBTypeExpression

-- Type
type IBTypeExpression = TypeExpression IdentifierBindingPhase

type instance TypeExpressionData IdentifierBindingPhase = Range

instance WithRange IBTypeExpression where
  getRange = getTypeExpressionData