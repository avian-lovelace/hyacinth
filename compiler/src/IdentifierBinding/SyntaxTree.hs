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
    BoundRecordIdentifier (BoundRecordIdentifier),
    IBRecordIdentifier,
    IBFieldIdentifier,
    IBExpression,
    IBMainFunction,
    IBSubFunction,
    IBTypeExpression,
    IBWithTypeAnnotation,
    IBScope,
    IBNonPositionalStatement,
    IBFunctionDefinition,
    IBFunctionDefinitionData (IBFunctionDefinitionData, ibFunctionDefinitionCapturedIdentifiers, ibFunctionDefinitionRange),
    WithTextName (getTextName),
  )
where

import Core.FilePositions
import Core.SyntaxTree
import Core.Utils
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
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

class WithTextName i where
  getTextName :: i -> Text

instance WithTextName BoundValueIdentifier where
  getTextName (BoundValueIdentifier _ name) = name

type ValueIdentifierIndex = Int

type IBFunctionIdentifier = FunctionIdentifier IdentifierBindingPhase

type instance FunctionIdentifier IdentifierBindingPhase = BoundFunctionIdentifier

data BoundFunctionIdentifier = BoundFunctionIdentifier FunctionIndex UnboundIdentifier
  deriving (Eq, Ord)

instance Pretty BoundFunctionIdentifier where
  pretty (BoundFunctionIdentifier index name) = "(BoundFunctionIdentifier " ++ show index ++ " " ++ pretty name ++ ")"

instance WithTextName BoundFunctionIdentifier where
  getTextName (BoundFunctionIdentifier _ name) = name

type IBRecordIdentifier = RecordIdentifier IdentifierBindingPhase

type instance RecordIdentifier IdentifierBindingPhase = BoundRecordIdentifier

type RecordIndex = Int

data BoundRecordIdentifier = BoundRecordIdentifier RecordIndex UnboundIdentifier
  deriving (Eq, Ord)

instance Pretty BoundRecordIdentifier where
  pretty (BoundRecordIdentifier index name) = "(BoundRecordIdentifier " ++ show index ++ " " ++ pretty name ++ ")"

instance WithTextName BoundRecordIdentifier where
  getTextName (BoundRecordIdentifier _ name) = name

type IBFieldIdentifier = FieldIdentifier IdentifierBindingPhase

type instance FieldIdentifier IdentifierBindingPhase = UnboundIdentifier

-- Expression
type IBExpression = Expression IdentifierBindingPhase

type instance ExpressionData IdentifierBindingPhase = Range

type instance FunctionExpressionContent IdentifierBindingPhase = IBFunctionDefinition

type instance RecordFieldValues IdentifierBindingPhase = Map IBFieldIdentifier IBExpression

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