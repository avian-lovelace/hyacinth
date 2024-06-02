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
    BoundTypeParameter (BoundTypeParameter),
    IBTypeParameter,
    TypeParameterIndex,
    IBTypeIdentifier,
    BoundMutabilityParameter (BoundMutabilityParameter),
    IBMutabilityParameter,
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
    ValueIdentifierIndex,
    RecordIndex,
    getValueIdentifierIndex,
    getFunctionIdentifierIndex,
    getRecordIdentifierIndex,
  )
where

import Core.FilePositions
import Core.SyntaxTree
import Core.Utils
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Text (Text)
import Parsing.SyntaxTree

data IdentifierBindingPhase

-- Module
type IBModule = Module IdentifierBindingPhase

type instance ModuleData IdentifierBindingPhase = ()

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

getValueIdentifierIndex :: BoundValueIdentifier -> ValueIdentifierIndex
getValueIdentifierIndex (BoundValueIdentifier index _) = index

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

getFunctionIdentifierIndex :: BoundFunctionIdentifier -> FunctionIndex
getFunctionIdentifierIndex (BoundFunctionIdentifier index _) = index

type IBRecordIdentifier = RecordIdentifier IdentifierBindingPhase

type instance RecordIdentifier IdentifierBindingPhase = BoundRecordIdentifier

type RecordIndex = Int

data BoundRecordIdentifier = BoundRecordIdentifier RecordIndex UnboundIdentifier
  deriving (Eq, Ord)

instance Pretty BoundRecordIdentifier where
  pretty (BoundRecordIdentifier index name) = "(BoundRecordIdentifier " ++ show index ++ " " ++ pretty name ++ ")"

instance WithTextName BoundRecordIdentifier where
  getTextName (BoundRecordIdentifier _ name) = name

getRecordIdentifierIndex :: BoundRecordIdentifier -> RecordIndex
getRecordIdentifierIndex (BoundRecordIdentifier index _) = index

type IBFieldIdentifier = FieldIdentifier IdentifierBindingPhase

type instance FieldIdentifier IdentifierBindingPhase = UnboundIdentifier

type IBMutabilityParameter = MutabilityParameter IdentifierBindingPhase

type instance MutabilityParameter IdentifierBindingPhase = BoundMutabilityParameter

type MutabilityParameterIndex = Int

data BoundMutabilityParameter = BoundMutabilityParameter MutabilityParameterIndex UnboundIdentifier
  deriving (Eq, Ord)

instance Pretty BoundMutabilityParameter where
  pretty (BoundMutabilityParameter index name) = "(BoundMutabilityParameter " ++ show index ++ " " ++ pretty name ++ ")"

instance WithTextName BoundMutabilityParameter where
  getTextName (BoundMutabilityParameter _ name) = name

type IBTypeIdentifier = TypeIdentifier IdentifierBindingPhase

type instance TypeIdentifier IdentifierBindingPhase = IBTypeParameter

type IBTypeParameter = TypeParameter IdentifierBindingPhase

type instance TypeParameter IdentifierBindingPhase = BoundTypeParameter

type TypeParameterIndex = Int

data BoundTypeParameter = BoundTypeParameter TypeParameterIndex UnboundIdentifier
  deriving (Eq, Ord)

instance Pretty BoundTypeParameter where
  pretty (BoundTypeParameter index name) = "(BoundTypeParameter " ++ show index ++ " " ++ pretty name ++ ")"

instance WithTextName BoundTypeParameter where
  getTextName (BoundTypeParameter _ name) = name

-- Expression
type IBExpression = Expression IdentifierBindingPhase

type instance ExpressionData IdentifierBindingPhase = Range

type instance FunctionExpressionContent IdentifierBindingPhase = IBFunctionDefinition

type instance RecordFieldValues IdentifierBindingPhase = Map IBFieldIdentifier IBExpression

type instance TypeArguments IdentifierBindingPhase = Seq IBTypeExpression

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