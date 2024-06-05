{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Parsing.SyntaxTree
  ( ParsingPhase,
    PModule,
    PScope,
    PStatement,
    PNonPositionalStatement,
    PIdentifier (PIdentifier),
    PExpression,
    UnboundIdentifier,
    PTypeExpression,
    PWithTypeAnnotation,
    PRecordIdentifier,
    PFieldIdentifier,
    PValueIdentifier,
    PTypeIdentifier,
    PTypeParameter,
    PMutabilityParameter,
  )
where

import Core.FilePositions
import Core.SyntaxTree
import Core.Utils
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)

data ParsingPhase

-- Module
type PModule = Module ParsingPhase

type instance ModuleData ParsingPhase = ()

type PMainFunction = MainFunction ParsingPhase

type instance MainFunctionData ParsingPhase = ()

type PFunctionDefinition = FunctionDefinition ParsingPhase

type instance FunctionDefinitionData ParsingPhase = Range

-- Scope
type PScope = Scope ParsingPhase

type instance ScopeData ParsingPhase = ()

-- Statement
type PStatement = Statement ParsingPhase

type instance StatementData ParsingPhase = Range

-- Non-positional statement

type PNonPositionalStatement = NonPositionalStatement ParsingPhase

type instance NonPositionalStatementData ParsingPhase = Range

type instance TypeParameters ParsingPhase = Seq PTypeParameter

-- Identifier
type UnboundIdentifier = Text

type instance Identifier ParsingPhase = PIdentifier

data PIdentifier = PIdentifier UnboundIdentifier PTypeArguments

instance Pretty PIdentifier where
  pretty (PIdentifier identifier typeArguments) = "(PIdentifier " ++ pretty identifier ++ pretty typeArguments ++ ")"

type PValueIdentifier = ValueIdentifier ParsingPhase

type instance ValueIdentifier ParsingPhase = UnboundIdentifier

type PFunctionIdentifier = FunctionIdentifier ParsingPhase

type instance FunctionIdentifier ParsingPhase = UnboundIdentifier

type PRecordIdentifier = RecordIdentifier ParsingPhase

type instance RecordIdentifier ParsingPhase = UnboundIdentifier

type PFieldIdentifier = FieldIdentifier ParsingPhase

type instance FieldIdentifier ParsingPhase = UnboundIdentifier

type PTypeIdentifier = TypeIdentifier ParsingPhase

type instance TypeIdentifier ParsingPhase = UnboundIdentifier

type PTypeParameter = TypeParameter ParsingPhase

type instance TypeParameter ParsingPhase = UnboundIdentifier

type PMutabilityParameter = MutabilityParameter ParsingPhase

type instance MutabilityParameter ParsingPhase = UnboundIdentifier

-- Expression
type PExpression = Expression ParsingPhase

type instance ExpressionData ParsingPhase = Range

type instance RecordFieldValues ParsingPhase = Map PFieldIdentifier PExpression

type PTypeArguments = TypeArguments ParsingPhase

type instance TypeArguments ParsingPhase = Seq PTypeExpression

instance WithRange PExpression where
  getRange = getExpressionData

-- Type annotation
type PWithTypeAnnotation = WithTypeAnnotation ParsingPhase

type instance TypeAnnotation ParsingPhase = Maybe PTypeExpression

-- Type expression
type PTypeExpression = TypeExpression ParsingPhase

type instance TypeExpressionData ParsingPhase = Range

instance WithRange PTypeExpression where
  getRange = getTypeExpressionData