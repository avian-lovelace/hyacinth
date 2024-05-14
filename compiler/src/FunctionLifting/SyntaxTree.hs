{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module FunctionLifting.SyntaxTree
  ( FunctionLiftingPhase,
    FLModule,
    FLStatement,
    FLExpression,
    FLMainFunction,
    FLSubFunction,
    FLTypeExpression,
    FLWithTypeAnnotation,
    FLScope,
    FLNonPositionalStatement,
    FLFunctionDefinition,
    FieldIndex,
  )
where

import Core.FilePositions
import Core.SyntaxTree
import Data.Sequence (Seq)
import IdentifierBinding.SyntaxTree

data FunctionLiftingPhase

-- Module
type FLModule = Module FunctionLiftingPhase

type instance ModuleData FunctionLiftingPhase = ()

type instance ModuleContent FunctionLiftingPhase = (FLMainFunction, Seq FLSubFunction)

type FLMainFunction = MainFunction FunctionLiftingPhase

type instance MainFunctionData FunctionLiftingPhase = ()

type FLSubFunction = SubFunction FunctionLiftingPhase

type instance SubFunctionData FunctionLiftingPhase = Range

-- Scope
type FLScope = Scope FunctionLiftingPhase

type instance ScopeData FunctionLiftingPhase = ()

-- Statement
type FLStatement = Statement FunctionLiftingPhase

type instance StatementData FunctionLiftingPhase = Range

-- Non-positional statement

type FLNonPositionalStatement = NonPositionalStatement FunctionLiftingPhase

type instance NonPositionalStatementData FunctionLiftingPhase = Range

type instance FunctionStatementContent FunctionLiftingPhase = FLFunctionDefinition

type FLFunctionDefinition = FunctionDefinition FunctionLiftingPhase

type instance FunctionDefinitionData FunctionLiftingPhase = Range

-- Identifier
type instance Identifier FunctionLiftingPhase = FLValueIdentifier

type FLValueIdentifier = ValueIdentifier FunctionLiftingPhase

type instance ValueIdentifier FunctionLiftingPhase = BoundValueIdentifier

type instance FunctionIdentifier FunctionLiftingPhase = BoundFunctionIdentifier

type instance RecordIdentifier FunctionLiftingPhase = BoundRecordIdentifier

type instance FieldIdentifier FunctionLiftingPhase = FieldIndex

type FieldIndex = Int

-- Expression
type FLExpression = Expression FunctionLiftingPhase

type instance ExpressionData FunctionLiftingPhase = Range

instance WithRange FLExpression where
  getRange = getExpressionData

type instance FunctionExpressionContent FunctionLiftingPhase = TCFunctionReference

type instance RecordFieldValues FunctionLiftingPhase = Seq FLExpression

type TCFunctionReference = FunctionReference FunctionLiftingPhase

-- Type annotation
type FLWithTypeAnnotation = WithTypeAnnotation FunctionLiftingPhase

type instance TypeAnnotation FunctionLiftingPhase = ()

-- Type
type FLTypeExpression = TypeExpression FunctionLiftingPhase

type instance TypeExpressionData FunctionLiftingPhase = Range