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
    IBTypeExpression,
    IBWithTypeAnnotation,
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

type instance StatementData IdentifierBindingPhase = Range

-- Identifier
type IBIdentifier = Identifier IdentifierBindingPhase

type instance IdentifierData IdentifierBindingPhase = Range

type BoundIdentifier = Int

type instance IdentifierContent IdentifierBindingPhase = BoundIdentifier

instance WithRange IBIdentifier where
  getRange (Identifier d _) = d

-- Expression
type IBExpression = Expression IdentifierBindingPhase

type instance ExpressionData IdentifierBindingPhase = Range

type FunctionIndex = Int

data IBFunctionExpressionContent = IBFunctionExpressionContent FunctionIndex (Seq IBIdentifier)

instance Pretty IBFunctionExpressionContent where
  pretty (IBFunctionExpressionContent functionIndex capturedIdentifiers) = show functionIndex ++ "(" ++ pretty capturedIdentifiers ++ ")"

type instance FunctionExpressionContent IdentifierBindingPhase = IBFunctionExpressionContent

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