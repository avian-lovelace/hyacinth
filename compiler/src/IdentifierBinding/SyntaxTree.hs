{-# LANGUAGE TypeFamilies #-}

module IdentifierBinding.SyntaxTree
  ( IdentifierBindingPhase,
    IBModule,
    IBModuleContent (IBModuleContent),
    IBStatement,
    IBIdentifier,
    IBExpression,
    BoundIdentifier,
    IBMainFunction,
    IBSubFunction,
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

data IBModuleContent = IBModuleContent IBMainFunction (Seq IBSubFunction)

instance Pretty IBModuleContent where
  pretty (IBModuleContent mainFunction subFunctions) = pretty mainFunction ++ "(" ++ pretty subFunctions ++ ")"

type instance ModuleContent IdentifierBindingPhase = IBModuleContent

type IBMainFunction = MainFunction IdentifierBindingPhase

type instance MainFunctionData IdentifierBindingPhase = ()

type IBSubFunction = SubFunction IdentifierBindingPhase

type instance SubFunctionData IdentifierBindingPhase = Range

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