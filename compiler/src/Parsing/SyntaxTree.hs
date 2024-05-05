{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Parsing.SyntaxTree
  ( ParsingPhase,
    PModule,
    PStatement,
    PIdentifier,
    PExpression,
    UnboundIdentifier,
    PFunctionExpressionContent (PFunctionExpressionContent),
    PTypeExpression,
    PWithTypeAnnotation,
  )
where

import Core.FilePositions
import Core.SyntaxTree
import Core.Utils
import Data.Sequence (Seq)
import Data.Text (Text)

data ParsingPhase

-- Module
type PModule = Module ParsingPhase

type instance ModuleData ParsingPhase = ()

type instance ModuleContent ParsingPhase = PMainFunctionDefinition

type PMainFunctionDefinition = MainFunctionDefinition ParsingPhase

type instance MainFunctionDefinitionData ParsingPhase = ()

-- Statement
type PStatement = Statement ParsingPhase

type instance StatementData ParsingPhase = Range

-- Identifier
type PIdentifier = Identifier ParsingPhase

type instance IdentifierData ParsingPhase = Range

type UnboundIdentifier = Text

type instance IdentifierContent ParsingPhase = UnboundIdentifier

-- Expression
type PExpression = Expression ParsingPhase

type instance ExpressionData ParsingPhase = Range

data PFunctionExpressionContent = PFunctionExpressionContent (Seq (PWithTypeAnnotation PIdentifier)) (PWithTypeAnnotation PExpression)

instance Pretty PFunctionExpressionContent where
  pretty (PFunctionExpressionContent parameters body) = "(" ++ pretty parameters ++ ") " ++ pretty body

type instance FunctionExpressionContent ParsingPhase = PFunctionExpressionContent

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