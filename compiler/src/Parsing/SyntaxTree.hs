{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Parsing.SyntaxTree
  ( ParsingPhase,
    PModule,
    PStatement,
    PIdentifier,
    PExpression,
    UnboundIdentifier,
    PTypeExpression,
    PWithTypeAnnotation,
  )
where

import Core.FilePositions
import Core.SyntaxTree
import Data.Text (Text)

data ParsingPhase

-- Module
type PModule = Module ParsingPhase

type instance ModuleData ParsingPhase = ()

type instance ModuleContent ParsingPhase = PMainFunction

type PMainFunction = MainFunction ParsingPhase

type instance MainFunctionData ParsingPhase = ()

type PFunctionDefinition = FunctionDefinition ParsingPhase

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

type instance FunctionExpressionContent ParsingPhase = PFunctionDefinition

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