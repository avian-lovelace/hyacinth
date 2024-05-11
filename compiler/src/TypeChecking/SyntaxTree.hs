{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TypeChecking.SyntaxTree
  ( TypeCheckingPhase,
    ReturnInfo (AlwaysReturns, SometimesReturns, NeverReturns),
    riAnd,
    riOr,
    TCModule,
    TCMainFunction,
    TCStatement,
    TCStatementData (TCStatementData, statementRange, statementReturnInfo),
    TCIdentifier,
    TCValueIdentifier,
    TCFunctionIdentifier,
    TCExpression,
    TCExpresionData (TCExpresionData, expressionRange, expressionType, expressionReturnInfo),
    TCWithTypeAnnotation,
    TCTypeExpression,
    TCScope,
    TCNonPositionalStatement,
    TCFunctionDefinition,
    TCFunctionDefinitionData
      ( TCFunctionDefinitionData,
        tcFunctionDefinitionRange,
        tcFunctionDefinitionType,
        tcFunctionDefinitionCapturedIdentifiers
      ),
  )
where

import Core.FilePositions
import Core.SyntaxTree
import Core.Type (Type)
import Data.Set (Set)
import IdentifierBinding.SyntaxTree

data TypeCheckingPhase

data ReturnInfo
  = AlwaysReturns
  | SometimesReturns
  | NeverReturns
  deriving (Eq)

riAnd :: ReturnInfo -> ReturnInfo -> ReturnInfo
AlwaysReturns `riAnd` _ = AlwaysReturns
_ `riAnd` AlwaysReturns = AlwaysReturns
SometimesReturns `riAnd` _ = SometimesReturns
_ `riAnd` SometimesReturns = SometimesReturns
NeverReturns `riAnd` NeverReturns = NeverReturns

riOr :: ReturnInfo -> ReturnInfo -> ReturnInfo
AlwaysReturns `riOr` AlwaysReturns = AlwaysReturns
NeverReturns `riOr` NeverReturns = NeverReturns
_ `riOr` _ = SometimesReturns

instance Semigroup ReturnInfo where
  (<>) = riAnd

instance Monoid ReturnInfo where
  mempty = NeverReturns

-- Module
type TCModule = Module TypeCheckingPhase

type instance ModuleData TypeCheckingPhase = ()

-- instance Pretty TCModuleContent where
--   pretty (TCModuleContent mainFunction subFunctions) = pretty mainFunction ++ "(" ++ pretty subFunctions ++ ")"

type instance ModuleContent TypeCheckingPhase = TCMainFunction

type TCMainFunction = MainFunction TypeCheckingPhase

type instance MainFunctionData TypeCheckingPhase = ()

-- Scope
type TCScope = Scope TypeCheckingPhase

type instance ScopeData TypeCheckingPhase = ReturnInfo

-- Statement
type TCStatement = Statement TypeCheckingPhase

data TCStatementData = TCStatementData
  { statementRange :: Range,
    statementReturnInfo :: ReturnInfo
  }

type instance StatementData TypeCheckingPhase = TCStatementData

-- Non-positional statement

type TCNonPositionalStatement = NonPositionalStatement TypeCheckingPhase

type instance NonPositionalStatementData TypeCheckingPhase = Range

type instance FunctionStatementContent TypeCheckingPhase = TCFunctionDefinition

type TCFunctionDefinition = FunctionDefinition TypeCheckingPhase

type instance FunctionDefinitionData TypeCheckingPhase = TCFunctionDefinitionData

data TCFunctionDefinitionData = TCFunctionDefinitionData
  { tcFunctionDefinitionRange :: Range,
    tcFunctionDefinitionType :: Type,
    tcFunctionDefinitionCapturedIdentifiers :: Set TCIdentifier
  }

-- Identifier
type TCIdentifier = Identifier TypeCheckingPhase

type instance Identifier TypeCheckingPhase = Either TCValueIdentifier TCFunctionIdentifier

type TCValueIdentifier = ValueIdentifier TypeCheckingPhase

type instance ValueIdentifier TypeCheckingPhase = BoundValueIdentifier

type TCFunctionIdentifier = FunctionIdentifier TypeCheckingPhase

type instance FunctionIdentifier TypeCheckingPhase = BoundFunctionIdentifier

-- Expression
type TCExpression = Expression TypeCheckingPhase

data TCExpresionData = TCExpresionData
  { expressionRange :: Range,
    expressionType :: Type,
    expressionReturnInfo :: ReturnInfo
  }

type instance ExpressionData TypeCheckingPhase = TCExpresionData

instance WithRange TCExpression where
  getRange = expressionRange . getExpressionData

-- instance Pretty TCFunctionExpressionContent where
--   pretty (TCFunctionExpressionContent functionIndex capturedIdentifiers) = show functionIndex ++ "(" ++ pretty capturedIdentifiers ++ ")"

type instance FunctionExpressionContent TypeCheckingPhase = TCFunctionDefinition

-- Type annotation
type TCWithTypeAnnotation = WithTypeAnnotation TypeCheckingPhase

type instance TypeAnnotation TypeCheckingPhase = ()

-- Type
type TCTypeExpression = TypeExpression TypeCheckingPhase

type instance TypeExpressionData TypeCheckingPhase = Range