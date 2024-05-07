{-# LANGUAGE TypeFamilies #-}

module TypeChecking.SyntaxTree
  ( TypeCheckingPhase,
    ReturnInfo (AlwaysReturns, SometimesReturns, NeverReturns),
    riAnd,
    riOr,
    TCModule,
    TCModuleContent (TCModuleContent),
    TCMainFunction,
    TCSubFunction,
    TCStatement,
    TCStatementData (TCStatementData, statementRange, statementReturnInfo),
    TCIdentifier,
    TCExpression,
    TCExpresionData (TCExpresionData, expressionRange, expressionType, expressionReturnInfo),
    TCFunctionExpressionContent (TCFunctionExpressionContent),
    TCWithTypeAnnotation,
    TCTypeExpression,
  )
where

import Core.FilePositions
import Core.SyntaxTree
import Core.Type (Type)
import Core.Utils
import Data.Sequence (Seq)
import IdentifierBinding.SyntaxTree (BoundIdentifier, FunctionIndex)

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

data TCModuleContent = TCModuleContent TCMainFunction (Seq TCSubFunction)

instance Pretty TCModuleContent where
  pretty (TCModuleContent mainFunction subFunctions) = pretty mainFunction ++ "(" ++ pretty subFunctions ++ ")"

type instance ModuleContent TypeCheckingPhase = TCModuleContent

type TCMainFunction = MainFunction TypeCheckingPhase

type instance MainFunctionData TypeCheckingPhase = ()

type TCSubFunction = SubFunction TypeCheckingPhase

type instance SubFunctionData TypeCheckingPhase = Range

-- Statement
type TCStatement = Statement TypeCheckingPhase

data TCStatementData = TCStatementData
  { statementRange :: Range,
    statementReturnInfo :: ReturnInfo
  }

type instance StatementData TypeCheckingPhase = TCStatementData

-- Identifier
type TCIdentifier = Identifier TypeCheckingPhase

type instance IdentifierData TypeCheckingPhase = Range

type instance IdentifierContent TypeCheckingPhase = BoundIdentifier

instance WithRange TCIdentifier where
  getRange (Identifier d _) = d

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

data TCFunctionExpressionContent = TCFunctionExpressionContent FunctionIndex (Seq TCIdentifier)

instance Pretty TCFunctionExpressionContent where
  pretty (TCFunctionExpressionContent functionIndex capturedIdentifiers) = show functionIndex ++ "(" ++ pretty capturedIdentifiers ++ ")"

type instance FunctionExpressionContent TypeCheckingPhase = TCFunctionExpressionContent

-- Type annotation
type TCWithTypeAnnotation = WithTypeAnnotation TypeCheckingPhase

type instance TypeAnnotation TypeCheckingPhase = ()

-- Type
type TCTypeExpression = TypeExpression TypeCheckingPhase

type instance TypeExpressionData TypeCheckingPhase = Range