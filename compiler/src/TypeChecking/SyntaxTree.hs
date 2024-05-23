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
    TCRecordIdentifier,
    TCFieldIdentifier,
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
    fromTypeExpression,
    asFieldType,
  )
where

import Control.Monad (unless)
import Core.Errors
import Core.FilePositions
import Core.SyntaxTree
import Core.Type
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import IdentifierBinding.SyntaxTree
import Parsing.SyntaxTree (UnboundIdentifier)

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

type TCRecordIdentifier = RecordIdentifier TypeCheckingPhase

type instance RecordIdentifier TypeCheckingPhase = BoundRecordIdentifier

type TCFieldIdentifier = FieldIdentifier TypeCheckingPhase

type instance FieldIdentifier TypeCheckingPhase = UnboundIdentifier

-- Expression
type TCExpression = Expression TypeCheckingPhase

data TCExpresionData = TCExpresionData
  { expressionRange :: Range,
    expressionType :: Type,
    expressionReturnInfo :: ReturnInfo
  }

type instance ExpressionData TypeCheckingPhase = TCExpresionData

type instance RecordFieldValues TypeCheckingPhase = Map TCFieldIdentifier TCExpression

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

fromTypeExpression :: IBTypeExpression -> WithErrors Type
fromTypeExpression = fromTypeExpressionHelper False

fromTypeExpressionHelper :: Bool -> IBTypeExpression -> WithErrors Type
fromTypeExpressionHelper _ (IntTypeExpression _) = return IntType
fromTypeExpressionHelper _ (FloatTypeExpression _) = return FloatType
fromTypeExpressionHelper _ (CharTypeExpression _) = return CharType
fromTypeExpressionHelper _ (StringTypeExpression _) = return StringType
fromTypeExpressionHelper _ (BoolTypeExpression _) = return BoolType
fromTypeExpressionHelper _ (NilTypeExpression _) = return NilType
fromTypeExpressionHelper _ (FunctionTypeExpression _ parameterTypeExpressions returnTypeExpression) = do
  parameterTypes <- mapM fromTypeExpression parameterTypeExpressions
  returnType <- fromTypeExpression returnTypeExpression
  return $ FunctionType parameterTypes returnType
fromTypeExpressionHelper inMutableContext (RecordTypeExpression _ recordName) =
  return $ RecordUnionType (if inMutableContext then Mutable else Immutable) (Set.singleton recordName)
fromTypeExpressionHelper inMutableContext (UnionTypeExpression expressionRange left right) = do
  (leftMutability, leftRecords) <- getRecordSet left
  (rightMutability, rightRecords) <- getRecordSet right
  unless (leftMutability == rightMutability) $ singleError (UnionTypeDifferentMutabilitiesError expressionRange leftMutability rightMutability)
  return $ RecordUnionType leftMutability (Set.union leftRecords rightRecords)
  where
    getRecordSet sideTypeExpression = do
      sideType <- fromTypeExpressionHelper inMutableContext sideTypeExpression
      case sideType of
        (RecordUnionType mutability records) -> return (mutability, records)
        nonRecordType -> singleError $ NonRecordTypeInUnionError nonRecordType (getRange sideTypeExpression)
fromTypeExpressionHelper _ (MutTypeExpression expressionRange inner) = do
  innerType <- fromTypeExpressionHelper True inner
  case innerType of
    RecordUnionType {} -> return ()
    _ -> singleError $ NonRecordTypeMarkedAsMutableError expressionRange innerType
  return innerType

asFieldType :: IBTypeExpression -> WithErrors (Mutability -> Type)
asFieldType (IntTypeExpression _) = return $ const IntType
asFieldType (FloatTypeExpression _) = return $ const FloatType
asFieldType (CharTypeExpression _) = return $ const CharType
asFieldType (StringTypeExpression _) = return $ const StringType
asFieldType (BoolTypeExpression _) = return $ const BoolType
asFieldType (NilTypeExpression _) = return $ const NilType
asFieldType (FunctionTypeExpression _ parameterTypeExpressions returnTypeExpression) = do
  parameterTypes <- mapM fromTypeExpression parameterTypeExpressions
  returnType <- fromTypeExpression returnTypeExpression
  return $ const $ FunctionType parameterTypes returnType
asFieldType (RecordTypeExpression _ recordName) = return $ \mutability -> RecordUnionType mutability (Set.singleton recordName)
asFieldType (UnionTypeExpression _ left right) = do
  leftRecords <- getRecordSet left
  rightRecords <- getRecordSet right
  return $ \mutability -> RecordUnionType mutability (Set.union leftRecords rightRecords)
  where
    getRecordSet sideTypeExpression = do
      sideType <- asFieldType sideTypeExpression
      case sideType Immutable of
        (RecordUnionType _ records) -> return records
        nonRecordType -> singleError $ NonRecordTypeInUnionError nonRecordType (getRange sideTypeExpression)
asFieldType (MutTypeExpression expressionRange _) = singleError $ FieldTypeMarkedAsMutableError expressionRange