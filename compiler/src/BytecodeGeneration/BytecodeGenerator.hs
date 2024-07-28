module BytecodeGeneration.BytecodeGenerator
  ( encodeFile,
  )
where

import BytecodeGeneration.Bytecode
import BytecodeGeneration.BytecodeGeneration
import Control.Monad (forM)
import Control.Monad.State (runState)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LB
import Data.Foldable
import Data.Sequence (Seq (Empty, (:<|)))
import qualified Data.Sequence as Seq
import IdentifierBinding.SyntaxTree
import IntermediateCodeGeneration.IntermediateCode

encodeFile :: Mod -> BB.Builder
encodeFile fileScope =
  let (code, BytecodeGeneratorState {constants}) = runState (encodeModule fileScope) initialState
   in BB.word16BE (fromIntegral $ length constants)
        <> foldMap encodeConstant constants
        <> code

encodeModule :: Mod -> BytecodeGenerator BB.Builder
encodeModule (Mod mainFunction subFunctions) = do
  encodedMainFunction <- encodeMainFunction mainFunction
  encodedSubFunctions <- traverse encodeFunction subFunctions
  return $ withNumBytes encodedMainFunction <> fold (withNumBytes <$> encodedSubFunctions)
  where
    withNumBytes builder =
      let bytestring = BB.toLazyByteString builder
       in BB.word32BE (fromIntegral . LB.length $ bytestring) <> BB.lazyByteString bytestring

encodeMainFunction :: MainFunc -> BytecodeGenerator BB.Builder
encodeMainFunction (MainFunc statements) = do
  encodedStatements <- mapM encodeStatement statements
  return $ fold encodedStatements <> returnInstruction

encodeFunction :: SubFunc -> BytecodeGenerator BB.Builder
encodeFunction (SubFunc parameters body) = do
  initializeFunction parameters
  encodedBody <- encodeExpression body
  return $ encodedBody <> returnInstruction

encodeStatement :: Stmt -> BytecodeGenerator BB.Builder
encodeStatement (VariableDeclarationStmt variableName variableValue) = do
  encodedVariableValue <- encodeExpression variableValue
  addVariable variableName
  return encodedVariableValue
encodeStatement (VariableMutationStmt variableName variableValue) = do
  encodedVariableValue <- encodeExpression variableValue
  variableIndex <- getVariableIndex variableName
  adjustStackSize (-1)
  return $ encodedVariableValue <> mutateVariableInstruction (fromIntegral variableIndex)
encodeStatement (FieldMutationStmt record fieldIndex value) = do
  encodedRecord <- encodeExpression record
  encodedValue <- encodeExpression value
  adjustStackSize (-2)
  return $ encodedRecord <> encodedValue <> mutateFieldInstruction (fromIntegral fieldIndex)
encodeStatement (IndexMutationStmt list index value) = do
  encodedList <- encodeExpression list
  encodedIndex <- encodeExpression index
  encodedValue <- encodeExpression value
  adjustStackSize (-3)
  return $ encodedList <> encodedIndex <> encodedValue <> mutateIndexInstruction
encodeStatement (ExpressionStmt expression) = do
  encodedExpression <- encodeExpression expression
  adjustStackSize (-1)
  return $ encodedExpression <> popInstruction
encodeStatement (WhileLoopStmt condition body) = do
  encodedCondition <- encodeExpression condition
  adjustStackSize (-1)
  let conditionBytestring = BB.toLazyByteString encodedCondition
  -- The body of a while loop statement is implicitly an expression statement, not just an expression
  encodedBody <- encodeStatement (ExpressionStmt body)
  let bodyBytestring = BB.toLazyByteString encodedBody
  return $
    BB.lazyByteString conditionBytestring
      <> jumpIfFalseInstruction (fromIntegral (LB.length bodyBytestring + jumpInstructionNumBytes))
      <> BB.lazyByteString bodyBytestring
      <> jumpInstruction (fromIntegral (-(LB.length bodyBytestring + jumpIfFalseInstructionNumBytes + LB.length conditionBytestring + jumpIfFalseInstructionNumBytes)))
encodeStatement (ReturnStmt expression) = do
  encodedExpression <- encodeExpression expression
  adjustStackSize (-1)
  return $ encodedExpression <> returnInstruction

encodeExpression :: Expr -> BytecodeGenerator BB.Builder
encodeExpression (LiteralExpr literalValue) = do
  adjustStackSize 1
  case literalValue of
    IntLiteral value -> return $ intInstruction $ fromIntegral value
    FloatLiteral value -> return $ floatInstruction value
    CharLiteral value -> return $ charInstruction value
    StringLiteral value -> do
      index <- addConstant (StringConstant value)
      return $ constantInstruction (fromIntegral index)
    BoolLiteral True -> return trueInstruction
    BoolLiteral False -> return falseInstruction
    NilLiteral -> return nilInstruction
encodeExpression (IdentifierExpr identifier) = pushIdentifierValue identifier
encodeExpression (BuiltInFunctionExpr builtInFunction) = do
  adjustStackSize 1
  return $ builtInFunctionInstruction builtInFunction
encodeExpression (IfThenElseExpr condition trueExpression falseExpression) = do
  startingStackSize <- getStackSize
  encodedCondition <- encodeExpression condition
  setStackSize startingStackSize
  encodedTrueExpression <- encodeExpression trueExpression
  let trueExpressionBytestring = BB.toLazyByteString encodedTrueExpression
  setStackSize startingStackSize
  encodedFalseExpression <- encodeExpression falseExpression
  let falseExpressionBytestring = BB.toLazyByteString encodedFalseExpression
  let jumpAfterTrueBytestring = BB.toLazyByteString $ jumpInstruction (fromIntegral $ LB.length falseExpressionBytestring)
  return $
    encodedCondition
      <> jumpIfFalseInstruction (fromIntegral $ LB.length trueExpressionBytestring + LB.length jumpAfterTrueBytestring)
      <> BB.lazyByteString trueExpressionBytestring
      <> BB.lazyByteString jumpAfterTrueBytestring
      <> BB.lazyByteString falseExpressionBytestring
encodeExpression (ScopeExpr statements) = do
  startStackSize <- getStackSize
  encodedStatements <- mapM encodeStatement statements
  endStackSize <- getStackSize
  setStackSize $ startStackSize + 1
  return $ fold encodedStatements <> popMultipleInstruction (fromIntegral $ endStackSize - startStackSize) <> nilInstruction
encodeExpression (FunctionExpr functionIndex capturedIdentifiers) = do
  startingStackSize <- getStackSize
  pushIdentifierValues <- traverse pushIdentifierValue capturedIdentifiers
  setStackSize $ startingStackSize + 1
  return $ fold pushIdentifierValues <> functionInstruction (fromIntegral functionIndex) (fromIntegral . length $ capturedIdentifiers)
encodeExpression (CallExpr function arguments) = do
  startingStackSize <- getStackSize
  encodedFunction <- encodeExpression function
  encodedArguments <- traverse encodeExpression arguments
  setStackSize $ startingStackSize + 1
  return $ encodedFunction <> fold encodedArguments <> callInstruction (fromIntegral . length $ arguments)
encodeExpression (RecordExpr recordIndex fields) = do
  startingStackSize <- getStackSize
  encodedFields <- traverse encodeExpression fields
  setStackSize $ startingStackSize + 1
  return $ fold encodedFields <> recordInstruction (fromIntegral recordIndex) (fromIntegral . Seq.length $ fields)
encodeExpression (FieldExpr inner fieldIndex) = do
  encodedInner <- encodeExpression inner
  return $ encodedInner <> fieldInstruction (fromIntegral fieldIndex)
encodeExpression (CaseExpr switch cases) = do
  startingStackSize <- getStackSize
  encodedSwitch <- encodeExpression switch
  caseRecordValuePairs <- forM cases $ \(caseRecord, caseParameter, caseValue) -> do
    addVariable caseParameter
    return (caseRecord, caseValue)
  encodedCases <- encodeCases caseRecordValuePairs
  setStackSize $ startingStackSize + 1
  return $ encodedSwitch <> encodedCases <> removeFromStackInstruction 1
  where
    encodeCases :: Seq (RecordIndex, Expr) -> BytecodeGenerator BB.Builder
    encodeCases Empty = undefined
    encodeCases ((_, caseValue) :<| Empty) = encodeExpression caseValue
    encodeCases ((caseRecord, caseValue) :<| restCases) = do
      startingStackSize <- getStackSize
      encodedCaseValue <- encodeExpression caseValue
      let caseValueBytestring = BB.toLazyByteString encodedCaseValue
      setStackSize startingStackSize
      encodedRestCases <- encodeCases restCases
      let restCasesBytestring = BB.toLazyByteString encodedRestCases
      return $
        jumpIfDoesntMatchRecordIdInstruction (fromIntegral caseRecord) (fromIntegral $ LB.length caseValueBytestring + jumpIfFalseInstructionNumBytes)
          <> BB.lazyByteString caseValueBytestring
          <> jumpInstruction (fromIntegral $ LB.length restCasesBytestring)
          <> BB.lazyByteString restCasesBytestring
encodeExpression (ListExpr values) = do
  startingStackSize <- getStackSize
  encodedValues <- mapM encodeExpression values
  setStackSize $ startingStackSize + 1
  return $ fold encodedValues <> listInstruction (fromIntegral $ Seq.length values)
encodeExpression (IndexExpr innerExpr indexExpr) = do
  startingStackSize <- getStackSize
  encodedInner <- encodeExpression innerExpr
  encodedIndex <- encodeExpression indexExpr
  setStackSize $ startingStackSize + 1
  return $ encodedInner <> encodedIndex <> indexInstruction

pushIdentifierValue :: ValueIdentifierIndex -> BytecodeGenerator BB.Builder
pushIdentifierValue identifier = do
  index <- getVariableIndex identifier
  adjustStackSize 1
  return $ readVariableInstruction (fromIntegral index)