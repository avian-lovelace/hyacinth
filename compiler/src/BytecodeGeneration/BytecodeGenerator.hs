module BytecodeGeneration.BytecodeGenerator
  ( encodeFile,
  )
where

import BytecodeGeneration.Bytecode
import BytecodeGeneration.BytecodeGeneration
import Core.FilePositions (dummyRange)
import Core.SyntaxTree
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LB
import Data.Foldable
import VariableBinding.SyntaxTree

encodeFile :: VBFileScope -> BB.Builder
encodeFile fileScope =
  let (BytecodeGeneratorState {constants}, code) = runGenerator (encodeFileScope fileScope) initialState
   in BB.word16BE (fromIntegral $ length constants)
        <> foldMap encodeConstant constants
        <> code

encodeFileScope :: VBFileScope -> BytecodeGenerator BB.Builder
encodeFileScope (FileScope _ statements) = withNewScope $ do
  encodedStatements <- mapM encodeStatement statements
  return $ fold encodedStatements

encodeStatement :: VBStatement -> BytecodeGenerator BB.Builder
encodeStatement (PrintStatement _ expression) = do
  encodedExpression <- encodeExpression expression
  return $ encodedExpression <> printInstruction
encodeStatement (VariableDeclarationStatement _ (VariableName _ variableName) variableValue) = do
  addVariableToScope variableName
  encodedVariableValue <- encodeExpression variableValue
  return encodedVariableValue
encodeStatement (VariableMutationStatement _ (VariableName _ variableName) variableValue) = do
  encodedVariableValue <- encodeExpression variableValue
  variableIndex <- getVariableIndex variableName
  return $ encodedVariableValue <> mutateVariableInstruction (fromIntegral variableIndex)
encodeStatement (ExpressionStatement _ expression) = do
  encodedExpression <- encodeExpression expression
  return $ encodedExpression <> popInstruction
encodeStatement (WhileLoopStatement _ condition statement) = do
  encodedCondition <- encodeExpression condition
  let conditionBytestring = BB.toLazyByteString encodedCondition
  encodedStatement <- encodeStatement statement
  let statementBytestring = BB.toLazyByteString encodedStatement
  return $
    BB.lazyByteString conditionBytestring
      <> jumpIfFalseInstruction (fromIntegral (LB.length statementBytestring + jumpInstructionNumBytes))
      <> BB.lazyByteString statementBytestring
      <> jumpInstruction (fromIntegral (-(LB.length statementBytestring + jumpIfFalseInstructionNumBytes + LB.length conditionBytestring + jumpIfFalseInstructionNumBytes)))

encodeExpression :: VBExpression -> BytecodeGenerator BB.Builder
encodeExpression (IntLiteralExpression _ value) = return $ intInstruction $ fromIntegral value
encodeExpression (DoubleLiteralExpression _ value) = return $ doubleInstruction value
encodeExpression (CharLiteralExpression _ value) = return $ charInstruction value
encodeExpression (StringLiteralExpression _ value) = do
  index <- addConstant (StringConstant value)
  return $ constantInstruction (fromIntegral index)
encodeExpression (BoolLiteralExpression _ True) = return $ trueInstruction
encodeExpression (BoolLiteralExpression _ False) = return $ falseInstruction
encodeExpression (NilExpression _) = return $ nilInstruction
encodeExpression (NegateExpression _ innerExpression) = do
  encodedInnerExpression <- encodeExpression innerExpression
  return $ encodedInnerExpression <> negateInstruction
encodeExpression (AddExpression _ leftExpression rightExpression) = do
  encodedLeftExpression <- encodeExpression leftExpression
  encodedRightExpression <- encodeExpression rightExpression
  return $ encodedLeftExpression <> encodedRightExpression <> addInstruction
encodeExpression (SubtractExpression _ leftExpression rightExpression) = do
  encodedLeftExpression <- encodeExpression leftExpression
  encodedRightExpression <- encodeExpression rightExpression
  return $ encodedLeftExpression <> encodedRightExpression <> subtractInstruction
encodeExpression (MultiplyExpression _ leftExpression rightExpression) = do
  encodedLeftExpression <- encodeExpression leftExpression
  encodedRightExpression <- encodeExpression rightExpression
  return $ encodedLeftExpression <> encodedRightExpression <> multiplyInstruction
encodeExpression (DivideExpression _ leftExpression rightExpression) = do
  encodedLeftExpression <- encodeExpression leftExpression
  encodedRightExpression <- encodeExpression rightExpression
  return $ encodedLeftExpression <> encodedRightExpression <> divideInstruction
encodeExpression (ModuloExpression _ leftExpression rightExpression) = do
  encodedLeftExpression <- encodeExpression leftExpression
  encodedRightExpression <- encodeExpression rightExpression
  return $ encodedLeftExpression <> encodedRightExpression <> moduloInstruction
encodeExpression (NotExpression _ innerExpression) = do
  encodedInnerExpression <- encodeExpression innerExpression
  return $ encodedInnerExpression <> notInstruction
encodeExpression (AndExpression _ leftExpression rightExpression) = do
  encodedLeftExpression <- encodeExpression leftExpression
  encodedRightExpression <- encodeExpression rightExpression
  return $ encodedLeftExpression <> encodedRightExpression <> andInstruction
encodeExpression (OrExpression _ leftExpression rightExpression) = do
  encodedLeftExpression <- encodeExpression leftExpression
  encodedRightExpression <- encodeExpression rightExpression
  return $ encodedLeftExpression <> encodedRightExpression <> orInstruction
encodeExpression (EqualExpression _ leftExpression rightExpression) = do
  encodedLeftExpression <- encodeExpression leftExpression
  encodedRightExpression <- encodeExpression rightExpression
  return $ encodedLeftExpression <> encodedRightExpression <> equalInstruction
encodeExpression (NotEqualExpression _ leftExpression rightExpression) = do
  encodedLeftExpression <- encodeExpression leftExpression
  encodedRightExpression <- encodeExpression rightExpression
  return $ encodedLeftExpression <> encodedRightExpression <> notEqualInstruction
encodeExpression (GreaterExpression _ leftExpression rightExpression) = do
  encodedLeftExpression <- encodeExpression leftExpression
  encodedRightExpression <- encodeExpression rightExpression
  return $ encodedLeftExpression <> encodedRightExpression <> greaterInstruction
encodeExpression (LessExpression _ leftExpression rightExpression) = do
  encodedLeftExpression <- encodeExpression leftExpression
  encodedRightExpression <- encodeExpression rightExpression
  return $ encodedLeftExpression <> encodedRightExpression <> lessInstruction
encodeExpression (GreaterEqualExpression _ leftExpression rightExpression) = do
  encodedLeftExpression <- encodeExpression leftExpression
  encodedRightExpression <- encodeExpression rightExpression
  return $ encodedLeftExpression <> encodedRightExpression <> greaterEqualInstruction
encodeExpression (LessEqualExpression _ leftExpression rightExpression) = do
  encodedLeftExpression <- encodeExpression leftExpression
  encodedRightExpression <- encodeExpression rightExpression
  return $ encodedLeftExpression <> encodedRightExpression <> lessEqualInstruction
encodeExpression (VariableExpression _ (VariableName _ variableName)) = do
  index <- getVariableIndex variableName
  return $ readVariableInstruction (fromIntegral index)
encodeExpression (IfThenElseExpression _ condition trueExpression maybeFalseExpression) = do
  encodedCondition <- encodeExpression condition
  encodedTrueExpression <- encodeExpression trueExpression
  let trueExpressionBytestring = BB.toLazyByteString encodedTrueExpression
  encodedFalseExpression <- case maybeFalseExpression of
    Just falseExpression -> encodeExpression falseExpression
    Nothing -> encodeExpression $ NilExpression dummyRange
  let falseExpressionBytestring = BB.toLazyByteString encodedFalseExpression
  let jumpAfterTrueBytestring = BB.toLazyByteString $ jumpInstruction (fromIntegral $ LB.length falseExpressionBytestring)
  return $
    encodedCondition
      <> jumpIfFalseInstruction (fromIntegral $ LB.length trueExpressionBytestring + LB.length jumpAfterTrueBytestring)
      <> BB.lazyByteString trueExpressionBytestring
      <> BB.lazyByteString jumpAfterTrueBytestring
      <> BB.lazyByteString falseExpressionBytestring
encodeExpression (ScopeExpression _ statements) = withNewScope $ do
  encodedStatements <- mapM encodeStatement statements
  return $ fold encodedStatements