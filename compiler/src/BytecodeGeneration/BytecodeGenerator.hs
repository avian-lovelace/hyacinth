module BytecodeGeneration.BytecodeGenerator
  ( encodeFile,
  )
where

import BytecodeGeneration.Bytecode
import BytecodeGeneration.BytecodeGeneration
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LB
import Data.Foldable
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import IdentifierBinding.SyntaxTree
import IntermediateCodeGeneration.IntermediateCode

encodeFile :: Mod -> BB.Builder
encodeFile fileScope =
  let (BytecodeGeneratorState {constants}, code) = runGenerator (encodeModule fileScope) initialState
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
encodeMainFunction (MainFunc scope) = do
  body <- encodeScope scope
  return $ body <> returnInstruction

encodeFunction :: SubFunc -> BytecodeGenerator BB.Builder
encodeFunction (SubFunc parameters body) = do
  encodedBody <- withFunctionScope parameters $ encodeExpression body
  return $ encodedBody <> returnInstruction

encodeStatement :: Stmt -> BytecodeGenerator BB.Builder
encodeStatement (VariableDeclarationStmt variableName variableValue) = do
  addVariableToScope variableName
  encodedVariableValue <- encodeExpression variableValue
  return encodedVariableValue
encodeStatement (VariableMutationStmt variableName variableValue) = do
  encodedVariableValue <- encodeExpression variableValue
  variableIndex <- getVariableIndex variableName
  return $ encodedVariableValue <> mutateVariableInstruction (fromIntegral variableIndex)
encodeStatement (ExpressionStmt expression) = do
  encodedExpression <- encodeExpression expression
  return $ encodedExpression <> popInstruction
encodeStatement (WhileLoopStmt condition body) = do
  encodedCondition <- encodeExpression condition
  let conditionBytestring = BB.toLazyByteString encodedCondition
  -- The body of a while loop statement is implicitly an expression statement, not just an expresison
  encodedBody <- encodeStatement (ExpressionStmt body)
  let bodyBytestring = BB.toLazyByteString encodedBody
  return $
    BB.lazyByteString conditionBytestring
      <> jumpIfFalseInstruction (fromIntegral (LB.length bodyBytestring + jumpInstructionNumBytes))
      <> BB.lazyByteString bodyBytestring
      <> jumpInstruction (fromIntegral (-(LB.length bodyBytestring + jumpIfFalseInstructionNumBytes + LB.length conditionBytestring + jumpIfFalseInstructionNumBytes)))
encodeStatement (ReturnStmt expression) = do
  encodedExpression <- encodeExpression expression
  return $ encodedExpression <> returnInstruction

encodeExpression :: Expr -> BytecodeGenerator BB.Builder
encodeExpression (LiteralExpr literalValue) = do
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
encodeExpression (BuiltInFunctionExpr builtInFunction parameters) = do
  encodedParameters <- mapM encodeExpression parameters
  let encodedFunction = case builtInFunction of
        NegateFn -> negateInstruction
        AddFn -> addInstruction
        SubtractFn -> subtractInstruction
        MultiplyFn -> multiplyInstruction
        DivideFn -> divideInstruction
        ModuloFn -> moduloInstruction
        NotFn -> notInstruction
        EqualFn -> equalInstruction
        NotEqualFn -> notEqualInstruction
        GreaterFn -> greaterInstruction
        LessFn -> lessInstruction
        GreaterEqualFn -> greaterEqualInstruction
        LessEqualFn -> lessEqualInstruction
        PrintFn -> printInstruction
  return $ fold encodedParameters <> encodedFunction
encodeExpression (IfThenElseExpr condition trueExpression falseExpression) = do
  encodedCondition <- encodeExpression condition
  encodedTrueExpression <- encodeExpression trueExpression
  let trueExpressionBytestring = BB.toLazyByteString encodedTrueExpression
  encodedFalseExpression <- encodeExpression falseExpression
  let falseExpressionBytestring = BB.toLazyByteString encodedFalseExpression
  let jumpAfterTrueBytestring = BB.toLazyByteString $ jumpInstruction (fromIntegral $ LB.length falseExpressionBytestring)
  return $
    encodedCondition
      <> jumpIfFalseInstruction (fromIntegral $ LB.length trueExpressionBytestring + LB.length jumpAfterTrueBytestring)
      <> BB.lazyByteString trueExpressionBytestring
      <> BB.lazyByteString jumpAfterTrueBytestring
      <> BB.lazyByteString falseExpressionBytestring
encodeExpression (ScopeExpr scope) = encodeScope scope
encodeExpression (FunctionExpr functionIndex capturedIdentifiers) = do
  pushIdentifierValues <- traverse pushIdentifierValue capturedIdentifiers
  return $ fold pushIdentifierValues <> functionInstruction (fromIntegral functionIndex) (fromIntegral . length $ capturedIdentifiers)
encodeExpression (CallExpr function arguments) = do
  encodedFunction <- encodeExpression function
  encodedArguments <- traverse encodeExpression arguments
  return $ encodedFunction <> fold encodedArguments <> callInstruction (fromIntegral . length $ arguments)
encodeExpression (RecordExpr recordIndex fields) = do
  encodedFields <- traverse encodeExpression fields
  return $ fold encodedFields <> recordInstruction (fromIntegral recordIndex) (fromIntegral . Seq.length $ fields)
encodeExpression (FieldExpr inner fieldIndex) = do
  encodedInner <- encodeExpression inner
  return $ encodedInner <> fieldInstruction (fromIntegral fieldIndex)

encodeScope :: Seq Stmt -> BytecodeGenerator BB.Builder
encodeScope statements = withNewScope $ do
  encodedStatements <- mapM encodeStatement statements
  return $ fold encodedStatements

pushIdentifierValue :: ValueIdentifierIndex -> BytecodeGenerator BB.Builder
pushIdentifierValue identifier = do
  index <- getVariableIndex identifier
  return $ readVariableInstruction (fromIntegral index)