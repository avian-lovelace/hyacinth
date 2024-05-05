module BytecodeGeneration.BytecodeGenerator
  ( encodeFile,
  )
where

import BytecodeGeneration.Bytecode
import BytecodeGeneration.BytecodeGeneration
import Core.FilePositions
import Core.SyntaxTree
import Core.Type
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LB
import Data.Foldable
import TypeChecking.SyntaxTree

encodeFile :: TCModule -> BB.Builder
encodeFile fileScope =
  let (BytecodeGeneratorState {constants}, code) = runGenerator (encodeModule fileScope) initialState
   in BB.word16BE (fromIntegral $ length constants)
        <> foldMap encodeConstant constants
        <> code

encodeModule :: TCModule -> BytecodeGenerator BB.Builder
encodeModule (Module _ (TCModuleContent mainFunction subFunctions)) = do
  encodedMainFunction <- encodeMainFunction mainFunction
  encodedSubFunctions <- traverse encodeFunction subFunctions
  return $ withNumBytes encodedMainFunction <> fold (withNumBytes <$> encodedSubFunctions)
  where
    withNumBytes builder =
      let bytestring = BB.toLazyByteString builder
       in BB.word32BE (fromIntegral . LB.length $ bytestring) <> BB.lazyByteString bytestring

encodeMainFunction :: TCMainFunctionDefinition -> BytecodeGenerator BB.Builder
encodeMainFunction (MainFunctionDefinition _ statements) = do
  body <- withNewScope $ do
    encodedStatements <- mapM encodeStatement statements
    return $ fold encodedStatements
  return $ body <> returnInstruction

encodeFunction :: TCFunctionDefinition -> BytecodeGenerator BB.Builder
encodeFunction (FunctionDefinition _ parameters capturedIdentifiers (WithTypeAnnotation body ())) = do
  encodedBody <- withFunctionScope (getParameterName <$> parameters) (getIdentifier <$> capturedIdentifiers) $ encodeExpression body
  return $ encodedBody <> returnInstruction
  where
    getParameterName (WithTypeAnnotation (Identifier _ identifier) ()) = identifier
    getIdentifier (Identifier _ identifier) = identifier

encodeStatement :: TCStatement -> BytecodeGenerator BB.Builder
encodeStatement (PrintStatement _ expression) = do
  encodedExpression <- encodeExpression expression
  return $ encodedExpression <> printInstruction
encodeStatement (VariableDeclarationStatement _ _ (WithTypeAnnotation (Identifier _ variableName) _) variableValue) = do
  addVariableToScope variableName
  encodedVariableValue <- encodeExpression variableValue
  return encodedVariableValue
encodeStatement (VariableMutationStatement _ (Identifier _ variableName) variableValue) = do
  encodedVariableValue <- encodeExpression variableValue
  variableIndex <- getVariableIndex variableName
  return $ encodedVariableValue <> mutateVariableInstruction (fromIntegral variableIndex)
encodeStatement (ExpressionStatement _ expression) = do
  encodedExpression <- encodeExpression expression
  return $ encodedExpression <> popInstruction
encodeStatement (WhileLoopStatement d condition body) = do
  encodedCondition <- encodeExpression condition
  let conditionBytestring = BB.toLazyByteString encodedCondition
  -- The body of a while loop statement is implicitly an expression statement, not just an expresison
  encodedBody <- encodeStatement (ExpressionStatement d body)
  let bodyBytestring = BB.toLazyByteString encodedBody
  return $
    BB.lazyByteString conditionBytestring
      <> jumpIfFalseInstruction (fromIntegral (LB.length bodyBytestring + jumpInstructionNumBytes))
      <> BB.lazyByteString bodyBytestring
      <> jumpInstruction (fromIntegral (-(LB.length bodyBytestring + jumpIfFalseInstructionNumBytes + LB.length conditionBytestring + jumpIfFalseInstructionNumBytes)))
encodeStatement (ReturnStatement _ (Just expression)) = do
  encodedExpression <- encodeExpression expression
  return $ encodedExpression <> returnInstruction
encodeStatement (ReturnStatement _ Nothing) = return $ nilInstruction <> returnInstruction

encodeExpression :: TCExpression -> BytecodeGenerator BB.Builder
encodeExpression (IntLiteralExpression _ value) = return $ intInstruction $ fromIntegral value
encodeExpression (FloatLiteralExpression _ value) = return $ floatInstruction value
encodeExpression (CharLiteralExpression _ value) = return $ charInstruction value
encodeExpression (StringLiteralExpression _ value) = do
  index <- addConstant (StringConstant value)
  return $ constantInstruction (fromIntegral index)
encodeExpression (BoolLiteralExpression _ True) = return trueInstruction
encodeExpression (BoolLiteralExpression _ False) = return falseInstruction
encodeExpression (NilExpression _) = return nilInstruction
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
-- TODO: Add short-circuiting for and/or
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
encodeExpression (VariableExpression _ variableName) = pushIdentifierValue variableName
encodeExpression (IfThenElseExpression _ condition trueExpression maybeFalseExpression) = do
  encodedCondition <- encodeExpression condition
  encodedTrueExpression <- encodeExpression trueExpression
  let trueExpressionBytestring = BB.toLazyByteString encodedTrueExpression
  encodedFalseExpression <- case maybeFalseExpression of
    Just falseExpression -> encodeExpression falseExpression
    Nothing -> encodeExpression $ NilExpression (TCExpresionData dummyRange NilType NeverReturns)
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
encodeExpression (FunctionExpression _ (TCFunctionExpressionContent functionIndex capturedIdentifiers)) = do
  pushIdentifierValues <- traverse pushIdentifierValue capturedIdentifiers
  return $ fold pushIdentifierValues <> functionInstruction (fromIntegral functionIndex) (fromIntegral . length $ capturedIdentifiers)
encodeExpression (FunctionCallExpression _ function arguments) = do
  encodedFunction <- encodeExpression function
  encodedArguments <- traverse encodeExpression arguments
  return $ encodedFunction <> fold encodedArguments <> callInstruction (fromIntegral . length $ arguments)

pushIdentifierValue :: TCIdentifier -> BytecodeGenerator BB.Builder
pushIdentifierValue (Identifier _ variableName) = do
  index <- getVariableIndex variableName
  return $ readVariableInstruction (fromIntegral index)