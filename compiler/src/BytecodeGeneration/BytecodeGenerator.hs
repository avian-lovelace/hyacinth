module BytecodeGeneration.BytecodeGenerator
  ( writeFileScope,
  )
where

import BytecodeGeneration.Bytecode
import BytecodeGeneration.BytecodeGeneration
import Core.SyntaxTree
import Data.Sequence (Seq (Empty))
import VariableBinding.SyntaxTree

writeFileScope :: VBFileScope -> Chunk
writeFileScope (FileScope _ statements) = chunk finalState
  where
    initialState = BytecodeGeneratorState {scopes = [Scope {variables = Empty}], chunk = Chunk {code = Empty, constants = Empty}}
    fileGenerator = mapM_ writeStatement statements
    (finalState, _) = runGenerator fileGenerator initialState

writeStatement :: VBStatement -> BytecodeGenerator ()
writeStatement (PrintStatement _ expression) = do
  writeExpression expression
  writeInstruction PrintInstruction
writeStatement (VariableDeclarationStatement _ (VariableName _ variableName) variableValue) = do
  addVariableToScope variableName
  writeExpression variableValue
writeStatement (VariableMutationStatement _ (VariableName _ variableName) variableValue) = do
  writeExpression variableValue
  variableIndex <- getVariableIndex variableName
  writeInstruction $ MutateVariableInstruction variableIndex

writeExpression :: VBExpression -> BytecodeGenerator ()
writeExpression (IntLiteralExpression _ value) = do
  index <- writeConstant (IntValue (fromIntegral value))
  writeInstruction (ConstantInstruction index)
writeExpression (DoubleLiteralExpression _ value) = do
  index <- writeConstant (DoubleValue value)
  writeInstruction (ConstantInstruction index)
writeExpression (BoolLiteralExpression _ True) = writeInstruction TrueInstruction
writeExpression (BoolLiteralExpression _ False) = writeInstruction FalseInstruction
writeExpression (NegateExpression _ innerExpression) = do
  writeExpression innerExpression
  writeInstruction NegateInstruction
writeExpression (AddExpression _ leftExpression rightExpression) = do
  writeExpression leftExpression
  writeExpression rightExpression
  writeInstruction AddInstruction
writeExpression (SubtractExpression _ leftExpression rightExpression) = do
  writeExpression leftExpression
  writeExpression rightExpression
  writeInstruction SubtractInstruction
writeExpression (MultiplyExpression _ leftExpression rightExpression) = do
  writeExpression leftExpression
  writeExpression rightExpression
  writeInstruction MultiplyInstruction
writeExpression (DivideExpression _ leftExpression rightExpression) = do
  writeExpression leftExpression
  writeExpression rightExpression
  writeInstruction DivideInstruction
writeExpression (ModuloExpression _ leftExpression rightExpression) = do
  writeExpression leftExpression
  writeExpression rightExpression
  writeInstruction ModuloInstruction
writeExpression (NotExpression _ innerExpression) = do
  writeExpression innerExpression
  writeInstruction NotInstruction
writeExpression (AndExpression _ leftExpression rightExpression) = do
  writeExpression leftExpression
  writeExpression rightExpression
  writeInstruction AndInstruction
writeExpression (OrExpression _ leftExpression rightExpression) = do
  writeExpression leftExpression
  writeExpression rightExpression
  writeInstruction OrInstruction
writeExpression (EqualExpression _ leftExpression rightExpression) = do
  writeExpression leftExpression
  writeExpression rightExpression
  writeInstruction EqualInstruction
writeExpression (NotEqualExpression _ leftExpression rightExpression) = do
  writeExpression leftExpression
  writeExpression rightExpression
  writeInstruction NotEqualInstruction
writeExpression (GreaterExpression _ leftExpression rightExpression) = do
  writeExpression leftExpression
  writeExpression rightExpression
  writeInstruction GreaterInstruction
writeExpression (LessExpression _ leftExpression rightExpression) = do
  writeExpression leftExpression
  writeExpression rightExpression
  writeInstruction LessInstruction
writeExpression (GreaterEqualExpression _ leftExpression rightExpression) = do
  writeExpression leftExpression
  writeExpression rightExpression
  writeInstruction GreaterEqualInstruction
writeExpression (LessEqualExpression _ leftExpression rightExpression) = do
  writeExpression leftExpression
  writeExpression rightExpression
  writeInstruction LessEqualInstruction
writeExpression (VariableExpression _ (VariableName _ variableName)) = do
  index <- getVariableIndex variableName
  writeInstruction (ReadVariableInstruction index)