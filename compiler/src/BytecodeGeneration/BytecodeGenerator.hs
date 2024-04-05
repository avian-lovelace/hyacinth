module BytecodeGeneration.BytecodeGenerator (
  writeFileScope
) where

import BytecodeGeneration.Bytecode
import Parsing.SyntaxTree

import Data.Sequence(Seq(Empty, (:<|), (:|>)))
import Data.Foldable
import Data.Function
import Control.Arrow

writeInstruction instruction (Chunk instructions values) = Chunk (instructions :|> instruction) values

writeConstant value (Chunk instructions values) = Chunk instructions (values :|> value)

writeFileScope :: FileScope -> Chunk
writeFileScope (FileScope statements) = foldl' (flip writeStatement) initialChunk statements
  & writeInstruction ReturnInstruction
  where initialChunk = Chunk Empty Empty

writeStatement :: Statement -> Chunk -> Chunk
writeStatement (PrintStatement _ expression) = 
    writeExpression expression >>>
    writeInstruction PrintInstruction

writeExpression :: Expression -> Chunk -> Chunk
writeExpression (ParenthesesExpression _ innerExpression) = writeExpression innerExpression
writeExpression (NegateExpression _ innerExpression) =
    writeExpression innerExpression >>>
    writeInstruction NegateInstruction
writeExpression (AddExpression _ leftExpression rightExpression) =
    writeExpression leftExpression >>>
    writeExpression rightExpression >>>
    writeInstruction AddInstruction
writeExpression (SubtractExpression _ leftExpression rightExpression) =
    writeExpression leftExpression >>>
    writeExpression rightExpression >>>
    writeInstruction SubtractInstruction
writeExpression (MultiplyExpression _ leftExpression rightExpression) =
    writeExpression leftExpression >>>
    writeExpression rightExpression >>>
    writeInstruction MultiplyInstruction
writeExpression (DivideExpression _ leftExpression rightExpression) =
    writeExpression leftExpression >>>
    writeExpression rightExpression >>>
    writeInstruction DivideInstruction
writeExpression (IntLiteralExpression _ value) = \chunk -> chunk &
    (writeConstant (Value (fromIntegral value)) >>>
    writeInstruction (ConstantInstruction (fromIntegral $ length $ constants chunk)))