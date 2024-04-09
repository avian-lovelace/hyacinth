module BytecodeGeneration.BytecodeGenerator
  ( writeFileScope,
  )
where

import BytecodeGeneration.Bytecode
import Control.Arrow
import Data.Foldable
import Data.Function
import Data.Sequence (Seq (Empty, (:|>)))
import Parsing.SyntaxTree

writeInstruction :: Instruction -> Chunk -> Chunk
writeInstruction instruction (Chunk instructions values) = Chunk (instructions :|> instruction) values

writeConstant :: Value -> Chunk -> Chunk
writeConstant value (Chunk instructions values) = Chunk instructions (values :|> value)

writeFileScope :: FileScope -> Chunk
writeFileScope (FileScope statements) =
  foldl' (flip writeStatement) initialChunk statements
    & writeInstruction ReturnInstruction
  where
    initialChunk = Chunk Empty Empty

writeStatement :: Statement -> Chunk -> Chunk
writeStatement (PrintStatement _ expression) =
  writeExpression expression
    >>> writeInstruction PrintInstruction

writeExpression :: Expression -> Chunk -> Chunk
writeExpression (IntLiteralExpression _ value) = \chunk ->
  chunk
    & ( writeConstant (IntValue (fromIntegral value))
          >>> writeInstruction (ConstantInstruction (fromIntegral $ length $ constants chunk))
      )
writeExpression (DoubleLiteralExpression _ value) = \chunk ->
  chunk
    & ( writeConstant (DoubleValue value)
          >>> writeInstruction (ConstantInstruction (fromIntegral $ length $ constants chunk))
      )
writeExpression (BoolLiteralExpression _ True) = writeInstruction TrueInstruction
writeExpression (BoolLiteralExpression _ False) = writeInstruction FalseInstruction
writeExpression (NegateExpression _ innerExpression) =
  writeExpression innerExpression
    >>> writeInstruction NegateInstruction
writeExpression (AddExpression _ leftExpression rightExpression) =
  writeExpression leftExpression
    >>> writeExpression rightExpression
    >>> writeInstruction AddInstruction
writeExpression (SubtractExpression _ leftExpression rightExpression) =
  writeExpression leftExpression
    >>> writeExpression rightExpression
    >>> writeInstruction SubtractInstruction
writeExpression (MultiplyExpression _ leftExpression rightExpression) =
  writeExpression leftExpression
    >>> writeExpression rightExpression
    >>> writeInstruction MultiplyInstruction
writeExpression (DivideExpression _ leftExpression rightExpression) =
  writeExpression leftExpression
    >>> writeExpression rightExpression
    >>> writeInstruction DivideInstruction
writeExpression (ModuloExpression _ leftExpression rightExpression) =
  writeExpression leftExpression
    >>> writeExpression rightExpression
    >>> writeInstruction ModuloInstruction
writeExpression (NotExpression _ innerExpression) =
  writeExpression innerExpression
    >>> writeInstruction NotInstruction
writeExpression (AndExpression _ leftExpression rightExpression) =
  writeExpression leftExpression
    >>> writeExpression rightExpression
    >>> writeInstruction AndInstruction
writeExpression (OrExpression _ leftExpression rightExpression) =
  writeExpression leftExpression
    >>> writeExpression rightExpression
    >>> writeInstruction OrInstruction
writeExpression (EqualExpression _ leftExpression rightExpression) =
  writeExpression leftExpression
    >>> writeExpression rightExpression
    >>> writeInstruction EqualInstruction
writeExpression (NotEqualExpression _ leftExpression rightExpression) =
  writeExpression leftExpression
    >>> writeExpression rightExpression
    >>> writeInstruction NotEqualInstruction
writeExpression (GreaterExpression _ leftExpression rightExpression) =
  writeExpression leftExpression
    >>> writeExpression rightExpression
    >>> writeInstruction GreaterInstruction
writeExpression (LessExpression _ leftExpression rightExpression) =
  writeExpression leftExpression
    >>> writeExpression rightExpression
    >>> writeInstruction LessInstruction
writeExpression (GreaterEqualExpression _ leftExpression rightExpression) =
  writeExpression leftExpression
    >>> writeExpression rightExpression
    >>> writeInstruction GreaterEqualInstruction
writeExpression (LessEqualExpression _ leftExpression rightExpression) =
  writeExpression leftExpression
    >>> writeExpression rightExpression
    >>> writeInstruction LessEqualInstruction