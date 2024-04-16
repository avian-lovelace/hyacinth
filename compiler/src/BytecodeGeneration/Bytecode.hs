module BytecodeGeneration.Bytecode
  ( Chunk (Chunk, code, constants),
    Instruction
      ( ReturnInstruction,
        PrintInstruction,
        ConstantInstruction,
        NegateInstruction,
        AddInstruction,
        SubtractInstruction,
        MultiplyInstruction,
        DivideInstruction,
        ModuloInstruction,
        NotInstruction,
        AndInstruction,
        OrInstruction,
        EqualInstruction,
        NotEqualInstruction,
        GreaterInstruction,
        LessInstruction,
        GreaterEqualInstruction,
        LessEqualInstruction,
        TrueInstruction,
        FalseInstruction,
        ReadVariableInstruction,
        MutateVariableInstruction
      ),
    Value (IntValue, DoubleValue),
  )
where

import Data.Int
import Data.Sequence (Seq)

data Chunk = Chunk
  { code :: Seq Instruction,
    constants :: Seq Value
  }
  deriving (Show)

data Instruction
  = ReturnInstruction
  | PrintInstruction
  | ConstantInstruction ConstIndex
  | NegateInstruction
  | AddInstruction
  | SubtractInstruction
  | MultiplyInstruction
  | DivideInstruction
  | ModuloInstruction
  | NotInstruction
  | AndInstruction
  | OrInstruction
  | EqualInstruction
  | NotEqualInstruction
  | GreaterInstruction
  | LessInstruction
  | GreaterEqualInstruction
  | LessEqualInstruction
  | TrueInstruction
  | FalseInstruction
  | ReadVariableInstruction StackIndex
  | MutateVariableInstruction StackIndex
  deriving (Show)

type ConstIndex = Int8

type StackIndex = Int8

data Value = IntValue Int32 | DoubleValue Double
  deriving (Show)