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
    Constant (IntConstant, DoubleConstant, CharConstant, StringConstant),
  )
where

import Data.Int
import Data.Sequence (Seq)
import Data.Text (Text)

data Chunk = Chunk
  { code :: Seq Instruction,
    constants :: Seq Constant
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

data Constant
  = IntConstant Int32
  | DoubleConstant Double
  | CharConstant Char
  | StringConstant Text
  deriving (Show)