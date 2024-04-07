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
        ModuloInstruction
      ),
    Value (Value),
    encodeChunk,
  )
where

import qualified Data.ByteString.Builder as BB
import Data.Foldable
import Data.Int
import Data.Sequence (Seq)

data Chunk = Chunk
  { code :: Seq Instruction,
    constants :: Seq Value
  }
  deriving (Show)

encodeChunk :: Chunk -> BB.Builder
encodeChunk (Chunk code constants) =
  BB.int32BE (fromIntegral $ length constants)
    <> fold (BB.int32BE <$> (\(Value value) -> value) <$> constants)
    <> fold (encodeInstruction <$> code)

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
  deriving (Show)

type ConstIndex = Int8

encodeInstruction :: Instruction -> BB.Builder
encodeInstruction ReturnInstruction = BB.int8 1
encodeInstruction PrintInstruction = BB.int8 2
encodeInstruction (ConstantInstruction constIndex) = BB.int8 3 <> BB.int8 constIndex
encodeInstruction NegateInstruction = BB.int8 4
encodeInstruction AddInstruction = BB.int8 5
encodeInstruction SubtractInstruction = BB.int8 6
encodeInstruction MultiplyInstruction = BB.int8 7
encodeInstruction DivideInstruction = BB.int8 8
encodeInstruction ModuloInstruction = BB.int8 9

data Value = Value Int32
  deriving (Show)

encodeValue :: Value -> BB.Builder
encodeValue (Value value) = BB.int32BE value