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
        FalseInstruction
      ),
    Value (IntValue, DoubleValue),
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
    <> fold (encodeValue <$> constants)
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
encodeInstruction NotInstruction = BB.int8 10
encodeInstruction AndInstruction = BB.int8 11
encodeInstruction OrInstruction = BB.int8 12
encodeInstruction EqualInstruction = BB.int8 13
encodeInstruction NotEqualInstruction = BB.int8 14
encodeInstruction GreaterInstruction = BB.int8 15
encodeInstruction LessInstruction = BB.int8 16
encodeInstruction GreaterEqualInstruction = BB.int8 17
encodeInstruction LessEqualInstruction = BB.int8 18
encodeInstruction TrueInstruction = BB.int8 19
encodeInstruction FalseInstruction = BB.int8 20

data Value = IntValue Int32 | DoubleValue Double
  deriving (Show)

encodeValue :: Value -> BB.Builder
encodeValue (IntValue value) = BB.int8 1 <> BB.int32BE value
encodeValue (DoubleValue value) = BB.int8 2 <> BB.doubleBE value