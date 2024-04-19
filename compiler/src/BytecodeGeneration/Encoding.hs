module BytecodeGeneration.Encoding
  ( Encodable,
    encode,
  )
where

import BytecodeGeneration.Bytecode
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as C
import qualified Data.Text.Encoding as TE

class Encodable e where
  encode :: e -> BB.Builder

instance Encodable Chunk where
  encode (Chunk code constants) =
    BB.int32BE (fromIntegral $ length constants)
      <> foldMap encode constants
      <> foldMap encode code

instance Encodable Instruction where
  encode :: Instruction -> BB.Builder
  encode ReturnInstruction = BB.int8 1
  encode PrintInstruction = BB.int8 2
  encode (ConstantInstruction constIndex) = BB.int8 3 <> BB.int8 constIndex
  encode NegateInstruction = BB.int8 4
  encode AddInstruction = BB.int8 5
  encode SubtractInstruction = BB.int8 6
  encode MultiplyInstruction = BB.int8 7
  encode DivideInstruction = BB.int8 8
  encode ModuloInstruction = BB.int8 9
  encode NotInstruction = BB.int8 10
  encode AndInstruction = BB.int8 11
  encode OrInstruction = BB.int8 12
  encode EqualInstruction = BB.int8 13
  encode NotEqualInstruction = BB.int8 14
  encode GreaterInstruction = BB.int8 15
  encode LessInstruction = BB.int8 16
  encode GreaterEqualInstruction = BB.int8 17
  encode LessEqualInstruction = BB.int8 18
  encode TrueInstruction = BB.int8 19
  encode FalseInstruction = BB.int8 20
  encode (ReadVariableInstruction stackIndex) = BB.int8 21 <> BB.int8 stackIndex
  encode (MutateVariableInstruction stackIndex) = BB.int8 22 <> BB.int8 stackIndex

instance Encodable Constant where
  encode :: Constant -> BB.Builder
  encode (IntConstant value) = BB.int8 1 <> BB.int32BE value
  encode (DoubleConstant value) = BB.int8 2 <> BB.doubleBE value
  encode (CharConstant value) = BB.int8 3 <> BB.int8 (fromIntegral $ B.length encoded) <> BB.byteString encoded
    where
      encoded = C.singleton value
  encode (StringConstant value) = BB.int8 4 <> BB.int32BE (fromIntegral $ B.length encoded) <> BB.byteString encoded
    where
      encoded = TE.encodeUtf8 value