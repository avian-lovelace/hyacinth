module BytecodeGeneration.Bytecode
  ( returnInstruction,
    builtInFunctionInstruction,
    constantInstruction,
    trueInstruction,
    falseInstruction,
    readVariableInstruction,
    mutateVariableInstruction,
    nilInstruction,
    popInstruction,
    popMultipleInstruction,
    jumpInstruction,
    jumpInstructionNumBytes,
    jumpIfFalseInstruction,
    jumpIfFalseInstructionNumBytes,
    intInstruction,
    floatInstruction,
    charInstruction,
    functionInstruction,
    callInstruction,
    recordInstruction,
    fieldInstruction,
    jumpIfDoesntMatchRecordIdInstruction,
    removeFromStackInstruction,
    mutateFieldInstruction,
    Constant (StringConstant),
    encodeConstant,
  )
where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as C
import Data.Int
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word16, Word8)
import IntermediateCodeGeneration.IntermediateCode (BuiltInFn (..))

type ConstIndex = Word16

type StackIndex = Word16

type InstructionOffset = Int16

type FunctionIndex = Word16

type RecordId = Word16

type FieldIndex = Word8

-- Instructions
returnInstruction :: BB.Builder
returnInstruction = BB.word8 1

builtInFunctionInstruction :: BuiltInFn -> BB.Builder
builtInFunctionInstruction builtInFn = BB.word8 2 <> BB.word8 functionId
  where
    functionId = case builtInFn of
      NegateFn -> 1
      AddFn -> 2
      SubtractFn -> 3
      MultiplyFn -> 4
      DivideFn -> 5
      ModuloFn -> 6
      NotFn -> 7
      EqualFn -> 8
      NotEqualFn -> 9
      GreaterFn -> 10
      LessFn -> 11
      GreaterEqualFn -> 12
      LessEqualFn -> 13
      PrintFn -> 14
      PrintLineFn -> 15

constantInstruction :: ConstIndex -> BB.Builder
constantInstruction constIndex = BB.word8 3 <> BB.word16BE constIndex

nilInstruction :: BB.Builder
nilInstruction = BB.word8 4

trueInstruction :: BB.Builder
trueInstruction = BB.word8 5

falseInstruction :: BB.Builder
falseInstruction = BB.word8 6

intInstruction :: Int32 -> BB.Builder
intInstruction value = BB.word8 7 <> BB.int32BE value

floatInstruction :: Double -> BB.Builder
floatInstruction value = BB.word8 8 <> BB.doubleBE value

charInstruction :: Char -> BB.Builder
charInstruction value = BB.word8 9 <> BB.byteString encodedValue <> padding
  where
    encodedValue = C.singleton value
    -- A UTF-8 character is 1-4 bytes, so we pad with zero bytes to get a fixed instruction length
    padding = mconcat $ replicate (4 - C.length encodedValue) (BB.word8 0)

popInstruction :: BB.Builder
popInstruction = BB.word8 10

popMultipleInstruction :: StackIndex -> BB.Builder
popMultipleInstruction numValuesToPop = BB.word8 11 <> BB.word16BE numValuesToPop

removeFromStackInstruction :: StackIndex -> BB.Builder
removeFromStackInstruction stackIndex = BB.word8 12 <> BB.word16BE stackIndex

jumpInstruction :: InstructionOffset -> BB.Builder
jumpInstruction bytesToJump = BB.word8 13 <> BB.int16BE bytesToJump

jumpInstructionNumBytes :: Int64
jumpInstructionNumBytes = 3

jumpIfFalseInstruction :: InstructionOffset -> BB.Builder
jumpIfFalseInstruction bytesToJump = BB.word8 14 <> BB.int16BE bytesToJump

jumpIfFalseInstructionNumBytes :: Int64
jumpIfFalseInstructionNumBytes = 3

readVariableInstruction :: StackIndex -> BB.Builder
readVariableInstruction stackIndex = BB.word8 15 <> BB.word16BE stackIndex

mutateVariableInstruction :: StackIndex -> BB.Builder
mutateVariableInstruction stackIndex = BB.word8 16 <> BB.word16BE stackIndex

functionInstruction :: FunctionIndex -> Word8 -> BB.Builder
functionInstruction functionIndex numCapturedIdentifiers = BB.word8 17 <> BB.word16BE functionIndex <> BB.word8 numCapturedIdentifiers

callInstruction :: Word8 -> BB.Builder
callInstruction numArguments = BB.word8 18 <> BB.word8 numArguments

recordInstruction :: RecordId -> Word8 -> BB.Builder
recordInstruction recordId numFields = BB.word8 19 <> BB.word16BE recordId <> BB.word8 numFields

fieldInstruction :: FieldIndex -> BB.Builder
fieldInstruction fieldIndex = BB.word8 20 <> BB.word8 fieldIndex

mutateFieldInstruction :: FieldIndex -> BB.Builder
mutateFieldInstruction fieldIndex = BB.word8 21 <> BB.word8 fieldIndex

jumpIfDoesntMatchRecordIdInstruction :: RecordId -> InstructionOffset -> BB.Builder
jumpIfDoesntMatchRecordIdInstruction recordId offset = BB.word8 22 <> BB.word16BE recordId <> BB.int16BE offset

-- Constants
data Constant
  = StringConstant Text
  deriving (Show)

encodeConstant :: Constant -> BB.Builder
encodeConstant (StringConstant value) = BB.word8 1 <> BB.word16BE (fromIntegral $ B.length encoded) <> BB.byteString encoded
  where
    encoded = TE.encodeUtf8 value