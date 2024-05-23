module BytecodeGeneration.Bytecode
  ( returnInstruction,
    printInstruction,
    constantInstruction,
    negateInstruction,
    addInstruction,
    subtractInstruction,
    multiplyInstruction,
    divideInstruction,
    moduloInstruction,
    notInstruction,
    andInstruction,
    orInstruction,
    equalInstruction,
    notEqualInstruction,
    greaterInstruction,
    lessInstruction,
    greaterEqualInstruction,
    lessEqualInstruction,
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

type ConstIndex = Word16

type StackIndex = Word16

type InstructionOffset = Int16

type FunctionIndex = Word16

type RecordId = Word16

type FieldIndex = Word8

-- Instructions
returnInstruction :: BB.Builder
returnInstruction = BB.word8 1

printInstruction :: BB.Builder
printInstruction = BB.word8 2

constantInstruction :: ConstIndex -> BB.Builder
constantInstruction constIndex = BB.word8 3 <> BB.word16BE constIndex

negateInstruction :: BB.Builder
negateInstruction = BB.word8 4

addInstruction :: BB.Builder
addInstruction = BB.word8 5

subtractInstruction :: BB.Builder
subtractInstruction = BB.word8 6

multiplyInstruction :: BB.Builder
multiplyInstruction = BB.word8 7

divideInstruction :: BB.Builder
divideInstruction = BB.word8 8

moduloInstruction :: BB.Builder
moduloInstruction = BB.word8 9

notInstruction :: BB.Builder
notInstruction = BB.word8 10

andInstruction :: BB.Builder
andInstruction = BB.word8 11

orInstruction :: BB.Builder
orInstruction = BB.word8 12

equalInstruction :: BB.Builder
equalInstruction = BB.word8 13

notEqualInstruction :: BB.Builder
notEqualInstruction = BB.word8 14

greaterInstruction :: BB.Builder
greaterInstruction = BB.word8 15

lessInstruction :: BB.Builder
lessInstruction = BB.word8 16

greaterEqualInstruction :: BB.Builder
greaterEqualInstruction = BB.word8 17

lessEqualInstruction :: BB.Builder
lessEqualInstruction = BB.word8 18

trueInstruction :: BB.Builder
trueInstruction = BB.word8 19

falseInstruction :: BB.Builder
falseInstruction = BB.word8 20

readVariableInstruction :: StackIndex -> BB.Builder
readVariableInstruction stackIndex = BB.word8 21 <> BB.word16BE stackIndex

mutateVariableInstruction :: StackIndex -> BB.Builder
mutateVariableInstruction stackIndex = BB.word8 22 <> BB.word16BE stackIndex

nilInstruction :: BB.Builder
nilInstruction = BB.word8 23

popInstruction :: BB.Builder
popInstruction = BB.word8 24

popMultipleInstruction :: StackIndex -> BB.Builder
popMultipleInstruction numValuesToPop = BB.word8 25 <> BB.word16BE numValuesToPop

jumpInstruction :: InstructionOffset -> BB.Builder
jumpInstruction bytesToJump = BB.word8 26 <> BB.int16BE bytesToJump

jumpInstructionNumBytes :: Int64
jumpInstructionNumBytes = 3

jumpIfFalseInstruction :: InstructionOffset -> BB.Builder
jumpIfFalseInstruction bytesToJump = BB.word8 27 <> BB.int16BE bytesToJump

jumpIfFalseInstructionNumBytes :: Int64
jumpIfFalseInstructionNumBytes = 3

intInstruction :: Int32 -> BB.Builder
intInstruction value = BB.word8 28 <> BB.int32BE value

floatInstruction :: Double -> BB.Builder
floatInstruction value = BB.word8 29 <> BB.doubleBE value

charInstruction :: Char -> BB.Builder
charInstruction value = BB.word8 30 <> BB.byteString encodedValue <> padding
  where
    encodedValue = C.singleton value
    -- A UTF-8 character is 1-4 bytes, so we pad with zero bytes to get a fixed instruction length
    padding = mconcat $ replicate (4 - C.length encodedValue) (BB.word8 0)

functionInstruction :: FunctionIndex -> Word8 -> BB.Builder
functionInstruction functionIndex numCapturedIdentifiers = BB.word8 31 <> BB.word16BE functionIndex <> BB.word8 numCapturedIdentifiers

callInstruction :: Word8 -> BB.Builder
callInstruction numArguments = BB.word8 32 <> BB.word8 numArguments

recordInstruction :: RecordId -> Word8 -> BB.Builder
recordInstruction recordId numFields = BB.word8 33 <> BB.word16BE recordId <> BB.word8 numFields

fieldInstruction :: FieldIndex -> BB.Builder
fieldInstruction fieldIndex = BB.word8 34 <> BB.word8 fieldIndex

jumpIfDoesntMatchRecordIdInstruction :: RecordId -> InstructionOffset -> BB.Builder
jumpIfDoesntMatchRecordIdInstruction recordId offset = BB.word8 35 <> BB.word16BE recordId <> BB.int16BE offset

removeFromStackInstruction :: StackIndex -> BB.Builder
removeFromStackInstruction stackIndex = BB.word8 36 <> BB.word16BE stackIndex

mutateFieldInstruction :: FieldIndex -> BB.Builder
mutateFieldInstruction fieldIndex = BB.word8 37 <> BB.word8 fieldIndex

-- Constants
data Constant
  = StringConstant Text
  deriving (Show)

encodeConstant :: Constant -> BB.Builder
encodeConstant (StringConstant value) = BB.word8 1 <> BB.word16BE (fromIntegral $ B.length encoded) <> BB.byteString encoded
  where
    encoded = TE.encodeUtf8 value