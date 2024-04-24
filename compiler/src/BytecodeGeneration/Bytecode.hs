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
    jumpIfFalseInstruction,
    intInstruction,
    doubleInstruction,
    Constant (StringConstant),
    encodeConstant,
    charInstruction,
  )
where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as C
import Data.Int
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word16)

type ConstIndex = Word16

type StackIndex = Word16

type InstructionOffset = Int16

-- Instructions
returnInstruction :: BB.Builder
returnInstruction = BB.int8 1

printInstruction :: BB.Builder
printInstruction = BB.int8 2

constantInstruction :: ConstIndex -> BB.Builder
constantInstruction constIndex = BB.int8 3 <> BB.word16BE constIndex

negateInstruction :: BB.Builder
negateInstruction = BB.int8 4

addInstruction :: BB.Builder
addInstruction = BB.int8 5

subtractInstruction :: BB.Builder
subtractInstruction = BB.int8 6

multiplyInstruction :: BB.Builder
multiplyInstruction = BB.int8 7

divideInstruction :: BB.Builder
divideInstruction = BB.int8 8

moduloInstruction :: BB.Builder
moduloInstruction = BB.int8 9

notInstruction :: BB.Builder
notInstruction = BB.int8 10

andInstruction :: BB.Builder
andInstruction = BB.int8 11

orInstruction :: BB.Builder
orInstruction = BB.int8 12

equalInstruction :: BB.Builder
equalInstruction = BB.int8 13

notEqualInstruction :: BB.Builder
notEqualInstruction = BB.int8 14

greaterInstruction :: BB.Builder
greaterInstruction = BB.int8 15

lessInstruction :: BB.Builder
lessInstruction = BB.int8 16

greaterEqualInstruction :: BB.Builder
greaterEqualInstruction = BB.int8 17

lessEqualInstruction :: BB.Builder
lessEqualInstruction = BB.int8 18

trueInstruction :: BB.Builder
trueInstruction = BB.int8 19

falseInstruction :: BB.Builder
falseInstruction = BB.int8 20

readVariableInstruction :: StackIndex -> BB.Builder
readVariableInstruction stackIndex = BB.int8 21 <> BB.word16BE stackIndex

mutateVariableInstruction :: StackIndex -> BB.Builder
mutateVariableInstruction stackIndex = BB.int8 22 <> BB.word16BE stackIndex

nilInstruction :: BB.Builder
nilInstruction = BB.int8 23

popInstruction :: BB.Builder
popInstruction = BB.int8 24

popMultipleInstruction :: StackIndex -> BB.Builder
popMultipleInstruction numValuesToPop = BB.int8 25 <> BB.word16BE numValuesToPop

jumpInstruction :: InstructionOffset -> BB.Builder
jumpInstruction bytesToJump = BB.int8 26 <> BB.int16BE bytesToJump

jumpIfFalseInstruction :: InstructionOffset -> BB.Builder
jumpIfFalseInstruction bytesToJump = BB.int8 27 <> BB.int16BE bytesToJump

intInstruction :: Int32 -> BB.Builder
intInstruction value = BB.int8 28 <> BB.int32BE value

doubleInstruction :: Double -> BB.Builder
doubleInstruction value = BB.int8 29 <> BB.doubleBE value

charInstruction :: Char -> BB.Builder
charInstruction value = BB.int8 30 <> BB.byteString encodedValue <> padding
  where
    encodedValue = C.singleton value
    -- A UTF-8 character is 1-4 bytes, so we pad with zero bytes to get a fixed instruction length
    padding = mconcat $ replicate (4 - C.length encodedValue) (BB.int8 0)

-- Constants
data Constant
  = StringConstant Text
  deriving (Show)

encodeConstant :: Constant -> BB.Builder
encodeConstant (StringConstant value) = BB.int8 1 <> BB.word16BE (fromIntegral $ B.length encoded) <> BB.byteString encoded
  where
    encoded = TE.encodeUtf8 value