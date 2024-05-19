module BytecodeGeneration.BytecodeGeneration
  ( BytecodeGenerator,
    BytecodeGeneratorState (BytecodeGeneratorState, constants),
    addConstant,
    getVariableIndex,
    initialState,
    addVariable,
    initializeFunction,
    getStackSize,
    setStackSize,
    adjustStackSize,
  )
where

import BytecodeGeneration.Bytecode
import Control.Monad.State (State, get, put)
import Data.Foldable (foldl')
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Sequence (Seq (Empty), (|>))
import qualified Data.Sequence as Seq
import IdentifierBinding.SyntaxTree

data BytecodeGeneratorState = BytecodeGeneratorState
  { identifierIndexMap :: Map ValueIdentifierIndex Int,
    stackSize :: Int,
    constants :: Seq Constant
  }

type BytecodeGenerator a = State BytecodeGeneratorState a

initialState :: BytecodeGeneratorState
initialState = BytecodeGeneratorState {identifierIndexMap = Map.empty, stackSize = 0, constants = Empty}

addConstant :: Constant -> BytecodeGenerator Int
addConstant value = do
  BytecodeGeneratorState {constants} <- get
  setConstants $ constants |> value
  return $ length constants

addVariable :: ValueIdentifierIndex -> BytecodeGenerator ()
addVariable newVariable = do
  BytecodeGeneratorState {identifierIndexMap, stackSize} <- get
  setIdentifierIndexMap $ Map.insert newVariable (stackSize - 1) identifierIndexMap

initializeFunction :: Seq ValueIdentifierIndex -> BytecodeGenerator ()
initializeFunction parameters = do
  BytecodeGeneratorState {identifierIndexMap} <- get
  let (updatedIdentifierIndexMap, _) = foldl' (\(indexMap, index) parameter -> (Map.insert parameter index indexMap, index + 1)) (identifierIndexMap, 0) parameters
  setIdentifierIndexMap updatedIdentifierIndexMap
  setStackSize $ Seq.length parameters

getVariableIndex :: ValueIdentifierIndex -> BytecodeGenerator Int
getVariableIndex variable = do
  BytecodeGeneratorState {identifierIndexMap} <- get
  return $ identifierIndexMap ! variable

getStackSize :: BytecodeGenerator Int
getStackSize = stackSize <$> get

adjustStackSize :: Int -> BytecodeGenerator ()
adjustStackSize adjustment = do
  BytecodeGeneratorState {stackSize} <- get
  setStackSize $ stackSize + adjustment

setIdentifierIndexMap :: Map ValueIdentifierIndex Int -> BytecodeGenerator ()
setIdentifierIndexMap identifierIndexMap = do
  state <- get
  put state {identifierIndexMap}

setStackSize :: Int -> BytecodeGenerator ()
setStackSize stackSize = do
  state <- get
  put state {stackSize}

setConstants :: Seq Constant -> BytecodeGenerator ()
setConstants constants = do
  state <- get
  put state {constants}