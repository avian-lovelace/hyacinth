module BytecodeGeneration.BytecodeGeneration
  ( BytecodeGeneratorState
      ( BytecodeGeneratorState,
        scopes,
        chunk
      ),
    Scope (Scope, variables),
    BytecodeGenerator (BytecodeGenerator, runGenerator),
    writeInstruction,
    writeConstant,
    addVariableToScope,
    getVariableIndex,
  )
where

import BytecodeGeneration.Bytecode
import Data.Int (Int8)
import Data.Sequence (Seq ((:<|), (:|>)), elemIndexL, (|>))
import VariableBinding.SyntaxTree

data BytecodeGeneratorState = BytecodeGeneratorState
  { scopes :: Seq Scope,
    chunk :: Chunk
  }

newtype Scope = Scope {variables :: Seq BoundIdentifier}

newtype BytecodeGenerator a = BytecodeGenerator {runGenerator :: BytecodeGeneratorState -> (BytecodeGeneratorState, a)} deriving (Functor)

instance Applicative BytecodeGenerator where
  pure a = BytecodeGenerator $ \state -> (state, a)
  generatorF <*> generatorA = BytecodeGenerator $ \state1 ->
    let (state2, f) = runGenerator generatorF state1
     in let (state3, a) = runGenerator generatorA state2
         in (state3, f a)

instance Monad BytecodeGenerator where
  return = pure
  generatorA >>= makeGeneratorB = BytecodeGenerator $ \state1 ->
    let (state2, a) = runGenerator generatorA state1
     in runGenerator (makeGeneratorB a) state2

writeInstruction :: Instruction -> BytecodeGenerator ()
writeInstruction instruction = BytecodeGenerator $ \(BytecodeGeneratorState {scopes, chunk = Chunk {code, constants}}) ->
  (BytecodeGeneratorState {scopes, chunk = Chunk {code = code |> instruction, constants}}, ())

writeConstant :: Constant -> BytecodeGenerator Int8
writeConstant value = BytecodeGenerator $ \(BytecodeGeneratorState {scopes, chunk = Chunk {code, constants}}) ->
  (BytecodeGeneratorState {scopes, chunk = Chunk {code, constants = constants |> value}}, fromIntegral $ length constants)

addVariableToScope :: BoundIdentifier -> BytecodeGenerator ()
addVariableToScope newVariable = BytecodeGenerator run
  where
    run (BytecodeGeneratorState {scopes = initScopes :|> (Scope {variables}), chunk}) =
      (BytecodeGeneratorState {scopes = initScopes |> Scope {variables = variables |> newVariable}, chunk}, ())
    run _ = undefined

getVariableIndex :: BoundIdentifier -> BytecodeGenerator Int8
getVariableIndex variable = BytecodeGenerator $ \state -> (state, fromIntegral $ getVariableIndexInScopes $ scopes state)
  where
    getVariableIndexInScopes ((Scope {variables}) :<| tailScopes) = case elemIndexL variable variables of
      Just i -> i
      Nothing -> length variables + getVariableIndexInScopes tailScopes
    getVariableIndexInScopes _ = undefined