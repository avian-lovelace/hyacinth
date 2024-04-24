module BytecodeGeneration.BytecodeGeneration
  ( BytecodeGenerator (BytecodeGenerator, runGenerator),
    BytecodeGeneratorState (BytecodeGeneratorState, constants),
    addConstant,
    addVariableToScope,
    getVariableIndex,
    withNewScope,
    initialState,
  )
where

import BytecodeGeneration.Bytecode
import qualified Data.ByteString.Builder as BB
import Data.Sequence (Seq (Empty, (:<|), (:|>)), elemIndexL, (|>))
import qualified Data.Sequence as Seq
import VariableBinding.SyntaxTree

data BytecodeGeneratorState = BytecodeGeneratorState
  { scopes :: Seq Scope,
    constants :: Seq Constant
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

initialState :: BytecodeGeneratorState
initialState = BytecodeGeneratorState {scopes = Empty, constants = Empty}

addConstant :: Constant -> BytecodeGenerator Int
addConstant value = BytecodeGenerator $ \(BytecodeGeneratorState {scopes, constants}) ->
  (BytecodeGeneratorState {scopes, constants = constants |> value}, fromIntegral $ length constants)

withNewScope :: BytecodeGenerator BB.Builder -> BytecodeGenerator BB.Builder
withNewScope generator = do
  pushNewScope
  result <- generator
  endedScope <- popScope
  let popLocalVariables = popMultipleInstruction $ fromIntegral . Seq.length . variables $ endedScope
  return $ result <> popLocalVariables <> nilInstruction

pushNewScope :: BytecodeGenerator ()
pushNewScope = BytecodeGenerator $ \BytecodeGeneratorState {scopes, constants} ->
  (BytecodeGeneratorState {scopes = scopes |> Scope {variables = Empty}, constants}, ())

popScope :: BytecodeGenerator Scope
popScope = BytecodeGenerator $ \BytecodeGeneratorState {scopes, constants} -> case scopes of
  initScopes :|> finalScope -> (BytecodeGeneratorState {scopes = initScopes, constants}, finalScope)
  Empty -> undefined -- Should not get here

addVariableToScope :: BoundIdentifier -> BytecodeGenerator ()
addVariableToScope newVariable = BytecodeGenerator run
  where
    run (BytecodeGeneratorState {scopes = initScopes :|> (Scope {variables}), constants}) =
      (BytecodeGeneratorState {scopes = initScopes |> Scope {variables = variables |> newVariable}, constants}, ())
    run _ = undefined

getVariableIndex :: BoundIdentifier -> BytecodeGenerator Int
getVariableIndex variable = BytecodeGenerator $ \state -> (state, fromIntegral $ getVariableIndexInScopes $ scopes state)
  where
    getVariableIndexInScopes ((Scope {variables}) :<| tailScopes) = case elemIndexL variable variables of
      Just i -> i
      Nothing -> length variables + getVariableIndexInScopes tailScopes
    getVariableIndexInScopes _ = undefined