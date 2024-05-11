module BytecodeGeneration.BytecodeGeneration
  ( BytecodeGenerator (BytecodeGenerator, runGenerator),
    BytecodeGeneratorState (BytecodeGeneratorState, constants),
    addConstant,
    addVariableToScope,
    getVariableIndex,
    withNewScope,
    withFunctionScope,
    initialState,
  )
where

import BytecodeGeneration.Bytecode
import qualified Data.ByteString.Builder as BB
import Data.Foldable (traverse_)
import Data.Sequence (Seq (Empty, (:<|), (:|>)), elemIndexL, (|>))
import qualified Data.Sequence as Seq
import IdentifierBinding.SyntaxTree

data BytecodeGeneratorState = BytecodeGeneratorState
  { scopes :: Seq Scope,
    constants :: Seq Constant
  }

newtype Scope = Scope {variables :: Seq BoundValueIdentifier}

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

withFunctionScope :: Seq BoundValueIdentifier -> Seq BoundValueIdentifier -> BytecodeGenerator BB.Builder -> BytecodeGenerator BB.Builder
withFunctionScope parameters capturedIdentifiers generator = do
  pushNewScope
  traverse_ addVariableToScope parameters
  traverse_ addVariableToScope capturedIdentifiers
  result <- generator
  _ <- popScope
  return result

pushNewScope :: BytecodeGenerator ()
pushNewScope = BytecodeGenerator $ \BytecodeGeneratorState {scopes, constants} ->
  (BytecodeGeneratorState {scopes = scopes |> Scope {variables = Empty}, constants}, ())

popScope :: BytecodeGenerator Scope
popScope = BytecodeGenerator $ \BytecodeGeneratorState {scopes, constants} -> case scopes of
  initScopes :|> finalScope -> (BytecodeGeneratorState {scopes = initScopes, constants}, finalScope)
  Empty -> undefined -- Should not get here

addVariableToScope :: BoundValueIdentifier -> BytecodeGenerator ()
addVariableToScope newVariable = BytecodeGenerator run
  where
    run (BytecodeGeneratorState {scopes = initScopes :|> (Scope {variables}), constants}) =
      (BytecodeGeneratorState {scopes = initScopes |> Scope {variables = variables |> newVariable}, constants}, ())
    run _ = undefined

getVariableIndex :: BoundValueIdentifier -> BytecodeGenerator Int
getVariableIndex variable = BytecodeGenerator $ \state -> (state, fromIntegral $ getVariableIndexInScopes $ scopes state)
  where
    getVariableIndexInScopes ((Scope {variables}) :<| tailScopes) = case elemIndexL variable variables of
      Just i -> i
      Nothing -> length variables + getVariableIndexInScopes tailScopes
    {- Should not get here, as the identifier binding step checks if an identifier is used while not in scope. This is
      undefined rather an ShouldNotGetHereError, as bytecode generation shouldn't throw errors unless there is a
      compiler but, so it doesn't use WithErrors.
    -}
    getVariableIndexInScopes _ = undefined