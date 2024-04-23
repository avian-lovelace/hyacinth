module Lib
  ( run,
    VMResult,
    runCode,
  )
where

import BytecodeGeneration.Bytecode
import BytecodeGeneration.BytecodeGenerator
import BytecodeGeneration.Encoding
import Core.Errors
import Core.Utils
import qualified Data.ByteString.Builder as BB
import qualified Data.Text as Text
import Lexing.Lexer
import Parsing.Parser
import Sectioning.Sectioner
import System.Directory
import System.Exit
import System.Process
import VariableBinding.VariableBinder

run :: IO ()
run = case compileCode "let s1 = \"foo\"; let c = ' '; let s2 = \"bar\"; print s1 + c + s2;" of
  Error es -> mapM_ (putStrLn . pretty) es
  Success byteCode -> do
    result <- runByteCode byteCode
    case result of
      (ExitSuccess, stdOut, _) -> putStrLn stdOut
      (ExitFailure _, _, stdErr) -> putStrLn stdErr

runCode :: Text.Text -> IO (WithErrors VMResult)
runCode code = case compileCode code of
  Success bytecode -> do
    let bytecodeFilePath = "../byte.code"
    writeBytecodeToFile bytecodeFilePath bytecode
    vmResult <- runVM bytecodeFilePath
    removeFile bytecodeFilePath
    return $ Success vmResult
  Error e -> return $ Error e

runByteCode :: Chunk -> IO VMResult
runByteCode code = do
  let bytecodeFilePath = "../byte.code"
  writeBytecodeToFile bytecodeFilePath code
  vmResult <- runVM bytecodeFilePath
  removeFile bytecodeFilePath
  return vmResult

compileCode :: Text.Text -> WithErrors Chunk
compileCode code = do
  tokens <- lexText code
  sections <- sectionFile tokens
  pAst <- parseFile sections
  vbAst <- runVariableBinding pAst
  let bytecode = writeFileScope vbAst
  return bytecode

writeBytecodeToFile :: FilePath -> Chunk -> IO ()
writeBytecodeToFile filePath bytecode = BB.writeFile filePath (encode bytecode)

type VMResult = (ExitCode, String, String)

runVM :: FilePath -> IO VMResult
runVM bytecodeFilePath = readProcessWithExitCode "../virtual-machine/target/debug/virtual-machine" [bytecodeFilePath] ""

-- args <- getArgs
-- case length args of
--   0 -> putStrLn "A file name is required"
--   1 -> lexFile $ head args
--   _ -> putStrLn "Too many args"

-- compile :: FilePath -> IO ()
-- compile filePath = do
--   file <- Text.IO.readFile filePath
--   let result = lexText file
--   case result of
--     Error err -> putStrLn $ printError err
--     Success tokens -> do
--       printTokens $ toList tokens
--       case expressionParser $ makeStream tokens of
--         Error err -> putStrLn $ printError err
--         Success (SeqStream remainingTokens _, expression) -> do
--           putStrLn $ show remainingTokens
--           putStrLn $ printExpression expression

-- lexFile :: FilePath -> IO ()
-- lexFile filePath = do
--   file <- Text.IO.readFile filePath
--   let result = lexText file
--   case result of
--     Error err -> putStrLn $ pretty err
--     Success tokens -> do
--       printTokens $ toList tokens

-- printTokens :: [Token] -> IO ()
-- printTokens tokens = sequence_ $ map (putStrLn . pretty) tokens