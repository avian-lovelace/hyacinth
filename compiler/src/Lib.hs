module Lib
  ( run,
    VMResult,
    runCode,
  )
where

import BytecodeGeneration.Bytecode
import BytecodeGeneration.BytecodeGenerator
import Core.Errors
import Core.Utils
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LB
import Data.Foldable (toList)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Lexing.Lexer
import Lexing.Tokens
import Parsing.Parser
import Parsing.Parsing
import System.Directory
import System.Environment (getArgs)
-- import Turtle

import System.Exit
import System.Process

run :: IO ()
run = do
  (exitCode, stdOut, stdErr) <- readProcessWithExitCode "../virtual-machine/target/debug/virtual-machine" [] ""
  print $ case exitCode of
    ExitSuccess -> stdOut
    ExitFailure _ -> stdErr

runCode :: Text.Text -> IO (WithError VMResult)
runCode code = case compileCode code of
  Success bytecode -> do
    let bytecodeFilePath = "../byte.code"
    writeBytecodeToFile bytecodeFilePath bytecode
    vmResult <- runVM bytecodeFilePath
    removeFile bytecodeFilePath
    return $ Success vmResult
  Error e -> return $ Error e

compileCode :: Text.Text -> WithError Chunk
compileCode code = do
  tokens <- lexText code
  ast <- snd $ runParser fileParser tokens
  let bytecode = writeFileScope ast
  return bytecode

writeBytecodeToFile :: FilePath -> Chunk -> IO ()
writeBytecodeToFile filePath bytecode = BB.writeFile filePath (encodeChunk bytecode)

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

lexFile :: FilePath -> IO ()
lexFile filePath = do
  file <- Text.IO.readFile filePath
  let result = lexText file
  case result of
    Error err -> putStrLn $ pretty err
    Success tokens -> do
      printTokens $ toList tokens

printTokens :: [Token] -> IO ()
printTokens tokens = sequence_ $ map (putStrLn . pretty) tokens