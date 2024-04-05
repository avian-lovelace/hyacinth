module Lib
  ( run
  )
where

import Core.Utils
import Core.Errors
import Lexing.Tokens
import Lexing.Lexer
import Parsing.Parser
import Parsing.Parsing
import BytecodeGeneration.BytecodeGenerator
import BytecodeGeneration.Bytecode

import Data.Foldable(toList)
import System.Environment(getArgs)

import qualified Data.Text.IO as Text.IO
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB



run :: IO ()
run = do
  let code = "print -5 * 3 + 2; print 3 * (5 - 2);"
  let Success tokens = lexText code
  print $ show tokens
  let Success ast = snd $ runParser fileParser tokens
  print $ show ast
  let bytecode = writeFileScope ast
  print $ show bytecode
  BB.writeFile ("../code.byte") (encodeChunk bytecode)
  

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