module Lib
  ( run,
    runCode,
    compileCode,
  )
where

import BytecodeGeneration.BytecodeGenerator
import Core.ErrorIO
import Core.Errors
import Core.Utils
import qualified Data.ByteString.Builder as BB
import Data.Text (Text)
import qualified Data.Text as Text
import FunctionLifting.FunctionLifter
import IdentifierBinding.IdentifierBinder
import Lexing.Lexer
import Parsing.Parser
import Sectioning.Sectioner
import System.Directory
import System.Exit
import System.Process
import TypeChecking.TypeChecker

debug :: Bool
debug = True

standardBytecodeFilePath :: FilePath
standardBytecodeFilePath = "../byte.code"

standardCode :: Text
standardCode = "print fib[6]; func fib[x: Int]: Int -> if x <= 1 then x else fib[x - 1] + fib[x - 2];"

run :: IO ()
run = runAndOutputErrors $ do
  output <- runCode debugLog standardBytecodeFilePath standardCode
  liftIO $ putStrLn output

debugLog :: String -> ErrorIO ()
debugLog s = if debug then liftIO . putStrLn $ s else return ()

runCode :: (String -> ErrorIO ()) -> FilePath -> Text.Text -> ErrorIO String
runCode logger bytecodeFilePath code = do
  bytecode <- compileCode logger code
  runVM logger bytecodeFilePath bytecode

compileCode :: (String -> ErrorIO ()) -> Text.Text -> ErrorIO BB.Builder
compileCode logger code = do
  tokens <- liftWithErrors $ lexText code
  logger "Completed lexing"
  logger $ pretty tokens
  sections <- liftWithErrors $ sectionFile tokens
  logger "Completed sectioning"
  logger $ pretty sections
  pAst <- liftWithErrors $ parseFile sections
  logger "Completed parsing"
  logger $ pretty pAst
  (boundValueIdentifierCounter, boundFunctionIdentifierCounter, ibAst) <- liftWithErrors $ runIdentifierBinding pAst
  logger "Completed identifier binding"
  logger $ pretty ibAst
  (recordFieldOrders, tcAst) <- liftWithErrors $ runTypeChecking ibAst
  logger "Completed type checking"
  logger $ pretty tcAst
  flAst <- liftWithErrors $ runFunctionLifting boundValueIdentifierCounter boundFunctionIdentifierCounter recordFieldOrders tcAst
  logger "Completed function lifting"
  logger $ pretty flAst
  let bytecode = encodeFile flAst
  logger "Completed bytecode generation"
  logger $ show $ BB.toLazyByteString bytecode
  return bytecode

runVM :: (String -> ErrorIO ()) -> FilePath -> BB.Builder -> ErrorIO String
runVM logger bytecodeFilePath bytecode = do
  liftIO $ BB.writeFile bytecodeFilePath bytecode
  logger $ "Output bytecode to " ++ bytecodeFilePath
  vmResult <- liftIO $ readProcessWithExitCode "../virtual-machine/target/debug/virtual-machine" [bytecodeFilePath] ""
  logger "Ran VM"
  liftIO $ removeFile bytecodeFilePath
  logger "Deleted bytecode file"
  liftWithErrors $ case vmResult of
    (ExitSuccess, stdOut, _) -> Success stdOut
    (ExitFailure errorCode, _, stdErr) -> singleError $ RuntimeError errorCode stdErr