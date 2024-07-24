module Lib
  ( run,
    runCode,
    compileCode,
  )
where

import BytecodeGeneration.BytecodeGenerator
import Control.Monad (forM_, when)
import Core.EitherIO
import Core.Errors
import Core.Utils
import Data.Bifunctor (first)
import qualified Data.ByteString.Builder as BB
import Data.Foldable (Foldable (toList))
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import IdentifierBinding.IdentifierBinder
import IntermediateCodeGeneration.IntermediateCodeGenerator
import Lexing.Lexer
import Options.Applicative
import Parsing.Parser
import Sectioning.Sectioner
import System.Directory
import System.Exit
import System.FilePath ((<.>))
import qualified System.FilePath as FP
import System.Process
import TypeChecking.TypeChecker

run :: IO ()
run = do
  options <- execParser compilerOptionsParserInfo
  result <- runEitherIO $ runCompiler options
  case result of
    Left es -> do
      putStrLn $ "Compilation failed with " ++ show (length es) ++ " error:"
      forM_ es $ \errorMessage -> do
        putStrLn ""
        putStrLn errorMessage
    Right () -> return ()

codeFileExtension :: String
codeFileExtension = ".hyc"

bytecodeFileExtension :: String
bytecodeFileExtension = "hyb"

data CompilerOptions = CompilerOptions
  { inputFilePath :: FilePath,
    outputFilePath :: Maybe FilePath,
    debug :: Bool
  }

compilerOptionsParser :: Parser CompilerOptions
compilerOptionsParser = do
  inputFilePath <-
    argument inputReader $
      metavar "input_file"
        <> help "Path to the Hyacinth code file to be compiled"
  outputFilePath <-
    optional $
      argument str $
        metavar "output_file"
          <> help "Path where the output bytecode file will be written"
  debug <-
    switch $
      long "debug"
        <> short 'd'
        <> help "Enable additional logging during compilation"
  pure $ CompilerOptions {inputFilePath, outputFilePath, debug}
  where
    inputReader = eitherReader $ \inputFilePath ->
      if FP.takeExtension inputFilePath == codeFileExtension
        then return inputFilePath
        else Left $ "Error: The input file must be a .hyc file, but got " ++ inputFilePath

compilerOptionsParserInfo :: ParserInfo CompilerOptions
compilerOptionsParserInfo =
  info
    (compilerOptionsParser <**> helper)
    ( fullDesc
        <> progDesc
          ( unlines
              [ "This is the compiler for Hyacinth, a language created by Robin Gieseking. For more information",
                "on Hyacinth, check out the documentation at github.com/avian-lovelace/hyacinth#readme"
              ]
          )
        <> header "The Hyacinth Compiler"
    )

runCompiler :: CompilerOptions -> EitherIO [String] ()
runCompiler (CompilerOptions {inputFilePath, outputFilePath, debug}) = do
  let defaultOutputFilePath = FP.dropExtension inputFilePath <.> bytecodeFileExtension
  let finalOutputFilePath = fromMaybe defaultOutputFilePath outputFilePath
  code <- liftIO $ TextIO.readFile inputFilePath
  let logger = when debug . putStrLn
  bytecode <- first (fmap show . toList) $ compileCode logger code
  liftIO $ BB.writeFile finalOutputFilePath bytecode

compileCode :: (String -> IO ()) -> Text -> EitherIO (Seq Error) BB.Builder
compileCode logger code = do
  tokens <- liftWithErrors $ lexText code
  liftIO . logger $ "Completed lexing"
  liftIO . logger $ pretty tokens
  sections <- liftWithErrors $ sectionFile tokens
  liftIO . logger $ "Completed sectioning"
  liftIO . logger $ pretty sections
  pAst <- liftWithErrors $ parseFile sections
  liftIO . logger $ "Completed parsing"
  liftIO . logger $ pretty pAst
  (boundValueIdentifierCounter, boundFunctionIdentifierCounter, ibAst) <- liftWithErrors $ runIdentifierBinding pAst
  liftIO . logger $ "Completed identifier binding"
  liftIO . logger $ pretty ibAst
  (recordFieldOrders, tcAst) <- liftWithErrors $ runTypeChecking ibAst
  liftIO . logger $ "Completed type checking"
  liftIO . logger $ pretty tcAst
  flAst <- liftWithErrors $ runIntermediateCodeGeneration boundValueIdentifierCounter boundFunctionIdentifierCounter recordFieldOrders tcAst
  liftIO . logger $ "Completed function lifting"
  liftIO . logger $ show flAst
  let bytecode = encodeFile flAst
  liftIO . logger $ "Completed bytecode generation"
  liftIO . logger $ show $ BB.toLazyByteString bytecode
  return bytecode

runCode :: (String -> IO ()) -> FilePath -> Text -> EitherIO (Seq Error) String
runCode logger bytecodeFilePath code = do
  bytecode <- compileCode logger code
  runVM logger bytecodeFilePath bytecode

runVM :: (String -> IO ()) -> FilePath -> BB.Builder -> EitherIO (Seq Error) String
runVM logger bytecodeFilePath bytecode = do
  liftIO $ BB.writeFile bytecodeFilePath bytecode
  liftIO . logger $ "Output bytecode to " ++ bytecodeFilePath
  vmResult <- liftIO $ readProcessWithExitCode "../virtual-machine/target/debug/hyacinth-vm" [bytecodeFilePath] ""
  liftIO . logger $ "Ran VM"
  liftIO $ removeFile bytecodeFilePath
  liftIO . logger $ "Deleted bytecode file"
  liftWithErrors $ case vmResult of
    (ExitSuccess, stdOut, _) -> return stdOut
    (ExitFailure errorCode, _, stdErr) -> singleError $ RuntimeError errorCode stdErr