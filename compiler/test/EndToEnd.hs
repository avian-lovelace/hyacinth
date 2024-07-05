module EndToEnd (runsSuccessfullyWithOutput, failsToCompileWithError) where

import Core.EitherIO
import Core.Errors
import Core.Utils
import Data.Foldable (fold)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Lib
import Test.Hspec

testLogger :: String -> IO ()
testLogger _ = return ()

testBytecodeFilePath :: FilePath
testBytecodeFilePath = "../byte.code"

runsSuccessfullyWithOutput :: Text -> String -> Expectation
runsSuccessfullyWithOutput code expected = do
  runResult <- runEitherIO $ runCode testLogger testBytecodeFilePath code
  case runResult of
    Left es -> expectationFailure $ fold $ Seq.intersperse "\n" $ pretty <$> es
    Right stdOut -> stdOut `shouldBe` expected

failsToCompileWithError :: Text -> (Error -> Bool) -> Expectation
code `failsToCompileWithError` matchError = do
  result <- runEitherIO $ compileCode testLogger code
  case result of
    Right _ -> expectationFailure "Compilation unexpectedly succeeded"
    Left es ->
      let numErrors = Seq.length es
       in if numErrors == 1
            then seqHead es `shouldSatisfy` matchError
            else expectationFailure $ "Expected to get one compilation error but got " ++ show numErrors