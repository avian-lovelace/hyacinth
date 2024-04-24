module EndToEnd (runsSuccessfullyWithOutput, failsToCompileWithError) where

import Core.Errors
import Core.Utils
import qualified Data.Sequence as Seq
import Data.Text (Text)
import GHC.IO.Exception
import Lib
import Test.Hspec

runsSuccessfullyWithOutput :: Text -> String -> Expectation
runsSuccessfullyWithOutput code expected = do
  runResult <- runCode code
  case runResult of
    Error e -> expectationFailure $ "Compilation failed with error: " ++ show e
    Success (ExitFailure _, _, stdErr) -> expectationFailure $ "Rumtime failed with error: " ++ stdErr
    Success (ExitSuccess, stdOut, _) -> stdOut `shouldBe` expected

failsToCompileWithError :: Text -> (Error -> Bool) -> Expectation
code `failsToCompileWithError` matchError = case compileCode code of
  Success _ -> expectationFailure "Compilation unexpectedly succeeded"
  Error es ->
    let numErrors = Seq.length es
     in if numErrors == 1
          then (seqHead es) `shouldSatisfy` matchError
          else expectationFailure $ "Expected to get one compilation error but got " ++ show numErrors