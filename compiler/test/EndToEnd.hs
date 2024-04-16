module EndToEnd (runsSuccessfullyWithOutput) where

import Core.Errors
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