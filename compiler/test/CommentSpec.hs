module CommentSpec
  ( testComments,
  )
where

import EndToEnd
import Test.Hspec

testComments :: Spec
testComments = do
  describe "Comments:" $ do
    it "Anything on a line after // is ignored" $
      "print \"Hello\"; // prints \"Hello\" " `runsSuccessfullyWithOutput` "Hello\n"
    it "Comments started with // end at the end of a line" $
      "print \"Hello\"; // prints \"Hello\"\n print \"World\"; " `runsSuccessfullyWithOutput` "Hello\nWorld\n"