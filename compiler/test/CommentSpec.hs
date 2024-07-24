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
      "print⟨String⟩[\"Hello\"]; // prints \"Hello\" " `runsSuccessfullyWithOutput` "Hello"
    it "Comments started with // end at the end of a line" $
      "printLine⟨String⟩[\"Hello\"]; // prints \"Hello\"\n printLine⟨String⟩[\"World\"]; " `runsSuccessfullyWithOutput` "Hello\nWorld\n"