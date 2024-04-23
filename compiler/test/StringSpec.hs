module StringSpec
  ( testStrings,
  )
where

import EndToEnd
import Test.Hspec

testStrings :: Spec
testStrings = do
  describe "Strings" $ do
    it "Chars are successfully stored as constants and retrieved" $
      "print 'z';" `runsSuccessfullyWithOutput` "z\n"
    it "Strings are successfully stored as constants and retrieved" $
      "print \"this is a test.\";" `runsSuccessfullyWithOutput` "this is a test.\n"
    it "Strings can be stored in variables and retrieved" $
      "let s1 = \"string1\"; let s2 = \"string2\"; print s1; print s2;" `runsSuccessfullyWithOutput` "string1\nstring2\n"
    it "Strings can be concatenated" $
      "let s1 = \"string1\"; let s2 = \"string2\"; print s1 + s2;" `runsSuccessfullyWithOutput` "string1string2\n"
    it "Strings can be concatenated with characters" $ do
      "let s = \"string\"; let c = 'x'; print s + c;" `runsSuccessfullyWithOutput` "stringx\n"
      "let s = \"string\"; let c = 'x'; print c + s;" `runsSuccessfullyWithOutput` "xstring\n"