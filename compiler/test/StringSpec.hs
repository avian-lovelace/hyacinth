module StringSpec
  ( testStrings,
  )
where

import EndToEnd
import Test.Hspec

testStrings :: Spec
testStrings = do
  describe "Strings:" $ do
    it "Chars are successfully stored as constants and retrieved" $
      "print⟨Char⟩['z'];" `runsSuccessfullyWithOutput` "z"
    it "Strings are successfully stored as constants and retrieved" $
      "print⟨String⟩[\"this is a test.\"];" `runsSuccessfullyWithOutput` "this is a test."
    it "Strings can be stored in variables and retrieved" $
      "let s1 = \"string1\"; let s2 = \"string2\"; printLine⟨String⟩[s1]; printLine⟨String⟩[s2];" `runsSuccessfullyWithOutput` "string1\nstring2\n"
    it "Strings can be concatenated" $
      "let s1 = \"string1\"; let s2 = \"string2\"; print⟨String⟩[s1 + s2];" `runsSuccessfullyWithOutput` "string1string2"
    it "Strings can be concatenated with characters" $ do
      "let s = \"string\"; let c = 'x'; print⟨String⟩[s + c];" `runsSuccessfullyWithOutput` "stringx"
      "let s = \"string\"; let c = 'x'; print⟨String⟩[c + s];" `runsSuccessfullyWithOutput` "xstring"