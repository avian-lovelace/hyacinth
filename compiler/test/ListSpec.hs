module ListSpec
  ( testLists,
  )
where

import Core.Errors
import EndToEnd
import Test.Hspec

testLists :: Spec
testLists = do
  describe "Lists:" $ do
    it "Lists can be created and indexed into" $
      "let foo = List⟨Int⟩[3, 5]; printLine⟨Int⟩[foo#0]; printLine⟨Int⟩[foo#1];" `runsSuccessfullyWithOutput` "3\n5\n"
    it "The value type of a list can be generated" $
      "let foo = List[3, 5]; printLine⟨Int⟩[foo#0]; printLine⟨Int⟩[foo#1];" `runsSuccessfullyWithOutput` "3\n5\n"
    it "The value type of a list can be generated with unions" $
      "let foo = List[Foo, Bar]; rec Foo = []; rec Bar = [];" `runsSuccessfullyWithOutput` ""
    it "The value type of a list can be inferred with unions" $
      "let foo: List⟨Foo | Bar⟩ = List[Foo]; rec Foo = []; rec Bar = [];" `runsSuccessfullyWithOutput` ""
  describe "List errors:" $ do
    it "The value type of an empty list cannot be generated" $
      "let foo = List[]" `failsToCompileWithError` listValueTypeInferenceError
    it "A list cannot contain incompatible types" $
      "let foo = List[\"Bar\", 3]" `failsToCompileWithError` listValueTypeInferenceError
    it "A list literal cannot be used for a non-list type" $
      "let foo: Int = List[3, 4]" `failsToCompileWithError` listTypeError
    it "Non-list types cannot indexed into" $
      "let foo = 5; foo#3;" `failsToCompileWithError` indexTypeError

listValueTypeInferenceError :: Error -> Bool
listValueTypeInferenceError (ListValueTypeInferenceError {}) = True
listValueTypeInferenceError _ = False

listTypeError :: Error -> Bool
listTypeError (ListTypeError {}) = True
listTypeError _ = False

indexTypeError :: Error -> Bool
indexTypeError (IndexTypeError {}) = True
indexTypeError _ = False