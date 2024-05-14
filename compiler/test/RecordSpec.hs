module RecordSpec
  ( testRecords,
  )
where

import Core.Errors
import EndToEnd
import Test.Hspec

testRecords :: Spec
testRecords = do
  describe "Records" $ do
    it "Records can be defined and used" $
      "rec Foo [a: Int, b: Float]; let foo = Foo[a = 1, b = 2.3]; print foo.a; print foo.b;" `runsSuccessfullyWithOutput` "1\n2.3\n"
    it "Records fields can be listed in any orer" $
      "rec Foo [a: Int, b: Float]; let foo = Foo[b = 2.3, a = 1]; print foo.a; print foo.b;" `runsSuccessfullyWithOutput` "1\n2.3\n"
    it "Records can have no fields" $
      "rec Nothing []; let foo = Nothing;" `runsSuccessfullyWithOutput` ""
    it "Records can be used in types" $
      "rec Nothing []; let foo: Nothing = Nothing;" `runsSuccessfullyWithOutput` ""
    it "Records can be used in functions" $
      "rec Pair [first: Int, second: Int]; func mult[p: Pair]: Int -> p.first * p.second; print mult[Pair[first = 3, second = 5]];" `runsSuccessfullyWithOutput` "15\n"
    it "Records can be returned from functions" $
      "rec Pair [first: Int, second: Int]; func dup[x: Int]: Pair -> Pair[first = x, second = x]; let p = dup[3]; print p.first; print p.second;" `runsSuccessfullyWithOutput` "3\n3\n"
    it "Records field values are evaluated based on the field order in their definition" $
      "rec Foo [a: Int, b: Int, c: Int]; func bar[x: Int]: Int -> { print x; return x; }; Foo[b = bar[2], c=bar[3], a=bar[1]];" `runsSuccessfullyWithOutput` "1\n2\n3\n"
  describe "Record errors" $ do
    it "Multiple records with the same name cannot be created in the same scope" $
      "rec Foo [a: Int]; rec Foo[b: Int];" `failsToCompileWithError` conflictingIdentifierDefinitionsError
    it "Records cannot be shadowed by another record in a nested scope" $
      "rec Foo [a: Int]; { rec Foo[b: Int]; };" `failsToCompileWithError` identifierConflictsWithRecordError
    it "Variables cannot take the name of a record that is in the same scope" $
      "rec Foo [a: Int]; let Foo = 3;" `failsToCompileWithError` conflictingIdentifierDefinitionsError
    it "Variables cannot take the name of a record that is in a surrounding scope" $
      "rec Foo [a: Int]; { let Foo = 3; };" `failsToCompileWithError` identifierConflictsWithRecordError
    {- TODO [BUG-1]: This test currently fails because if parsing the field list fails, we end up parsing the record name as an
      identifier expression. This ends up eating the recordExpressionConflictingFieldsError, and we end up with an
      unbound error for the expresison. There are some standalone fixes for this issue, for example adding a lookahead
      to the identifier expression parser to check there's nothing that looks like a field list immediately after.
      However, it may instead be worth replacing usages of parser combinators with a technique that could more reliably
      produce helpful parse errors.
    -}
    -- it "Record expressions cannot have multiple values for the same field" $
    --   "rec Foo [a: Int, b: Float]; let foo = Foo[a = 1, b = 2.3, a = 4];" `failsToCompileWithError` recordExpressionConflictingFieldsError
    it "Record definitions cannot have multiple types for the same field" $
      "rec Foo [a: Int, b: Float, a: Int]; let foo = Foo[a = 1, b = 2.3];" `failsToCompileWithError` recordStatementConflictingFieldsError
    it "Record expressions cannot have fields not defined for the record" $
      "rec Foo [a: Int, b: Float]; let foo = Foo[a = 1, b = 2.3, c  = 4];" `failsToCompileWithError` recordExpressionExtraFieldError
    it "Record expressions must have all fields defined for the record" $
      "rec Foo [a: Int, b: Float]; let foo = Foo[a = 1];" `failsToCompileWithError` recordExpressionMissingFieldError
    it "Record expressions must have all fields values match the defined field type" $
      "rec Foo [a: Int, b: Float]; let foo = Foo[a = 1, b = \"23\"];" `failsToCompileWithError` recordExpressionFieldTypeError
    it "Fields can only be accessed for values of a record type" $
      "rec Foo [a: Int, b: Float]; let notFoo = 5; print notFoo.a;" `failsToCompileWithError` accessedFieldOfNonRecordValueError
    it "Fields that do not exist on a value cannot be accessed" $
      "rec Foo [a: Int, b: Float]; let foo = Foo[a = 1, b = 2.3]; print foo.c;" `failsToCompileWithError` accessedFieldNotInRecordError

conflictingIdentifierDefinitionsError :: Error -> Bool
conflictingIdentifierDefinitionsError (ConflictingIdentifierDefinitionsError {}) = True
conflictingIdentifierDefinitionsError _ = False

identifierConflictsWithRecordError :: Error -> Bool
identifierConflictsWithRecordError (IdentifierConflictsWithRecordError {}) = True
identifierConflictsWithRecordError _ = False

-- recordExpressionConflictingFieldsError :: Error -> Bool
-- recordExpressionConflictingFieldsError (RecordExpressionConflictingFieldsError {}) = True
-- recordExpressionConflictingFieldsError _ = False

recordStatementConflictingFieldsError :: Error -> Bool
recordStatementConflictingFieldsError (RecordStatementConflictingFieldsError {}) = True
recordStatementConflictingFieldsError _ = False

recordExpressionExtraFieldError :: Error -> Bool
recordExpressionExtraFieldError (RecordExpressionExtraFieldError {}) = True
recordExpressionExtraFieldError _ = False

recordExpressionMissingFieldError :: Error -> Bool
recordExpressionMissingFieldError (RecordExpressionMissingFieldError {}) = True
recordExpressionMissingFieldError _ = False

recordExpressionFieldTypeError :: Error -> Bool
recordExpressionFieldTypeError (RecordExpressionFieldTypeError {}) = True
recordExpressionFieldTypeError _ = False

accessedFieldOfNonRecordValueError :: Error -> Bool
accessedFieldOfNonRecordValueError (AccessedFieldOfNonRecordValueError {}) = True
accessedFieldOfNonRecordValueError _ = False

accessedFieldNotInRecordError :: Error -> Bool
accessedFieldNotInRecordError (AccessedFieldNotInRecordError {}) = True
accessedFieldNotInRecordError _ = False