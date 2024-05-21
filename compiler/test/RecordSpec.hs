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
  describe "Record unions" $ do
    it "Record unions can be deconstructed with case expressions" $ do
      "rec Foo []; rec Bar []; let x: Foo | Bar = Foo; print case x of [Foo: p -> \"foo\", Bar: p -> \"bar\"];" `runsSuccessfullyWithOutput` "foo\n"
      "rec Foo []; rec Bar []; let x: Foo | Bar = Bar; print case x of [Foo: p -> \"foo\", Bar: p -> \"bar\"];" `runsSuccessfullyWithOutput` "bar\n"
    it "Record fields can be accessed inside of a case expression" $ do
      "rec Foo [a: Int]; rec Bar [b: Int]; let x: Foo | Bar = Foo[a = 1]; print case x of [Foo: p -> p.a, Bar: p -> p.b];" `runsSuccessfullyWithOutput` "1\n"
      "rec Foo [a: Int]; rec Bar [b: Int]; let x: Foo | Bar = Bar[b = 2]; print case x of [Foo: p -> p.a, Bar: p -> p.b];" `runsSuccessfullyWithOutput` "2\n"
    it "Non-matching cases of case expressions are not evaluated" $ do
      "rec Foo []; rec Bar []; let x: Foo | Bar = Foo; case x of [Foo: p -> { print \"foo\"; }, Bar: p -> { print \"bar\"; }];" `runsSuccessfullyWithOutput` "foo\n"
      "rec Foo []; rec Bar []; let x: Foo | Bar = Bar; case x of [Foo: p -> { print \"foo\"; }, Bar: p -> { print \"bar\"; }];" `runsSuccessfullyWithOutput` "bar\n"
    it "If all records of a record union share a field type, that field can be accessed" $ do
      "rec Foo [ a: Int ]; rec Bar [ a: Int ]; let x: Foo | Bar = Foo [a = 1]; print x.a;" `runsSuccessfullyWithOutput` "1\n"
      "rec Foo [ a: Int ]; rec Bar [ a: Int ]; let x: Foo | Bar = Bar [a = 2]; print x.a;" `runsSuccessfullyWithOutput` "2\n"
    it "Cases may have different types if they are all record types" $ do
      "rec Foo [ a: Int ]; rec Bar [ a: Int ]; let x: Foo | Bar = Foo [a = 0]; let y = case x of [Foo: p -> Foo[a = p.a + 1], Bar: p -> Bar[a = p.a + 2]]; print y.a;" `runsSuccessfullyWithOutput` "1\n"
      "rec Foo [ a: Int ]; rec Bar [ a: Int ]; let x: Foo | Bar = Bar [a = 0]; let y = case x of [Foo: p -> Foo[a = p.a + 1], Bar: p -> Bar[a = p.a + 2]]; print y.a;" `runsSuccessfullyWithOutput` "2\n"
    it "Fields of a union type can be accessed even if the field is at a different index in the component records" $ do
      "rec Foo [ b: Int ]; rec Bar [ a: Int, b: Int ]; let x: Foo | Bar = Foo [b = 1]; print x.b;" `runsSuccessfullyWithOutput` "1\n"
      "rec Foo [ b: Int ]; rec Bar [ a: Int, b: Int ]; let x: Foo | Bar = Bar [a = 1, b = 2]; print x.b;" `runsSuccessfullyWithOutput` "2\n"
  describe "Record union errors" $ do
    it "A case expression cannot have multiple cases for the same record" $
      "rec Foo []; rec Bar []; let x: Foo | Bar = Foo; print case x of [Foo: p -> 1, Bar: p -> 2, Foo: p -> 3];" `failsToCompileWithError` caseExpressionDuplicatedCasesError
    it "A field of a record union cannot be accessed if the field is not on all the component records" $
      "rec Foo [a: Int]; rec Bar [b: Int]; let x: Foo | Bar = Foo[a = 1]; print x.a;" `failsToCompileWithError` accessedFieldNotInRecordError
    it "Non-record types cannot be used in unions" $
      "rec Foo [a: Int]; let x: Foo | Int = Foo[a = 1];" `failsToCompileWithError` nonRecordTypeInUnionError
    it "If a field of a record union has different possible non-record types, that field cannot be accessed" $
      "rec Foo [a: Int]; rec Bar [a: Char]; let x: Foo | Bar = Foo[a = 1]; print x.a;" `failsToCompileWithError` fieldTypesAreNotCompatibleError
    it "The switch of a case expression must be of a record union type" $
      "rec Foo []; rec Bar []; print case 10 of [Foo: p -> \"foo\", Bar: p -> \"bar\"];" `failsToCompileWithError` caseSwitchHasNonRecordTypeError
    it "A case expression must include cases for all possible record types the switch could be" $
      "rec Foo []; rec Bar []; let x: Foo | Bar = Foo; print case x of [Foo: p -> \"foo\"];" `failsToCompileWithError` caseExpressionMisingCaseError
    it "A case expression not included cases for record types the switch cannot be" $
      "rec Foo []; rec Bar []; let x: Foo = Foo; print case x of [Foo: p -> \"foo\", Bar: p -> \"bar\"];" `failsToCompileWithError` caseExpressionExtraneousCaseError
    it "The case values of a case expression cannot be different if they are non-record types" $
      "rec Foo []; rec Bar []; let x: Foo | Bar = Foo; print case x of [Foo: p -> \"foo\", Bar: p -> 3];" `failsToCompileWithError` caseTypesAreNotCompatibleError

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

caseExpressionDuplicatedCasesError :: Error -> Bool
caseExpressionDuplicatedCasesError (CaseExpressionDuplicatedCasesError {}) = True
caseExpressionDuplicatedCasesError _ = False

nonRecordTypeInUnionError :: Error -> Bool
nonRecordTypeInUnionError (NonRecordTypeInUnionError {}) = True
nonRecordTypeInUnionError _ = False

fieldTypesAreNotCompatibleError :: Error -> Bool
fieldTypesAreNotCompatibleError (FieldTypesAreNotCompatibleError {}) = True
fieldTypesAreNotCompatibleError _ = False

caseSwitchHasNonRecordTypeError :: Error -> Bool
caseSwitchHasNonRecordTypeError (CaseSwitchHasNonRecordTypeError {}) = True
caseSwitchHasNonRecordTypeError _ = False

caseExpressionMisingCaseError :: Error -> Bool
caseExpressionMisingCaseError (CaseExpressionMisingCaseError {}) = True
caseExpressionMisingCaseError _ = False

caseExpressionExtraneousCaseError :: Error -> Bool
caseExpressionExtraneousCaseError (CaseExpressionExtraneousCaseError {}) = True
caseExpressionExtraneousCaseError _ = False

caseTypesAreNotCompatibleError :: Error -> Bool
caseTypesAreNotCompatibleError (CaseTypesAreNotCompatibleError {}) = True
caseTypesAreNotCompatibleError _ = False
