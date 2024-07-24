module RecordSpec
  ( testRecords,
  )
where

import Core.Errors
import EndToEnd
import Test.Hspec

testRecords :: Spec
testRecords = do
  describe "Records:" $ do
    it "Records can be defined and used" $
      "rec Foo = [a: Int, b: Float]; let foo = Foo[a = 1, b = 2.3]; printLine⟨Int⟩[foo.a]; printLine⟨Float⟩[foo.b];" `runsSuccessfullyWithOutput` "1\n2.3\n"
    it "Records fields can be listed in any orer" $
      "rec Foo = [a: Int, b: Float]; let foo = Foo[b = 2.3, a = 1]; printLine⟨Int⟩[foo.a]; printLine⟨Float⟩[foo.b];" `runsSuccessfullyWithOutput` "1\n2.3\n"
    it "Records can have no fields" $
      "rec Nothing = []; let foo = Nothing;" `runsSuccessfullyWithOutput` ""
    it "Records can be used in types" $
      "rec Nothing = []; let foo: Nothing = Nothing;" `runsSuccessfullyWithOutput` ""
    it "Records can be used in functions" $
      "rec Pair = [first: Int, second: Int]; func mult = [p: Pair]: Int -> p.first * p.second; print⟨Int⟩[mult[Pair[first = 3, second = 5]]];" `runsSuccessfullyWithOutput` "15"
    it "Records can be returned from functions" $
      "rec Pair = [first: Int, second: Int]; func dup = [x: Int]: Pair -> Pair[first = x, second = x]; let p = dup[3]; printLine⟨Int⟩[p.first]; printLine⟨Int⟩[p.second];" `runsSuccessfullyWithOutput` "3\n3\n"
    it "Records field values are evaluated based on the field order in their definition" $
      "rec Foo = [a: Int, b: Int, c: Int]; func bar = [x: Int]: Int -> { printLine⟨Int⟩[x]; return x; }; Foo[b = bar[2], c=bar[3], a=bar[1]];" `runsSuccessfullyWithOutput` "1\n2\n3\n"
  describe "Record errors:" $ do
    it "Multiple records with the same name cannot be created in the same scope" $
      "rec Foo = [a: Int]; rec Foo = [b: Int];" `failsToCompileWithError` conflictingIdentifierDefinitionsError
    it "Records cannot be shadowed by another record in a nested scope" $
      "rec Foo = [a: Int]; { rec Foo = [b: Int]; };" `failsToCompileWithError` identifierConflictsWithRecordError
    it "Variables cannot take the name of a record that is in the same scope" $
      "rec Foo = [a: Int]; let Foo = 3;" `failsToCompileWithError` conflictingIdentifierDefinitionsError
    it "Variables cannot take the name of a record that is in a surrounding scope" $
      "rec Foo = [a: Int]; { let Foo = 3; };" `failsToCompileWithError` identifierConflictsWithRecordError
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
      "rec Foo = [a: Int, b: Float, a: Int]; let foo = Foo[a = 1, b = 2.3];" `failsToCompileWithError` recordStatementConflictingFieldsError
    it "Record expressions cannot have fields not defined for the record" $
      "rec Foo = [a: Int, b: Float]; let foo = Foo[a = 1, b = 2.3, c  = 4];" `failsToCompileWithError` recordExpressionExtraFieldError
    it "Record expressions must have all fields defined for the record" $
      "rec Foo = [a: Int, b: Float]; let foo = Foo[a = 1];" `failsToCompileWithError` recordExpressionMissingFieldError
    it "Record expressions must have all fields values match the defined field type" $
      "rec Foo = [a: Int, b: Float]; let foo = Foo[a = 1, b = \"23\"];" `failsToCompileWithError` typeExpectationError
    it "Fields can only be accessed for values of a record type" $
      "rec Foo = [a: Int, b: Float]; let notFoo = 5; print⟨Int⟩[notFoo.a];" `failsToCompileWithError` accessedFieldOfNonRecordValueError
    it "Fields that do not exist on a value cannot be accessed" $
      "rec Foo = [a: Int, b: Float]; let foo = Foo[a = 1, b = 2.3]; print⟨Int⟩[foo.c];" `failsToCompileWithError` accessedFieldNotInRecordError
  describe "Record unions:" $ do
    it "Record unions can be deconstructed with case expressions" $ do
      "rec Foo = []; rec Bar = []; let x: Foo | Bar = Foo; print⟨String⟩[case x of [Foo: p -> \"foo\", Bar: p -> \"bar\"]];" `runsSuccessfullyWithOutput` "foo"
      "rec Foo = []; rec Bar = []; let x: Foo | Bar = Bar; print⟨String⟩[case x of [Foo: p -> \"foo\", Bar: p -> \"bar\"]];" `runsSuccessfullyWithOutput` "bar"
    it "Record fields can be accessed inside of a case expression" $ do
      "rec Foo = [a: Int]; rec Bar = [b: Int]; let x: Foo | Bar = Foo[a = 1]; print⟨Int⟩[case x of [Foo: p -> p.a, Bar: p -> p.b]];" `runsSuccessfullyWithOutput` "1"
      "rec Foo = [a: Int]; rec Bar = [b: Int]; let x: Foo | Bar = Bar[b = 2]; print⟨Int⟩[case x of [Foo: p -> p.a, Bar: p -> p.b]];" `runsSuccessfullyWithOutput` "2"
    it "Non-matching cases of case expressions are not evaluated" $ do
      "rec Foo = []; rec Bar = []; let x: Foo | Bar = Foo; case x of [Foo: p -> { print⟨String⟩[\"foo\"]; }, Bar: p -> { print⟨String⟩[\"bar\"]; }];" `runsSuccessfullyWithOutput` "foo"
      "rec Foo = []; rec Bar = []; let x: Foo | Bar = Bar; case x of [Foo: p -> { print⟨String⟩[\"foo\"]; }, Bar: p -> { print⟨String⟩[\"bar\"]; }];" `runsSuccessfullyWithOutput` "bar"
    it "If all records of a record union share a field type, that field can be accessed" $ do
      "rec Foo = [ a: Int ]; rec Bar = [ a: Int ]; let x: Foo | Bar = Foo [a = 1]; print⟨Int⟩[x.a];" `runsSuccessfullyWithOutput` "1"
      "rec Foo = [ a: Int ]; rec Bar = [ a: Int ]; let x: Foo | Bar = Bar [a = 2]; print⟨Int⟩[x.a];" `runsSuccessfullyWithOutput` "2"
    it "Cases may have different types if they are all record types" $ do
      "rec Foo = [ a: Int ]; rec Bar = [ a: Int ]; let x: Foo | Bar = Foo [a = 0]; let y = case x of [Foo: p -> Foo[a = p.a + 1], Bar: p -> Bar[a = p.a + 2]]; print⟨Int⟩[y.a];" `runsSuccessfullyWithOutput` "1"
      "rec Foo = [ a: Int ]; rec Bar = [ a: Int ]; let x: Foo | Bar = Bar [a = 0]; let y = case x of [Foo: p -> Foo[a = p.a + 1], Bar: p -> Bar[a = p.a + 2]]; print⟨Int⟩[y.a];" `runsSuccessfullyWithOutput` "2"
    it "Fields of a union type can be accessed even if the field is at a different index in the component records" $ do
      "rec Foo = [ b: Int ]; rec Bar = [ a: Int, b: Int ]; let x: Foo | Bar = Foo [b = 1]; print⟨Int⟩[x.b];" `runsSuccessfullyWithOutput` "1"
      "rec Foo = [ b: Int ]; rec Bar = [ a: Int, b: Int ]; let x: Foo | Bar = Bar [a = 1, b = 2]; print⟨Int⟩[x.b];" `runsSuccessfullyWithOutput` "2"
  describe "Record union errors:" $ do
    it "A case expression cannot have multiple cases for the same record" $
      "rec Foo = []; rec Bar = []; let x: Foo | Bar = Foo; print⟨Int⟩[case x of [Foo: p -> 1, Bar: p -> 2, Foo: p -> 3]];" `failsToCompileWithError` caseExpressionDuplicatedCasesError
    it "A field of a record union cannot be accessed if the field is not on all the component records" $
      "rec Foo = [a: Int]; rec Bar = [b: Int]; let x: Foo | Bar = Foo[a = 1]; print⟨Int⟩[x.a];" `failsToCompileWithError` accessedFieldNotInRecordError
    it "Non-record types cannot be used in unions" $
      "rec Foo = [a: Int]; let x: Foo | Int = Foo[a = 1];" `failsToCompileWithError` variableDeclarationMalformedTypeError
    it "If a field of a record union has different possible non-record types, that field cannot be accessed" $
      "rec Foo = [a: Int]; rec Bar = [a: Char]; let x: Foo | Bar = Foo[a = 1]; let y = x.a;" `failsToCompileWithError` fieldTypesAreNotCompatibleError
    it "The switch of a case expression must be of a record union type" $
      "rec Foo = []; rec Bar = []; print⟨String⟩[case 10 of [Foo: p -> \"foo\", Bar: p -> \"bar\"]];" `failsToCompileWithError` caseSwitchHasNonRecordTypeError
    it "A case expression must include cases for all possible record types the switch could be" $
      "rec Foo = []; rec Bar = []; let x: Foo | Bar = Foo; print⟨String⟩[case x of [Foo: p -> \"foo\"]];" `failsToCompileWithError` caseExpressionMisingCaseError
    it "A case expression not included cases for record types the switch cannot be" $
      "rec Foo = []; rec Bar = []; let x: Foo = Foo; print⟨String⟩[case x of [Foo: p -> \"foo\", Bar: p -> \"bar\"]];" `failsToCompileWithError` caseExpressionExtraneousCaseError
    it "The case values of a case expression cannot be different if they are non-record types" $
      "rec Foo = []; rec Bar = []; let x: Foo | Bar = Foo; let y = case x of [Foo: p -> \"foo\", Bar: p -> 3];" `failsToCompileWithError` caseTypesAreNotCompatibleError
  describe "Record mutability:" $ do
    it "Fields of mutable records can be mutated" $
      "rec Foo = [a: Int]; let x = mut Foo[a = 1]; printLine⟨Int⟩[x.a]; mut x.a = 2; printLine⟨Int⟩[x.a];" `runsSuccessfullyWithOutput` "1\n2\n"
    it "Nested fields of mutable records can be mutated" $
      "rec Foo = [a: Bar]; rec Bar = [b: Int]; let x = mut Foo[a = mut Bar[b = 1]]; printLine⟨Int⟩[x.a.b]; mut x.a.b = 2; printLine⟨Int⟩[x.a.b];" `runsSuccessfullyWithOutput` "1\n2\n"
    it "Records can be passed mutably to functions" $
      "rec Foo = [a: Int]; let f = [x: mut Foo]: Nil -> { mut x.a = x.a + 1; }; let foo = mut Foo[a = 1]; printLine⟨Int⟩[foo.a]; f[foo]; printLine⟨Int⟩[foo.a];" `runsSuccessfullyWithOutput` "1\n2\n"
    it "Records can be captured mutably to functions" $
      "rec Foo = [a: Int]; let foo = mut Foo[a = 1]; let f = []: Nil -> { mut foo.a = foo.a + 1; };  printLine⟨Int⟩[foo.a]; f[]; printLine⟨Int⟩[foo.a];" `runsSuccessfullyWithOutput` "1\n2\n"
    it "Mutable record unions can be deconstructed into mutable records with a case expression" $ do
      "rec Foo = [ a: Int ]; rec Bar = [ a: Int ]; let x: mut Foo | Bar = mut Foo [a = 1]; printLine⟨Int⟩[x.a]; mut x.a = 2; printLine⟨Int⟩[x.a];" `runsSuccessfullyWithOutput` "1\n2\n"
      "rec Foo = [ a: Int ]; rec Bar = [ a: Int ]; let x: mut Foo | Bar = mut Bar [a = 1]; printLine⟨Int⟩[x.a]; mut x.a = 2; printLine⟨Int⟩[x.a];" `runsSuccessfullyWithOutput` "1\n2\n"
    it "If all records of a mutable record union share a field type, that field can be mutated" $ do
      "rec Foo = [a: Int]; rec Bar = [a: Int]; let x: mut Foo | Bar = mut Foo[a = 1]; printLine⟨Int⟩[x.a]; case x of [Foo: p -> { mut p.a = 0; }, Bar: p -> { mut p.a = 2; }]; printLine⟨Int⟩[x.a];" `runsSuccessfullyWithOutput` "1\n0\n"
      "rec Foo = [a: Int]; rec Bar = [a: Int]; let x: mut Foo | Bar = mut Bar[a = 1]; printLine⟨Int⟩[x.a]; case x of [Foo: p -> { mut p.a = 0; }, Bar: p -> { mut p.a = 2; }]; printLine⟨Int⟩[x.a];" `runsSuccessfullyWithOutput` "1\n2\n"
  describe "Record mutability errors:" $ do
    it "Non-record types cannot be marked as mutable" $
      "let x: mut Int = 5; print⟨Int⟩[x];" `failsToCompileWithError` variableDeclarationMalformedTypeError
    it "In a record definition without a mutability paramter, field types cannot be marked as mutable, as field mutability is derived from the overall record mutability" $
      "rec Foo = []; rec Bar = [a: mut Foo];" `failsToCompileWithError` recordFieldExplicitMutabilityError
    it "Fields of non-record types cannot be mutated" $
      "let x = 5; mut x.a = 3;" `failsToCompileWithError` mutatedFieldOfNonRecordTypeError
    it "Fields of immutable records cannot be mutated" $
      "rec Foo = [a: Int]; let x = Foo[a = 1]; mut x.a = 2;" `failsToCompileWithError` mutatedFieldOfImmutableRecordError
    it "Fields not on a record cannot be mutated" $
      "rec Foo = [a: Int]; let x = mut Foo[a = 1]; mut x.b = 2;" `failsToCompileWithError` mutatedFieldNotInRecordError
    it "Fields not on all records of a record union cannot be mutated" $
      "rec Foo = [a: Int]; rec Bar = [b: Int]; let x: mut Foo | Bar = mut Foo[a = 1]; mut x.a = 2;" `failsToCompileWithError` mutatedFieldNotInRecordError
    it "When mutating a record union, the possible record field types cannot be different non-record types" $
      "rec Foo = [a: Int]; rec Bar = [a: String]; let x: mut Foo | Bar = mut Foo[a = 1]; mut x.a = 2;" `failsToCompileWithError` fieldTypesHaveEmptyIntersectionError
    it "When mutating a record union, if the field types are record unions, they must have a common record type" $
      "rec Foo = [a: Baz]; rec Bar = [a: Foo | Bar]; rec Baz = []; let x: mut Foo | Bar = mut Foo[a = Baz]; mut x.a = Baz;" `failsToCompileWithError` fieldTypesHaveEmptyIntersectionError
    it "When mutating a record union, if the field types are record unions, they must have a common record type" $
      "rec Foo = [a: Int]; let x = mut Foo[a = 1]; mut x.a = 'a';" `failsToCompileWithError` typeExpectationError
    it "Fields of a mutable record must be mutable records" $
      "rec Foo = [a: Int]; rec Bar = [b: Foo]; let x = mut Bar[b = Foo[a = 1]];" `failsToCompileWithError` recordExpressionMutabilityTypeError
  describe "Record type parameters:" $ do
    it "Records can be defined and created with type parameters" $
      "rec Foo = ⟨T⟩ => [value: T]; let x = Foo⟨Int⟩[value = 1]; print⟨Int⟩[x.value];" `runsSuccessfullyWithOutput` "1"
    it "Records can be defined and created with multiple type parameters" $
      "rec Foo = ⟨T, V⟩ => [a: T, b: V]; let x = Foo⟨Int, String⟩[a = 1, b = \"two\"]; printLine⟨Int⟩[x.a]; printLine⟨String⟩[x.b];" `runsSuccessfullyWithOutput` "1\ntwo\n"
    it "Records can be defined and created mutably with type parameters" $
      "rec Foo = ⟨T⟩ => [value: T]; let x = mut Foo⟨Int⟩[value = 1]; printLine⟨Int⟩[x.value]; mut x.value = 2; printLine⟨Int⟩[x.value];" `runsSuccessfullyWithOutput` "1\n2\n"
    it "Record mutability can be passed to fields with a type parameter" $
      "rec Foo = ⟨T⟩ => [value: T]; rec Bar = ⟨mut M⟩ => [foo: M Foo⟨Int⟩]; let x = mut Bar[foo = mut Foo[value = 1]]; printLine⟨Int⟩[x.foo.value]; mut x.foo.value = 2; printLine⟨Int⟩[x.foo.value];" `runsSuccessfullyWithOutput` "1\n2\n"
    it "Record immutability can be passed to fields with a type parameter" $
      "rec Foo = ⟨T⟩ => [value: T]; rec Bar = ⟨mut M⟩ => [foo: M Foo⟨Int⟩]; let x = Bar[foo = Foo[value = 1]]; mut x.foo.value = 2;" `failsToCompileWithError` mutatedFieldOfImmutableRecordError
    it "Record mutability can be passed to fields statically if a type parameter is defined" $
      "rec Foo = ⟨T⟩ => [value: T]; rec Bar = ⟨mut M⟩ => [foo: mut Foo⟨Int⟩]; let x = mut Bar[foo = mut Foo[value = 1]]; printLine⟨Int⟩[x.foo.value]; mut x.foo.value = 2; printLine⟨Int⟩[x.foo.value];" `runsSuccessfullyWithOutput` "1\n2\n"
    it "Record immmutability can be passed to fields statically if a type parameter is defined" $
      "rec Foo = ⟨T⟩ => [value: T]; rec Bar = ⟨mut M⟩ => [foo: Foo⟨Int⟩]; let x = Bar[foo = Foo[value = 1]]; mut x.foo.value = 2;" `failsToCompileWithError` mutatedFieldOfImmutableRecordError
    it "Records type parameters can be inferred" $
      "rec Foo = ⟨T⟩ => [value: T]; let x: Foo⟨Int⟩ = Foo[value = 1]; print⟨Int⟩[x.value];" `runsSuccessfullyWithOutput` "1"
    it "Type parameters of immutable records are covariant" $
      "rec Foo = ⟨T⟩ => [value: T]; rec Bar = []; rec Baz = []; let x: Foo⟨Bar | Baz⟩ = Foo⟨Bar⟩[value = Bar];" `runsSuccessfullyWithOutput` ""
    it "Type parameters of mutable records are invariant" $
      "rec Foo = ⟨T⟩ => [value: T]; rec Bar = []; rec Baz = []; let x: mut Foo⟨Bar | Baz⟩ = mut Foo⟨Bar⟩[value = Bar];" `failsToCompileWithError` typeExpectationError

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

accessedFieldOfNonRecordValueError :: Error -> Bool
accessedFieldOfNonRecordValueError (AccessedFieldOfNonRecordValueError {}) = True
accessedFieldOfNonRecordValueError _ = False

accessedFieldNotInRecordError :: Error -> Bool
accessedFieldNotInRecordError (AccessedFieldNotInRecordError {}) = True
accessedFieldNotInRecordError _ = False

caseExpressionDuplicatedCasesError :: Error -> Bool
caseExpressionDuplicatedCasesError (CaseExpressionDuplicatedCasesError {}) = True
caseExpressionDuplicatedCasesError _ = False

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

mutatedFieldOfNonRecordTypeError :: Error -> Bool
mutatedFieldOfNonRecordTypeError (MutatedFieldOfNonRecordTypeError {}) = True
mutatedFieldOfNonRecordTypeError _ = False

mutatedFieldOfImmutableRecordError :: Error -> Bool
mutatedFieldOfImmutableRecordError (MutatedFieldOfImmutableRecordError {}) = True
mutatedFieldOfImmutableRecordError _ = False

mutatedFieldNotInRecordError :: Error -> Bool
mutatedFieldNotInRecordError (MutatedFieldNotInRecordError {}) = True
mutatedFieldNotInRecordError _ = False

fieldTypesHaveEmptyIntersectionError :: Error -> Bool
fieldTypesHaveEmptyIntersectionError (FieldTypesHaveEmptyIntersectionError {}) = True
fieldTypesHaveEmptyIntersectionError _ = False

typeExpectationError :: Error -> Bool
typeExpectationError (TypeExpectationError {}) = True
typeExpectationError _ = False

variableDeclarationMalformedTypeError :: Error -> Bool
variableDeclarationMalformedTypeError (VariableDeclarationMalformedTypeError {}) = True
variableDeclarationMalformedTypeError _ = False

recordFieldExplicitMutabilityError :: Error -> Bool
recordFieldExplicitMutabilityError (RecordFieldExplicitMutabilityError {}) = True
recordFieldExplicitMutabilityError _ = False

recordExpressionMutabilityTypeError :: Error -> Bool
recordExpressionMutabilityTypeError (RecordExpressionMutabilityTypeError {}) = True
recordExpressionMutabilityTypeError _ = False