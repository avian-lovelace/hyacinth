module VarianceSpec
  ( testVariance,
  )
where

import Core.SyntaxTree
import Core.Utils
import qualified Data.Sequence as Seq
import Test.Hspec
import TypeChecking.TypeChecking
import TypeChecking.Variance
import TypeTesting

testVariance :: Spec
testVariance = do
  describe "Type component variances:" $ do
    it "A type is bivariant wrt unused type parameters" $ do
      testTypeInfo "type foo = ⟨T⟩ => Int" (getTypeSynonymVarianceFuncByName "foo") $ \varianceFunc -> do
        varianceFunc Immutable `shouldBe` Seq.fromList [Bivariant]
        varianceFunc Mutable `shouldBe` Seq.fromList [Bivariant]
    it "A type parameter reference is covariant wrt itself" $ do
      testTypeInfo "type foo = ⟨T⟩ => T" (getTypeSynonymVarianceFuncByName "foo") $ \varianceFunc -> do
        varianceFunc Immutable `shouldBe` Seq.fromList [Covariant]
        varianceFunc Mutable `shouldBe` Seq.fromList [Covariant]
    it "A function is covariant wrt its output" $ do
      testTypeInfo "type foo = ⟨T⟩ => [Int] -> T" (getTypeSynonymVarianceFuncByName "foo") $ \varianceFunc -> do
        varianceFunc Immutable `shouldBe` Seq.fromList [Covariant]
        varianceFunc Mutable `shouldBe` Seq.fromList [Covariant]
    it "A function is contravariant wrt its inputs" $ do
      testTypeInfo "type foo = ⟨T⟩ => [T] -> Int" (getTypeSynonymVarianceFuncByName "foo") $ \varianceFunc -> do
        varianceFunc Immutable `shouldBe` Seq.fromList [Contravariant]
        varianceFunc Mutable `shouldBe` Seq.fromList [Contravariant]
    it "Type synonyms with multiple type parameters have variances calculated correctly" $ do
      testTypeInfo "type foo = ⟨T, U, V⟩ => [T, U] -> V" (getTypeSynonymVarianceFuncByName "foo") $ \varianceFunc -> do
        varianceFunc Immutable `shouldBe` Seq.fromList [Contravariant, Contravariant, Covariant]
        varianceFunc Mutable `shouldBe` Seq.fromList [Contravariant, Contravariant, Covariant]
    it "An immutable list is covariant wrt its type argument" $ do
      testTypeInfo "type foo = ⟨T⟩ => List⟨T⟩" (getTypeSynonymVarianceFuncByName "foo") $ \varianceFunc -> do
        varianceFunc Immutable `shouldBe` Seq.fromList [Covariant]
        varianceFunc Mutable `shouldBe` Seq.fromList [Covariant]
    it "A mutable list is invariant wrt its type argument" $ do
      testTypeInfo "type foo = ⟨T⟩ => mut List⟨T⟩" (getTypeSynonymVarianceFuncByName "foo") $ \varianceFunc -> do
        varianceFunc Immutable `shouldBe` Seq.fromList [Invariant]
        varianceFunc Mutable `shouldBe` Seq.fromList [Invariant]
    it "Variance calculations correctly handle mutability parameters of type synonyms" $ do
      testTypeInfo "type foo = ⟨mut M, T⟩ => M List⟨T⟩" (getTypeSynonymVarianceFuncByName "foo") $ \varianceFunc -> do
        varianceFunc Immutable `shouldBe` Seq.fromList [Covariant]
        varianceFunc Mutable `shouldBe` Seq.fromList [Invariant]
  describe "Combining variances:" $ do
    it "Functions: Covariant + Contravariant = Invariant" $ do
      testTypeInfo "type foo = ⟨T⟩ => [T] -> T" (getTypeSynonymVarianceFuncByName "foo") $ \varianceFunc -> do
        varianceFunc Immutable `shouldBe` Seq.fromList [Invariant]
        varianceFunc Mutable `shouldBe` Seq.fromList [Invariant]
    it "Functions: Covariant * Covariant = Covariant" $ do
      testTypeInfo "type foo = ⟨T⟩ => [] -> [] -> T" (getTypeSynonymVarianceFuncByName "foo") $ \varianceFunc -> do
        varianceFunc Immutable `shouldBe` Seq.fromList [Covariant]
        varianceFunc Mutable `shouldBe` Seq.fromList [Covariant]
    it "Functions: Covariant * Contravariant = Contravariant" $ do
      testTypeInfo "type foo = ⟨T⟩ => [] -> [T] -> Int" (getTypeSynonymVarianceFuncByName "foo") $ \varianceFunc -> do
        varianceFunc Immutable `shouldBe` Seq.fromList [Contravariant]
        varianceFunc Mutable `shouldBe` Seq.fromList [Contravariant]
    it "Functions: Contravariant * Contravariant = Covariant" $ do
      testTypeInfo "type foo = ⟨T⟩ => [[T] -> Int] -> Int" (getTypeSynonymVarianceFuncByName "foo") $ \varianceFunc -> do
        varianceFunc Immutable `shouldBe` Seq.fromList [Covariant]
        varianceFunc Mutable `shouldBe` Seq.fromList [Covariant]
    it "Type synonyms: Covariant * Covariant = Covariant" $ do
      testTypeInfo
        "type foo = ⟨T⟩ => T; type bar = ⟨T⟩ => foo⟨T⟩"
        (sequence2 (getTypeSynonymVarianceFuncByName "foo", getTypeSynonymVarianceFuncByName "bar"))
        $ \(fooVarianceFunc, barVarianceFunc) -> do
          fooVarianceFunc Immutable `shouldBe` Seq.fromList [Covariant]
          fooVarianceFunc Mutable `shouldBe` Seq.fromList [Covariant]
          barVarianceFunc Immutable `shouldBe` Seq.fromList [Covariant]
          barVarianceFunc Mutable `shouldBe` Seq.fromList [Covariant]
    it "Type synonyms: Covariant * Contravariant = Covariant" $ do
      testTypeInfo
        "type foo = ⟨T⟩ => T; type bar = ⟨T⟩ => foo⟨[T] -> Int⟩"
        (sequence2 (getTypeSynonymVarianceFuncByName "foo", getTypeSynonymVarianceFuncByName "bar"))
        $ \(fooVarianceFunc, barVarianceFunc) -> do
          fooVarianceFunc Immutable `shouldBe` Seq.fromList [Covariant]
          fooVarianceFunc Mutable `shouldBe` Seq.fromList [Covariant]
          barVarianceFunc Immutable `shouldBe` Seq.fromList [Contravariant]
          barVarianceFunc Mutable `shouldBe` Seq.fromList [Contravariant]
    it "Type synonyms: Contravariant * Contravariant = Covariant" $ do
      testTypeInfo
        "type foo = ⟨T⟩ => [T] -> Int; type bar = ⟨T⟩ => foo⟨[T] -> Int⟩"
        (sequence2 (getTypeSynonymVarianceFuncByName "foo", getTypeSynonymVarianceFuncByName "bar"))
        $ \(fooVarianceFunc, barVarianceFunc) -> do
          fooVarianceFunc Immutable `shouldBe` Seq.fromList [Contravariant]
          fooVarianceFunc Mutable `shouldBe` Seq.fromList [Contravariant]
          barVarianceFunc Immutable `shouldBe` Seq.fromList [Covariant]
          barVarianceFunc Mutable `shouldBe` Seq.fromList [Covariant]
    it "Type synonyms: Bivariant * Contravariant = Bivariant" $ do
      testTypeInfo
        "type foo = ⟨T⟩ => Int; type bar = ⟨T⟩ => foo⟨[T] -> Int⟩"
        (sequence2 (getTypeSynonymVarianceFuncByName "foo", getTypeSynonymVarianceFuncByName "bar"))
        $ \(fooVarianceFunc, barVarianceFunc) -> do
          fooVarianceFunc Immutable `shouldBe` Seq.fromList [Bivariant]
          fooVarianceFunc Mutable `shouldBe` Seq.fromList [Bivariant]
          barVarianceFunc Immutable `shouldBe` Seq.fromList [Bivariant]
          barVarianceFunc Mutable `shouldBe` Seq.fromList [Bivariant]
    it "Type synonyms: Invariant * Contravariant = Invariant" $ do
      testTypeInfo
        "type foo = ⟨T⟩ => [T] -> T; type bar = ⟨T⟩ => foo⟨[T] -> Int⟩"
        (sequence2 (getTypeSynonymVarianceFuncByName "foo", getTypeSynonymVarianceFuncByName "bar"))
        $ \(fooVarianceFunc, barVarianceFunc) -> do
          fooVarianceFunc Immutable `shouldBe` Seq.fromList [Invariant]
          fooVarianceFunc Mutable `shouldBe` Seq.fromList [Invariant]
          barVarianceFunc Immutable `shouldBe` Seq.fromList [Invariant]
          barVarianceFunc Mutable `shouldBe` Seq.fromList [Invariant]
    it "Type synonyms: Invariant * Bivariant = Bivariant" $ do
      testTypeInfo
        "type foo = ⟨T⟩ => [T] -> T; type bar = ⟨T⟩ => foo⟨Int⟩"
        (sequence2 (getTypeSynonymVarianceFuncByName "foo", getTypeSynonymVarianceFuncByName "bar"))
        $ \(fooVarianceFunc, barVarianceFunc) -> do
          fooVarianceFunc Immutable `shouldBe` Seq.fromList [Invariant]
          fooVarianceFunc Mutable `shouldBe` Seq.fromList [Invariant]
          barVarianceFunc Immutable `shouldBe` Seq.fromList [Bivariant]
          barVarianceFunc Mutable `shouldBe` Seq.fromList [Bivariant]
    it "Type synonyms: Bivariant * Invariant = Bivariant" $ do
      testTypeInfo
        "type foo = ⟨T⟩ => Int; type bar = ⟨T⟩ => foo⟨[T] -> T⟩"
        (sequence2 (getTypeSynonymVarianceFuncByName "foo", getTypeSynonymVarianceFuncByName "bar"))
        $ \(fooVarianceFunc, barVarianceFunc) -> do
          fooVarianceFunc Immutable `shouldBe` Seq.fromList [Bivariant]
          fooVarianceFunc Mutable `shouldBe` Seq.fromList [Bivariant]
          barVarianceFunc Immutable `shouldBe` Seq.fromList [Bivariant]
          barVarianceFunc Mutable `shouldBe` Seq.fromList [Bivariant]
    it "Type synonyms: Covariant + Contravariant = Invariant" $ do
      testTypeInfo
        "type foo = ⟨T, U⟩ => [T] -> U; type bar = ⟨T⟩ => foo⟨T, T⟩"
        (sequence2 (getTypeSynonymVarianceFuncByName "foo", getTypeSynonymVarianceFuncByName "bar"))
        $ \(fooVarianceFunc, barVarianceFunc) -> do
          fooVarianceFunc Immutable `shouldBe` Seq.fromList [Contravariant, Covariant]
          fooVarianceFunc Mutable `shouldBe` Seq.fromList [Contravariant, Covariant]
          barVarianceFunc Immutable `shouldBe` Seq.fromList [Invariant]
          barVarianceFunc Mutable `shouldBe` Seq.fromList [Invariant]
    it "Type synonyms: Contravariant + Contravariant = Contravariant" $ do
      testTypeInfo
        "type foo = ⟨T, U⟩ => [T, U] -> Int; type bar = ⟨T⟩ => foo⟨T, T⟩"
        (sequence2 (getTypeSynonymVarianceFuncByName "foo", getTypeSynonymVarianceFuncByName "bar"))
        $ \(fooVarianceFunc, barVarianceFunc) -> do
          fooVarianceFunc Immutable `shouldBe` Seq.fromList [Contravariant, Contravariant]
          fooVarianceFunc Mutable `shouldBe` Seq.fromList [Contravariant, Contravariant]
          barVarianceFunc Immutable `shouldBe` Seq.fromList [Contravariant]
          barVarianceFunc Mutable `shouldBe` Seq.fromList [Contravariant]
    it "Multi-way by multi-way type synonym variances are calculated correctly" $ do
      testTypeInfo
        "type foo = ⟨T, U⟩ => [T] -> U; type bar = ⟨T, U, V⟩ => foo⟨[T, V] -> U, [U, V] -> Int⟩"
        (sequence2 (getTypeSynonymVarianceFuncByName "foo", getTypeSynonymVarianceFuncByName "bar"))
        $ \(fooVarianceFunc, barVarianceFunc) -> do
          fooVarianceFunc Immutable `shouldBe` Seq.fromList [Contravariant, Covariant]
          fooVarianceFunc Mutable `shouldBe` Seq.fromList [Contravariant, Covariant]
          barVarianceFunc Immutable `shouldBe` Seq.fromList [Covariant, Contravariant, Invariant]
          barVarianceFunc Mutable `shouldBe` Seq.fromList [Covariant, Contravariant, Invariant]
  describe "Record variances:" $ do
    it "Immutable records are covariant with respect to their fields" $ do
      testTypeInfo "rec foo = ⟨T⟩ => [a: T]" (getRecordTypeInfoByName "foo") $ \typeInfo -> do
        let varianceFunc = recordVarianceFunc typeInfo
        varianceFunc Immutable `shouldBe` Seq.fromList [Covariant]
    it "Mutable records are invariant with respect to their fields" $ do
      testTypeInfo "rec foo = ⟨T⟩ => [a: T]" (getRecordTypeInfoByName "foo") $ \typeInfo -> do
        let varianceFunc = recordVarianceFunc typeInfo
        varianceFunc Mutable `shouldBe` Seq.fromList [Invariant]
    it "A record's variance is the sum of the field variances" $ do
      testTypeInfo "rec foo = ⟨T⟩ => [a: T, b: [T] -> Int]" (getRecordTypeInfoByName "foo") $ \typeInfo -> do
        let varianceFunc = recordVarianceFunc typeInfo
        varianceFunc Immutable `shouldBe` Seq.fromList [Invariant]
        varianceFunc Mutable `shouldBe` Seq.fromList [Invariant]
    it "Records with multiple type parameters have their variances caulculated correctly" $ do
      testTypeInfo "rec foo = ⟨T, U, V⟩ => [a: T, b: [T, U] -> T]" (getRecordTypeInfoByName "foo") $ \typeInfo -> do
        let varianceFunc = recordVarianceFunc typeInfo
        varianceFunc Immutable `shouldBe` Seq.fromList [Invariant, Contravariant, Bivariant]
        varianceFunc Mutable `shouldBe` Seq.fromList [Invariant, Invariant, Bivariant]
    it "Immutable record references in type synonyms have their variances calculated correctly" $ do
      testTypeInfo
        "rec foo = ⟨T, U⟩ => [a: [T] -> Int]; type bar = ⟨T, U⟩ => foo⟨T, U⟩"
        (sequence2 (getRecordTypeInfoByName "foo", getTypeSynonymVarianceFuncByName "bar"))
        $ \(fooTypeInfo, barVarianceFunc) -> do
          let fooVarianceFunc = recordVarianceFunc fooTypeInfo
          fooVarianceFunc Immutable `shouldBe` Seq.fromList [Contravariant, Bivariant]
          fooVarianceFunc Mutable `shouldBe` Seq.fromList [Invariant, Bivariant]
          barVarianceFunc Immutable `shouldBe` Seq.fromList [Contravariant, Bivariant]
          barVarianceFunc Mutable `shouldBe` Seq.fromList [Contravariant, Bivariant]
    it "Mutable record references in type synonyms have their variances calculated correctly" $ do
      testTypeInfo
        "rec foo = ⟨T, U⟩ => [a: [T] -> Int]; type bar = ⟨T, U⟩ => mut foo⟨T, U⟩"
        (sequence2 (getRecordTypeInfoByName "foo", getTypeSynonymVarianceFuncByName "bar"))
        $ \(fooTypeInfo, barVarianceFunc) -> do
          let fooVarianceFunc = recordVarianceFunc fooTypeInfo
          fooVarianceFunc Immutable `shouldBe` Seq.fromList [Contravariant, Bivariant]
          fooVarianceFunc Mutable `shouldBe` Seq.fromList [Invariant, Bivariant]
          barVarianceFunc Immutable `shouldBe` Seq.fromList [Invariant, Bivariant]
          barVarianceFunc Mutable `shouldBe` Seq.fromList [Invariant, Bivariant]
    it "Record references with variable mutability in type synonyms have their variances calculated correctly" $ do
      testTypeInfo
        "rec foo = ⟨T, U⟩ => [a: [T] -> Int]; type bar = ⟨mut M, T, U⟩ => M foo⟨T, U⟩"
        (sequence2 (getRecordTypeInfoByName "foo", getTypeSynonymVarianceFuncByName "bar"))
        $ \(fooTypeInfo, barVarianceFunc) -> do
          let fooVarianceFunc = recordVarianceFunc fooTypeInfo
          fooVarianceFunc Immutable `shouldBe` Seq.fromList [Contravariant, Bivariant]
          fooVarianceFunc Mutable `shouldBe` Seq.fromList [Invariant, Bivariant]
          barVarianceFunc Immutable `shouldBe` Seq.fromList [Contravariant, Bivariant]
          barVarianceFunc Mutable `shouldBe` Seq.fromList [Invariant, Bivariant]
    it "References to records with no type parameters in type synonyms have their variances calculated correctly" $ do
      testTypeInfo
        "rec foo = [a: Int]; type bar = ⟨T, U⟩ => foo"
        (sequence2 (getRecordTypeInfoByName "foo", getTypeSynonymVarianceFuncByName "bar"))
        $ \(fooTypeInfo, barVarianceFunc) -> do
          let fooVarianceFunc = recordVarianceFunc fooTypeInfo
          fooVarianceFunc Immutable `shouldBe` Seq.fromList []
          fooVarianceFunc Mutable `shouldBe` Seq.fromList []
          barVarianceFunc Immutable `shouldBe` Seq.fromList [Bivariant, Bivariant]
          barVarianceFunc Mutable `shouldBe` Seq.fromList [Bivariant, Bivariant]
    it "Record references with complex type argyments in type synonyms have their variances calculated correctly" $ do
      testTypeInfo
        "rec foo = ⟨T, U⟩ => [a: [T] -> Int]; type bar = ⟨mut M, T, U, V⟩ => M foo⟨[T] -> U, [U] -> V⟩"
        (sequence2 (getRecordTypeInfoByName "foo", getTypeSynonymVarianceFuncByName "bar"))
        $ \(fooTypeInfo, barVarianceFunc) -> do
          let fooVarianceFunc = recordVarianceFunc fooTypeInfo
          fooVarianceFunc Immutable `shouldBe` Seq.fromList [Contravariant, Bivariant]
          fooVarianceFunc Mutable `shouldBe` Seq.fromList [Invariant, Bivariant]
          barVarianceFunc Immutable `shouldBe` Seq.fromList [Covariant, Contravariant, Bivariant]
          barVarianceFunc Mutable `shouldBe` Seq.fromList [Invariant, Invariant, Bivariant]
    it "Immutable record references in records have their variances calculated correctly" $ do
      testTypeInfo
        "rec foo = ⟨T, U⟩ => [a: [T] -> Int]; rec bar = ⟨mut M, T, U⟩ => [a: foo⟨T, U⟩];"
        (sequence2 (getRecordTypeInfoByName "foo", getRecordTypeInfoByName "bar"))
        $ \(fooTypeInfo, barTypeInfo) -> do
          let fooVarianceFunc = recordVarianceFunc fooTypeInfo
          fooVarianceFunc Immutable `shouldBe` Seq.fromList [Contravariant, Bivariant]
          fooVarianceFunc Mutable `shouldBe` Seq.fromList [Invariant, Bivariant]
          let barVarianceFunc = recordVarianceFunc barTypeInfo
          barVarianceFunc Immutable `shouldBe` Seq.fromList [Contravariant, Bivariant]
          barVarianceFunc Mutable `shouldBe` Seq.fromList [Invariant, Bivariant]
    it "Mutable record references in records have their variances calculated correctly" $ do
      testTypeInfo
        "rec foo = ⟨T, U⟩ => [a: [T] -> Int]; rec bar = ⟨mut M, T, U⟩ => [a: mut foo⟨T, U⟩];"
        (sequence2 (getRecordTypeInfoByName "foo", getRecordTypeInfoByName "bar"))
        $ \(fooTypeInfo, barTypeInfo) -> do
          let fooVarianceFunc = recordVarianceFunc fooTypeInfo
          fooVarianceFunc Immutable `shouldBe` Seq.fromList [Contravariant, Bivariant]
          fooVarianceFunc Mutable `shouldBe` Seq.fromList [Invariant, Bivariant]
          let barVarianceFunc = recordVarianceFunc barTypeInfo
          barVarianceFunc Immutable `shouldBe` Seq.fromList [Invariant, Bivariant]
          barVarianceFunc Mutable `shouldBe` Seq.fromList [Invariant, Bivariant]
    it "Record references with variable mutability in records have their variances calculated correctly" $ do
      testTypeInfo
        "rec foo = ⟨T, U⟩ => [a: [T] -> Int]; rec bar = ⟨T, U⟩ => [a: foo⟨T, U⟩];"
        (sequence2 (getRecordTypeInfoByName "foo", getRecordTypeInfoByName "bar"))
        $ \(fooTypeInfo, barTypeInfo) -> do
          let fooVarianceFunc = recordVarianceFunc fooTypeInfo
          fooVarianceFunc Immutable `shouldBe` Seq.fromList [Contravariant, Bivariant]
          fooVarianceFunc Mutable `shouldBe` Seq.fromList [Invariant, Bivariant]
          let barVarianceFunc = recordVarianceFunc barTypeInfo
          barVarianceFunc Immutable `shouldBe` Seq.fromList [Contravariant, Bivariant]
          barVarianceFunc Mutable `shouldBe` Seq.fromList [Invariant, Bivariant]
  describe "Mutual reference variances:" $ do
    it "Mutually referential records have their variances calculated correctly - test 1" $ do
      testTypeInfo
        "rec foo = ⟨T⟩ => [a: [bar⟨T⟩] -> T]; rec bar = ⟨T⟩ => [a: foo⟨T⟩];"
        (sequence2 (getRecordTypeInfoByName "foo", getRecordTypeInfoByName "bar"))
        $ \(fooTypeInfo, barTypeInfo) -> do
          let fooVarianceFunc = recordVarianceFunc fooTypeInfo
          fooVarianceFunc Immutable `shouldBe` Seq.fromList [Invariant]
          fooVarianceFunc Mutable `shouldBe` Seq.fromList [Invariant]
          let barVarianceFunc = recordVarianceFunc barTypeInfo
          barVarianceFunc Immutable `shouldBe` Seq.fromList [Invariant]
          barVarianceFunc Mutable `shouldBe` Seq.fromList [Invariant]
    it "Mutually referential records have their variances calculated correctly - test 2" $ do
      testTypeInfo
        "rec foo = ⟨T⟩ => [a: [T, bar⟨T⟩] -> Int]; rec bar = ⟨T⟩ => [a: [foo⟨T⟩] -> Int];"
        (sequence2 (getRecordTypeInfoByName "foo", getRecordTypeInfoByName "bar"))
        $ \(fooTypeInfo, barTypeInfo) -> do
          let fooVarianceFunc = recordVarianceFunc fooTypeInfo
          fooVarianceFunc Immutable `shouldBe` Seq.fromList [Contravariant]
          fooVarianceFunc Mutable `shouldBe` Seq.fromList [Invariant]
          let barVarianceFunc = recordVarianceFunc barTypeInfo
          barVarianceFunc Immutable `shouldBe` Seq.fromList [Covariant]
          barVarianceFunc Mutable `shouldBe` Seq.fromList [Invariant]
    it "Mutually referential records have their variances calculated correctly - test 3" $ do
      testTypeInfo
        "rec foo = ⟨T, U⟩ => [a: bar⟨T, U⟩, b: T]; rec bar = ⟨T, U⟩ => [a: foo⟨U, [T] -> Int⟩];"
        (sequence2 (getRecordTypeInfoByName "foo", getRecordTypeInfoByName "bar"))
        $ \(fooTypeInfo, barTypeInfo) -> do
          let fooVarianceFunc = recordVarianceFunc fooTypeInfo
          fooVarianceFunc Immutable `shouldBe` Seq.fromList [Invariant, Invariant]
          fooVarianceFunc Mutable `shouldBe` Seq.fromList [Invariant, Invariant]
          let barVarianceFunc = recordVarianceFunc barTypeInfo
          barVarianceFunc Immutable `shouldBe` Seq.fromList [Invariant, Invariant]
          barVarianceFunc Mutable `shouldBe` Seq.fromList [Invariant, Invariant]