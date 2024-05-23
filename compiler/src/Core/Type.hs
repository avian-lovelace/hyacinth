module Core.Type
  ( Type
      ( IntType,
        FloatType,
        CharType,
        StringType,
        BoolType,
        NilType,
        FunctionType,
        RecordUnionType
      ),
    subTypes,
    getCombinedType,
    getCombinedTypeF,
    getIntersectionType,
    getIntersectionTypeF,
  )
where

import Core.SyntaxTree
import Core.Utils
import Data.Foldable (fold)
import Data.List (intersperse)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import IdentifierBinding.SyntaxTree

data Type
  = IntType
  | FloatType
  | CharType
  | StringType
  | BoolType
  | NilType
  | FunctionType (Seq Type) Type
  | RecordUnionType Mutability (Set BoundRecordIdentifier)
  deriving (Eq)

instance Pretty Type where
  pretty IntType = "Int"
  pretty FloatType = "Float"
  pretty CharType = "Char"
  pretty StringType = "String"
  pretty BoolType = "Bool"
  pretty NilType = "Nil"
  pretty (FunctionType parameterTypes returnType) =
    "[" ++ (fold . Seq.intersperse ", " $ pretty <$> parameterTypes) ++ "] -> " ++ pretty returnType
  pretty (RecordUnionType mutability recordNames) = mutabilityString ++ (fold . intersperse " | " $ (Text.unpack . getTextName <$> Set.toList recordNames))
    where
      mutabilityString = case mutability of
        Mutable -> "mut "
        Immutable -> ""

subTypes :: Type -> Type -> Bool
(RecordUnionType mutability1 records1) `subTypes` (RecordUnionType mutability2 records2) = mutabiliesAreCompatible && records1 `Set.isSubsetOf` records2
  where
    mutabiliesAreCompatible = case (mutability1, mutability2) of
      (Immutable, Mutable) -> False
      _ -> True
type1 `subTypes` type2 = type1 == type2

getCombinedType :: Type -> Type -> Maybe Type
getCombinedType (RecordUnionType mutability1 records1) (RecordUnionType mutability2 records2) = Just $ RecordUnionType combinedMutability (Set.union records1 records2)
  where
    combinedMutability = case (mutability1, mutability2) of
      (Mutable, Mutable) -> Mutable
      _ -> Immutable
getCombinedType type1 type2 = if type1 == type2 then Just type1 else Nothing

getCombinedTypeF :: (Foldable t, Functor t) => t Type -> Maybe Type
getCombinedTypeF types = foldr1 getCombinedTypeM (Just <$> types)
  where
    getCombinedTypeM m1 m2 =
      do
        t1 <- m1
        t2 <- m2
        getCombinedType t1 t2

-- Given two type constraints, get the minimal type constraint that satisfies both
getIntersectionType :: Type -> Type -> Maybe Type
getIntersectionType (RecordUnionType mutability1 records1) (RecordUnionType mutability2 records2) =
  if Set.null combinedRecords
    then Nothing
    else Just $ RecordUnionType combinedMutability combinedRecords
  where
    combinedMutability = case (mutability1, mutability2) of
      -- Since these types are considered as type constraints, in the mixed case we use the more restrictive requirement of Mutable
      (Immutable, Immutable) -> Immutable
      _ -> Mutable
    combinedRecords = (Set.intersection records1 records2)
getIntersectionType type1 type2 = if type1 == type2 then Just type1 else Nothing

getIntersectionTypeF :: (Foldable t, Functor t) => t Type -> Maybe Type
getIntersectionTypeF types = foldr1 getIntersectionTypeM (Just <$> types)
  where
    getIntersectionTypeM m1 m2 =
      do
        t1 <- m1
        t2 <- m2
        getIntersectionType t1 t2