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
  )
where

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
  | RecordUnionType (Set BoundRecordIdentifier)
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
  pretty (RecordUnionType recordNames) = fold . intersperse " | " $ (Text.unpack . getTextName <$> Set.toList recordNames)

subTypes :: Type -> Type -> Bool
(RecordUnionType records1) `subTypes` (RecordUnionType records2) = records1 `Set.isSubsetOf` records2
type1 `subTypes` type2 = type1 == type2

getCombinedType :: Type -> Type -> Maybe Type
getCombinedType (RecordUnionType records1) (RecordUnionType records2) = Just $ RecordUnionType (Set.union records1 records2)
getCombinedType type1 type2 = if type1 == type2 then Just type1 else Nothing

getCombinedTypeF :: (Foldable t, Functor t) => t Type -> Maybe Type
getCombinedTypeF types = foldr1 getCombinedTypeM (Just <$> types)
  where
    getCombinedTypeM m1 m2 =
      do
        t1 <- m1
        t2 <- m2
        getCombinedType t1 t2