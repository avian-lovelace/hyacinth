module TypeChecking.Type
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
  )
where

import Core.SyntaxTree
import Core.Utils
import Data.Foldable (fold, toList)
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
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
  | RecordUnionType Mutability (Map BoundRecordIdentifier (Seq Type))
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
  pretty (RecordUnionType mutability recordNames) = mutabilityString ++ (fold . intersperse " | " $ (printRecord <$> Map.toList recordNames))
    where
      mutabilityString = case mutability of
        Mutable -> "mut "
        Immutable -> ""
      printRecord (recordName, typeArguments) = (Text.unpack . getTextName $ recordName) ++ printTypeArguments (toList typeArguments)
      printTypeArguments [] = ""
      printTypeArguments typeArguments = "⟨" ++ (fold . intersperse " | " $ (pretty <$> typeArguments))