module Core.Type
  ( Type
      ( IntType,
        FloatType,
        CharType,
        StringType,
        BoolType,
        NilType,
        FunctionType
      ),
  )
where

import Core.Utils
import Data.Foldable (fold)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

data Type
  = IntType
  | FloatType
  | CharType
  | StringType
  | BoolType
  | NilType
  | FunctionType (Seq Type) Type
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