module Core.Type
  ( Type
      ( IntType,
        FloatType,
        CharType,
        StringType,
        BoolType,
        NilType,
        FunctionType,
        RecordType
      ),
    fromTypeExpression,
  )
where

import Core.SyntaxTree
import Core.Utils
import Data.Foldable (fold)
import Data.Sequence (Seq)
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
  | RecordType BoundRecordIdentifier
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
  pretty (RecordType recordName) = Text.unpack . getTextName $ recordName

fromTypeExpression :: IBTypeExpression -> Type
fromTypeExpression (IntTypeExpression _) = IntType
fromTypeExpression (FloatTypeExpression _) = FloatType
fromTypeExpression (CharTypeExpression _) = CharType
fromTypeExpression (StringTypeExpression _) = StringType
fromTypeExpression (BoolTypeExpression _) = BoolType
fromTypeExpression (NilTypeExpression _) = NilType
fromTypeExpression (FunctionTypeExpression _ parameterTypes returnType) = FunctionType (fromTypeExpression <$> parameterTypes) (fromTypeExpression returnType)
fromTypeExpression (RecordTypeExpression _ recordName) = RecordType recordName