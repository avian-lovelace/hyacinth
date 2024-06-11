module Core.Pretty (Pretty (pretty)) where

import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text

class Pretty t where
  pretty :: t -> String

instance (Pretty a) => Pretty (Seq a) where
  pretty xs = fold . Seq.intersperse " " $ pretty <$> xs

instance (Pretty a) => Pretty [a] where
  pretty = pretty . Seq.fromList

instance (Pretty a) => Pretty (Maybe a) where
  pretty (Just x) = pretty x
  pretty Nothing = "()"

instance Pretty () where
  pretty () = "()"

instance Pretty Text where
  pretty = Text.unpack

instance Pretty Int where
  pretty = show

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty (Left a) = pretty a
  pretty (Right b) = pretty b

instance (Pretty a, Pretty b) => Pretty (a, b) where
  pretty (a, b) = pretty a ++ " " ++ pretty b

instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
  pretty (a, b, c) = pretty a ++ " " ++ pretty b ++ " " ++ pretty c

instance (Pretty a, Pretty b) => Pretty (Map a b) where
  pretty = pretty . Map.toList