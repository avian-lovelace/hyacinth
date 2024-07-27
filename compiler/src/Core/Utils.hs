module Core.Utils
  ( Pretty (pretty),
    seqHead,
    seqTail,
    seqSplitOn,
    seqPartitionEither,
    seqFilterMap,
    setFilterMap,
    firstM,
    secondM,
    insertAndReplace,
    (<>?),
    uncurry3,
  )
where

import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust)
import Data.Sequence (Seq (Empty, (:<|)), (<|))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

-- Classes
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

instance (Pretty a, Pretty b) => Pretty (Map a b) where
  pretty = pretty . Map.toList

-- Utility function
seqHead :: Seq a -> a
seqHead s = Seq.index s 0

seqTail :: Seq a -> a
seqTail s = Seq.index s (Seq.length s - 1)

seqSplitOn :: (a -> Bool) -> Seq a -> Seq (Seq a)
seqSplitOn _ Empty = Empty
seqSplitOn predicate xs = case Seq.breakl predicate xs of
  (initSeq, Empty) -> Seq.singleton initSeq
  (initSeq, _splitter :<| tailSeq) -> initSeq <| seqSplitOn predicate tailSeq

seqPartitionEither :: Seq (Either a b) -> (Seq a, Seq b)
seqPartitionEither = foldr collect (Empty, Empty)
  where
    collect (Left a) (lefts, rights) = (a <| lefts, rights)
    collect (Right b) (lefts, rights) = (lefts, b <| rights)

seqFilterMap :: (a -> Maybe b) -> Seq a -> Seq b
seqFilterMap f = fmap fromJust . Seq.filter isJust . fmap f

setFilterMap :: (Ord b) => (a -> Maybe b) -> Set a -> Set b
setFilterMap f = Set.map fromJust . Set.filter isJust . Set.map f

firstM :: (Monad m) => (a -> m c) -> (a, b) -> m (c, b)
firstM f (a, b) = do
  c <- f a
  return (c, b)

secondM :: (Monad m) => (b -> m c) -> (a, b) -> m (a, c)
secondM f (a, b) = do
  c <- f b
  return (a, c)

insertAndReplace :: (Ord k) => k -> a -> Map k a -> (Maybe a, Map k a)
insertAndReplace = Map.insertLookupWithKey (\_ a _ -> a)

(<>?) :: (Semigroup a) => a -> Maybe a -> a
x <>? Nothing = x
x <>? (Just y) = x <> y

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z