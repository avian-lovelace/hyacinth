module Core.Utils
  ( seqHead,
    seqLast,
    seqSplitOn,
    seqPartitionEither,
    seqFilterMap,
    setFilterMap,
    firstM,
    secondM,
    insertAndReplace,
    (<>?),
    uncurry3,
    seqTranspose,
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

seqHead :: Seq a -> a
seqHead s = Seq.index s 0

seqLast :: Seq a -> a
seqLast s = Seq.index s (Seq.length s - 1)

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

seqTranspose :: Seq (Seq a) -> Seq (Seq a)
seqTranspose seqs =
  if all Seq.null seqs
    then Empty
    else (seqHead <$> seqs) :<| seqTranspose (Seq.drop 1 <$> seqs)