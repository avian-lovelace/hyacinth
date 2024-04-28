module Core.Utils
  ( Pretty (pretty),
    seqHead,
    seqTail,
    seqSplitOn,
  )
where

import Data.Foldable
import Data.Sequence (Seq (Empty, (:<|)), (<|))
import qualified Data.Sequence as Seq

-- Classes
class Pretty t where
  pretty :: t -> String

instance (Pretty a) => Pretty (Seq a) where
  pretty xs = fold . Seq.intersperse " " $ pretty <$> xs

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