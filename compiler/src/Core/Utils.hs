module Core.Utils
  ( Pretty (pretty),
    seqHead,
    seqTail,
  )
where

import Data.Sequence as Seq

-- Classes
class Pretty t where
  pretty :: t -> String

-- Utility function
seqHead :: Seq.Seq a -> a
seqHead s = Seq.index s 0

seqTail :: Seq.Seq a -> a
seqTail s = Seq.index s (Seq.length s - 1)