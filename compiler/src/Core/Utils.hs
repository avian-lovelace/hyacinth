module Core.Utils
  ( Pretty (pretty),
    Identifier,
    seqHead,
    seqTail,
  )
where

import Data.Sequence as Seq
import Data.Text (Text)

-- Classes
class Pretty t where
  pretty :: t -> String

-- Types
type Identifier = Text

-- Utility function
seqHead :: Seq.Seq a -> a
seqHead s = Seq.index s 0

seqTail :: Seq.Seq a -> a
seqTail s = Seq.index s (Seq.length s - 1)