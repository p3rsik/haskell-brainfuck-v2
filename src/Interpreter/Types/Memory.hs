module Interpreter.Types.Memory
  ( Mem (..),
    Memory (..),
    MemoryConstraint (..),
  ) where

import           GHC.Show (Show (show))
import           Relude

-- | Newtype wrapper for memory part
newtype Mem a = Mem { unMem :: [a] }

instance (Show a) => Show (Mem a) where
  show Mem {..} = "..." <> Relude.show (take 10 unMem) <> "..."

instance (Eq a) => Eq (Mem a) where
  mem1 == mem2 = take 100 (unMem mem1) == take 100 (unMem mem2)

-- | Memory cells represented as an infinite tape.
data Memory a = MemoryConstraint a => Memory {left, right :: Mem a, current :: !a}

instance Show (Memory a) where
  show Memory {..} = "..." <> Relude.show left <> Relude.show current <> Relude.show right <> "..."

instance Eq a => Eq (Memory a) where
  mem1 == mem2 = l1 == l2 && r1 == r2 && current mem1 == current mem2
    where
      l1 = left mem1
      r1 = right mem1
      l2 = left mem2
      r2 = right mem2

type MemoryConstraint a = (Eq a, Num a, Show a, Ord a)
