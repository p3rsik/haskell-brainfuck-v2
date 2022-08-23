{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Interpreter.Types.Memory
  ( Memory (..),
    ProgramMemory (..),
    emptyMemory,
  ) where

import           Interpreter.Types.Internal
import           Relude

-- | Newtype wrapper for memory part
newtype Memory a = Memory { unMem :: [a] } deriving (Eq, Show)

-- | Memory cells represented as an infinite tape.
data ProgramMemory a = ProgramMemory {left, right :: Memory a, current :: !a} deriving (Eq, Show)

-- Initial empty memory
emptyMemory :: (Show a, Eq a, Num a) => ProgramMemory a
emptyMemory = ProgramMemory {left = Memory zeroes, current = 0, right = Memory zeroes}
  where
    zeroes = 0 : zeroes


instance InfiniteList (Memory a) a where
  unsafeHead (Memory (x : _)) = x
  unsafeHead (Memory [])      = error "Will never reach here"

  unsafeTail (Memory (_ : xs)) = Memory xs
  unsafeTail (Memory [])       = error "Will never reach here"
