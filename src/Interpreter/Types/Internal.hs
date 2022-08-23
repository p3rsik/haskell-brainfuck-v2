{-# LANGUAGE FunctionalDependencies #-}

module Interpreter.Types.Internal where

-- | Class to generalize unsafe versions of head and tail on infinite lists.
class InfiniteList a b | a -> b where
  unsafeHead :: a -> b
  unsafeTail :: a -> a
