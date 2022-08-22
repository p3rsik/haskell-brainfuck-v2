{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

module Interpreter.Util
  ( InfiniteList (..),
  )
where

import           GHC.Float              (expts10)
import           Interpreter.Data.Types (Command, Commands (..), Mem (..))
import           Relude

-- | Class to generalize unsafe versions of head and tail on infinite lists.
class InfiniteList a b | a -> b where
  unsafeHead :: a -> b
  unsafeTail :: a -> a

instance InfiniteList Commands Command where
  unsafeHead (Commands (x : _)) = x
  unsafeHead (Commands [])      = error "Will never reach here"

  unsafeTail (Commands (_ : xs)) = Commands xs
  unsafeTail (Commands [])       = error "Will never reach here"

instance InfiniteList (Mem a) a where
  unsafeHead (Mem (x : _)) = x
  unsafeHead (Mem [])      = error "Will never reach here"

  unsafeTail (Mem (_ : xs)) = Mem xs
  unsafeTail (Mem [])       = error "Will never reach here"
