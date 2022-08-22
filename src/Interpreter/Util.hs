module Interpreter.Util
  ( unsafeHead,
    unsafeTail,
  )
where

import           Relude

-- unsafe version of head
-- since memory is infinite by construction, it can be safely used on it
unsafeHead :: [a] -> a
unsafeHead (x : _) = x
unsafeHead []      = error "Will never reach here"

-- unsafe version of tail
-- since memory is infinite by costruction, it can be safely used on it
unsafeTail :: [a] -> [a]
unsafeTail (_ : xs) = xs
unsafeTail []       = error "Will never reach here"
