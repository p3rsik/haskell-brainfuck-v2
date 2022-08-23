module Interpreter.Types.ProgramState
  ( ProgramState (..),
  ) where

import           Interpreter.Types.Code   (Code (..))
import           Interpreter.Types.Memory (Memory (..))
import           Relude


-- | Local interpreter state for each program
data ProgramState a = ProgramState
  { getMemory :: Memory a,
    getCode   :: Code
  } deriving (Show, Eq)
