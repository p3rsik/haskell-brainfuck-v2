module Interpreter.Types.ProgramState
  ( ProgramState (..),
    setMemory,
    setCode,
  ) where

import           Interpreter.Types.Code   (ProgramCode (..))
import           Interpreter.Types.Memory (ProgramMemory (..))
import           Relude


-- | Local interpreter state for each program
data ProgramState a = ProgramState
  { programMemory :: ProgramMemory a,
    programCode   :: ProgramCode
  } deriving (Show, Eq)


setMemory :: ProgramMemory a -> ProgramState a -> ProgramState a
setMemory mem ProgramState {..} = let programMemory = mem in ProgramState {..}

setCode :: ProgramCode -> ProgramState a -> ProgramState a
setCode code ProgramState {..} = let programCode = code in ProgramState {..}
