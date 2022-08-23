module Interpreter.Types.Types
  ( PrintInterrupt,
    WriteInterrupt,
    ProgramUnverified (..),
    ProgramVerified (..),
  )
where

import           Control.Monad.Cont             (ContT)
import           Interpreter.Types.ProgramState
import           Relude



-- | Interrupt used to print current memory cell
type PrintInterrupt r m a =
  -- | a Char to print
  Char ->
  -- | return continuation
  (() -> ContT r m (ProgramState a)) ->
  ContT r m (ProgramState a)

-- | Interrupt used to write into the current memory cell
type WriteInterrupt r m a =
  -- | return continuation
  (Char -> ContT r m (ProgramState a)) ->
  ContT r m (ProgramState a)

-- | Newtype wrapper to encode the fact, that program is unverified(e.g. syntax was not checked)
newtype ProgramUnverified = ProgramUnverified { unProgramUnverified :: Text } deriving (Eq, Show)

-- | Newtype wrapper to encode the fact, that program is verified(e.g. syntax was checked)
newtype ProgramVerified = ProgramVerified { unProgramVerified :: Text } deriving (Eq, Show)
