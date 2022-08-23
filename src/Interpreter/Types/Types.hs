module Interpreter.Types.Types
  ( PrintInterrupt,
    WriteInterrupt,
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
