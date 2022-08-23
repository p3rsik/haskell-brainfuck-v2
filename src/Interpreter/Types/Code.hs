module Interpreter.Types.Code
  ( Command (..),
    Code (..),
    Commands (..),
  ) where

import           Relude


-- | Commands that are used in brainfuck language
data Command
  = MoveCell Int -- > or <
  | ChangeCell Int -- + or -
  | PrintCell -- .
  | WriteCell -- ,
  | LoopL -- [
  | LoopR -- ]
  | End -- just the end of the commands
  deriving (Show, Eq)

-- | Sequence of Brainfuck instruction
data Code = Code {toExec, executed :: Commands, currentInstruction :: !Command} deriving (Eq, Show)

-- | Newtype wrapper to enforce some invariants
newtype Commands = Commands { unCommands :: [Command] } deriving (Eq, Show)
