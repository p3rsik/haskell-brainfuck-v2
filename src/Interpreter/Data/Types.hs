module Interpreter.Data.Types
  ( SyntaxError (..),
    ProgramState (..),
    Memory (..),
    MemoryConstraint,
    Command (..),
    Code (..),
    InterpreterError (..),
    PrintInterrupt,
    WriteInterrupt,
    ProgramUnverified (..),
    ProgramVerified (..),
    Commands (..),
    Mem (..),
  )
where

import           Control.Monad.Cont (ContT)
import           GHC.Show           (Show (show))
import           Relude

-- | Local interpreter state for each program
data ProgramState a = ProgramState
  { getMemory :: Memory a,
    getCode   :: Code
  } deriving (Show, Eq)

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

-- | Interpreter wide errors
data InterpreterError
  = SyntaxError SyntaxError -- Wrapper around 'SyntaxError'
  | FileError Text -- file not found, can't be read, etc
  | UnexpectedError Text
  deriving (Show, Eq, Generic, Exception)

-- | Error for 'checkSyntax' function
data SyntaxError = NotMatchingBrackets deriving (Show, Eq)

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
