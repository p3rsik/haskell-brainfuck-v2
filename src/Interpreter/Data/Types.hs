module Interpreter.Data.Types
  ( SyntaxError (..)
  , ProgramState (..)
  , Memory (..)
  , MemoryConstraint
  , Command (..)
  , Code (..)
  , InterpreterError (..)
  , PrintInterrupt
  , WriteInterrupt
  )
where

import           Control.Monad.Cont (ContT)
import           GHC.Show           (Show (show))
import           Relude


-- Local interpreter state for each program
newtype ProgramState a = ProgramState { getProgramState :: (Memory a, Code) } deriving (Show, Eq)

-- Memory cells represented as an infinite tape.
data Memory a = MemoryConstraint a => Memory { left, right :: [a], current :: !a }

instance Show (Memory a) where
  show Memory {..} = "..." <>  Relude.show l <> Relude.show current <> Relude.show r <> "..."
    where
    l = reverse $ take 10 left
    r = take 10 right

instance Eq a => Eq (Memory a) where
  mem1 == mem2 = l1 == l2 && r1 == r2 && current mem1 == current mem2
    where
      l1 = take 100 $ left mem1
      r1 = take 100 $ right mem1
      l2 = take 100 $ left mem2
      r2 = take 100 $ right mem2

type MemoryConstraint a = (Eq a, Num a, Show a, Ord a)

-- Commands that are used in brainfuck language
data Command = MoveCell Int -- > or <
             | ChangeCell Int -- + or -
             | PrintCell -- .
             | WriteCell -- ,
             | LoopL -- [
             | LoopR -- ]
             | End -- just the end of the commands
             deriving (Show, Eq)

-- Sequence of Brainfuck instruction
data Code = Code { toExec, executed :: [Command], currentInstruction :: !Command } deriving (Eq, Show)

-- Interpreter wide errors
data InterpreterError =
  SyntaxError SyntaxError -- Wrapper around 'SyntaxError'
  | FileError Text -- file not found, can't be read, etc
  | UnexpectedError Text
  deriving (Show, Eq, Generic, Exception)

-- Error for 'checkSyntax' function
data SyntaxError = NotMatchingBrackets deriving (Show, Eq)

-- Interrupt used to print current memory cell
type PrintInterrupt r m a = Char  -- ^ a Char to print
  -> (() -> ContT r m (ProgramState a)) -- ^ return continuation
  -> ContT r m (ProgramState a)

-- Interrupt used to write into the current memory cell
type WriteInterrupt r m a = (Char -> ContT r m (ProgramState a)) -- ^ return continuation
  -> ContT r m (ProgramState a)
