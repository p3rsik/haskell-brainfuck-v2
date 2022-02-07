module Interpreter.Data
  ( Command (..)
  , Code (..)
  , shiftRCode
  , shiftLCode
  , Memory (..)
  , MemoryConstraint
  , emptyMemory
  , ProgramState (..)
  , getMemory
  , getCode
  , setMemory
  , setCode
  )
where

import           Relude

import           GHC.Show         (Show (show))
import           Interpreter.Util

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

-- This represents the code
data Code = Code { toExec, executed :: [Command], currentInstruction :: !Command } deriving (Eq, Show)

-- Shifts code tape by one instruction to the right(e.g. forward execution)
-- unsafe versions of head and tail used here because this function is carefully constructed
-- to work with them
shiftRCode :: Code -> Code
shiftRCode c@(Code _ _ End) = c
shiftRCode Code {..} = Code (unsafeTail toExec) (currentInstruction:executed) (unsafeHead toExec)

-- Shifts code tape by one instruction to the left(e.g. backwards execution)
-- unsafe versions of head and tail used here because this function is carefully constructed
-- to work with them
shiftLCode :: Code -> Code
shiftLCode c@(Code [] _ _) = c
shiftLCode Code {..} = Code (currentInstruction:toExec) (unsafeTail executed) (unsafeHead executed)


-- Memory cells represented as an infinite tape.
data Memory a = MemoryConstraint a => Memory { left, right :: [a], current :: !a }

instance Show (Memory a) where
  show Memory {..} = Relude.show l <> Relude.show current <> Relude.show r
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

-- Initial memory for Word8 memory cell type
emptyMemory :: MemoryConstraint a => Memory a
emptyMemory = Memory { left = zeroes, current = 0, right = zeroes}
  where
    zeroes = 0 : zeroes

-- Local interpreter state for each program
newtype ProgramState a = ProgramState { getProgramState :: (Memory a, Code) } deriving (Show, Eq)

getMemory :: ProgramState a -> Memory a
getMemory = fst . getProgramState

getCode :: ProgramState a -> Code
getCode = snd . getProgramState

setMemory :: Memory a -> ProgramState a -> ProgramState a
setMemory mem = ProgramState . (mem,) . snd . getProgramState

setCode :: Code -> ProgramState a -> ProgramState a
setCode code = ProgramState . (,code) . fst . getProgramState
