{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Interpreter.Types.Code
  ( Command (..),
    Code (..),
    emptyCode,
    ProgramCode (..),
    shiftRCode,
    shiftLCode,
  ) where

import           Interpreter.Types.Internal
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

-- | Chunk of code
newtype Code = Code { code :: [Command] } deriving (Eq, Show)

emptyCode :: Code
emptyCode = Code []

-- | Tape of commands that are used in interpreter to represent the code to execute + current command + executed code
data ProgramCode = ProgramCode {toExec, executed :: Code, currentInstruction :: !Command} deriving (Eq, Show)


instance InfiniteList Code Command where
  unsafeHead (Code (x : _)) = x
  unsafeHead (Code [])      = error "Will never reach here"

  unsafeTail (Code (_ : xs)) = Code xs
  unsafeTail (Code [])       = error "Will never reach here"


-- Shifts code tape by one instruction to the right(e.g. forward execution)
-- unsafe versions of head and tail used here because this function is carefully constructed
-- to work with them
shiftRCode :: ProgramCode -> ProgramCode
shiftRCode c@(ProgramCode _ _ End) = c
shiftRCode ProgramCode {..} = ProgramCode (unsafeTail toExec)
                                          (Code $ currentInstruction : code executed)
                                          (unsafeHead toExec)

-- Shifts code tape by one instruction to the left(e.g. backwards execution)
-- unsafe versions of head and tail used here because this function is carefully constructed
-- to work with them
shiftLCode :: ProgramCode -> ProgramCode
shiftLCode c@(ProgramCode (Code []) _ _) = c
shiftLCode ProgramCode {..} = ProgramCode (Code $ currentInstruction : code toExec)
                                          (unsafeTail executed)
                                          (unsafeHead executed)
