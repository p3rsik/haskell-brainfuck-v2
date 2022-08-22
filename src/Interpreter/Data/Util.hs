module Interpreter.Data.Util
  ( shiftRCode,
    shiftLCode,
    emptyMemory,
    printInterruptIO,
    writeInterruptIO,
    setMemory,
    setCode,
  )
where

import           Interpreter.Data.Types
import           Interpreter.Util
import           Relude
import           System.IO              (getChar, putChar)

-- Shifts code tape by one instruction to the right(e.g. forward execution)
-- unsafe versions of head and tail used here because this function is carefully constructed
-- to work with them
shiftRCode :: Code -> Code
shiftRCode c@(Code _ _ End) = c
shiftRCode Code {..} = Code (unsafeTail toExec) (Commands $ currentInstruction : unCommands executed) (unsafeHead toExec)

-- Shifts code tape by one instruction to the left(e.g. backwards execution)
-- unsafe versions of head and tail used here because this function is carefully constructed
-- to work with them
shiftLCode :: Code -> Code
shiftLCode c@(Code (Commands []) _ _) = c
shiftLCode Code {..} = Code (Commands $ currentInstruction : unCommands toExec) (unsafeTail executed) (unsafeHead executed)

-- Initial empty memory
emptyMemory :: MemoryConstraint a => Memory a
emptyMemory = Memory {left = Mem zeroes, current = 0, right = Mem zeroes}
  where
    zeroes = 0 : zeroes

-- Default interrupt used for printing
printInterruptIO :: PrintInterrupt (ProgramState a) IO a
printInterruptIO ch ret = do
  liftIO $ putChar ch
  ret ()

-- Default interrupt used for writing
writeInterruptIO :: WriteInterrupt (ProgramState a) IO a
writeInterruptIO ret = do
  l <- liftIO getChar
  ret l

setMemory :: Memory a -> ProgramState a -> ProgramState a
setMemory mem ProgramState {..} = let getMemory = mem in ProgramState {..}

setCode :: Code -> ProgramState a -> ProgramState a
setCode code ProgramState {..} = let getCode = code in ProgramState {..}
