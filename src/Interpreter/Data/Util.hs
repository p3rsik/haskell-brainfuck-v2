module Interpreter.Data.Util
  ( shiftRCode,
    shiftLCode,
    emptyMemory,
    getMemory,
    getCode,
    setMemory,
    setCode,
    printInterruptIO,
    writeInterruptIO,
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
shiftRCode Code {..} = Code (unsafeTail toExec) (currentInstruction : executed) (unsafeHead toExec)

-- Shifts code tape by one instruction to the left(e.g. backwards execution)
-- unsafe versions of head and tail used here because this function is carefully constructed
-- to work with them
shiftLCode :: Code -> Code
shiftLCode c@(Code [] _ _) = c
shiftLCode Code {..} = Code (currentInstruction : toExec) (unsafeTail executed) (unsafeHead executed)

-- Initial empty memory
emptyMemory :: MemoryConstraint a => Memory a
emptyMemory = Memory {left = zeroes, current = 0, right = zeroes}
  where
    zeroes = 0 : zeroes

getMemory :: ProgramState a -> Memory a
getMemory = fst . getProgramState

getCode :: ProgramState a -> Code
getCode = snd . getProgramState

setMemory :: Memory a -> ProgramState a -> ProgramState a
setMemory mem = ProgramState . (mem,) . snd . getProgramState

setCode :: Code -> ProgramState a -> ProgramState a
setCode code = ProgramState . (,code) . fst . getProgramState

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
