module Interpreter.Types.Util
  ( printInterruptIO,
    writeInterruptIO,
  )
where

import           Interpreter.Types.ProgramState
import           Interpreter.Types.Types
import           Relude
import           System.IO                      (getChar, putChar)


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
