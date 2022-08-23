module Interpreter.Run
  ( run,
    move,
    change,
    printCell,
    writeCell,
    execProgram,
    execProgramDebug,
  )
where

import           Control.Monad.Cont
import           Interpreter.Types
import           Prelude            (getLine)
import           Relude             hiding (getLine)

-- Moves current selected memory cell 'Int' cells forward
-- or backward, depending on the sign
move :: Int -> ProgramState a -> ProgramState a
move n s@(programMemory -> mem) = flip setMemory s $ move' n mem
  where
    move' :: Int -> ProgramMemory a -> ProgramMemory a
    move' 0 mem = mem
    move' i ProgramMemory {..} =
      if i < 0
        then move' (i + 1) $ ProgramMemory (Memory $ current : unMem left) (unsafeTail right) (unsafeHead right)
        else move' (i - 1) $ ProgramMemory (unsafeTail left) (Memory $ current : unMem right) (unsafeHead left)

-- Changes current selected memory cell by 'Int'
-- e.g. if 'Int' is -2, then 2 will be substructed from current cell
change :: Num a => Int -> ProgramState a -> ProgramState a
change i s@(programMemory -> ProgramMemory {..}) = flip setMemory s $ ProgramMemory left right (current + fromIntegral i)

-- Seeks '[' token in the code and moves current instruction to it
seekLoopL :: ProgramState a -> ProgramState a
seekLoopL s@(programCode -> code) = flip setCode s $ seekLoopL' 0 code
  where
    seekLoopL' :: Int -> ProgramCode -> ProgramCode
    seekLoopL' 0 c@(ProgramCode _ _ LoopL) = c
    -- if we see a '[' - that means that we need to skip the next ']'
    -- for this we use additional 'Int', which, basically, represents
    -- a nesting level(i.e. [ - first level [ - second level ...] - second level complete ] - first level complete)
    seekLoopL' n c@(ProgramCode _ _ LoopR) = seekLoopL' (succ n) $ shiftLCode c
    seekLoopL' n c@(ProgramCode _ _ LoopL) = seekLoopL' (pred n) $ shiftLCode c
    seekLoopL' n c                         = seekLoopL' n $ shiftLCode c

-- Seeks ']' token in the code and moves current instruction to it
seekLoopR :: ProgramState a -> ProgramState a
seekLoopR s@(programCode -> code) = flip setCode s $ seekLoopR' 0 code
  where
    seekLoopR' :: Int -> ProgramCode -> ProgramCode
    seekLoopR' 0 c@(ProgramCode _ _ LoopR) = c
    -- if we see a '[' - that means that we need to skip the next ']'
    -- for this we use additional 'Int', which, basically, represents
    -- a nesting level(i.e. [ - first level [ - second level ...] - second level complete ] - first level complete)
    seekLoopR' n c@(ProgramCode _ _ LoopL) = seekLoopR' (succ n) $ shiftRCode c
    seekLoopR' n c@(ProgramCode _ _ LoopR) = seekLoopR' (pred n) $ shiftRCode c
    seekLoopR' n c                         = seekLoopR' n $ shiftRCode c

-- Given 'ProgramState a' return currently selected memory cell as a 'Char'
printCell :: (Num a, Integral a) => ProgramState a -> Char
printCell (programMemory -> ProgramMemory {..}) = chr $ fromIntegral current

-- Replaces currently selected memory cell in 'ProgramState a' with the given 'Char'
writeCell :: (Num a, Show a, Ord a) => Char -> ProgramState a -> ProgramState a
writeCell a ps = flip setMemory ps $ let ProgramMemory {..} = programMemory ps in ProgramMemory {current = fromIntegral $ ord a, ..}

-- Atomicly executes one instruction for the given 'ProgramState a'
-- 'PrintInterrupt r m a' and 'WriteInterrupt r m a' are called for '.' and ',' respectively
-- to get/return needed input/output
run ::
  (Num a, Ord a, Eq a, Show a, Integral a) =>
  ProgramState a ->
  PrintInterrupt r m a ->
  WriteInterrupt r m a ->
  ContT r m (ProgramState a)
run ps printI writeI =
  let code = programCode ps
   in case currentInstruction code of
        MoveCell n -> return . execInstruction ps $ move (fromIntegral n)
        ChangeCell n -> return . execInstruction ps $ change (fromIntegral n)
        PrintCell -> do
          callCC $ \ret -> printI (printCell ps) $ ret . const ps
          return $ shiftRProgram ps
        WriteCell -> do
          ps' <- callCC $ \ret -> writeI $ ret . flip writeCell ps
          return $ shiftRProgram ps'
        LoopL -> case current $ programMemory ps of
          0 -> return . seekLoopR $ shiftRProgram ps
          _ -> return $ shiftRProgram ps
        LoopR -> case current $ programMemory ps of
          0 -> return $ shiftRProgram ps
          _ -> return . seekLoopL . flip setCode ps $ shiftLCode code
        End -> return ps
  where
    shiftRProgram :: ProgramState a -> ProgramState a
    shiftRProgram = flip execInstruction id
    execInstruction :: ProgramState a -> (ProgramState a -> ProgramState a) -> ProgramState a
    execInstruction ps@(programCode -> code) instruction = setCode (shiftRCode code) $ instruction ps

-- Executes whole program for the given 'Code'
execProgram ::
  forall a m r.
  (Num a, Ord a, Eq a, Show a, Monad m, Integral a) =>
  ProgramCode ->
  PrintInterrupt (ProgramState a) m a ->
  WriteInterrupt (ProgramState a) m a ->
  m (ProgramState a)
execProgram code printI writeI = loop $ ProgramState { programMemory = emptyMemory, programCode = code }
  where
    loop :: ProgramState a -> m (ProgramState a)
    loop ps@(programCode -> ProgramCode _ _ End) = return ps
    loop ps                           = (`runContT` loop) (run ps printI writeI)

-- Executes whole program step-by-step waiting for user input(e.g. Enter press) after each step
execProgramDebug ::
  forall a m r.
  (Num a, Ord a, Eq a, Show a, Integral a, MonadIO m) =>
  ProgramCode ->
  PrintInterrupt (ProgramState a) m a ->
  WriteInterrupt (ProgramState a) m a ->
  m (ProgramState a)
execProgramDebug code printI writeI = loop $ ProgramState { programMemory = emptyMemory, programCode = code }
  where
    loop :: ProgramState a -> m (ProgramState a)
    loop ps@(programCode -> ProgramCode _ _ End) = return ps
    loop ps = do
      ps' <- (`runContT` return) (run ps printI writeI)
      print ps'
      _ <- liftIO getLine
      loop ps'
