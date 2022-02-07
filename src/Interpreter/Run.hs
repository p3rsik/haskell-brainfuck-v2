module Interpreter.Run
  ( run
  , move
  , change
  , printCell
  , writeCell
  , Interrupt
  , execProgram
  , execProgramDebug
  )
where

import           Control.Monad.Cont
import           Prelude            (getLine)
import           Relude             hiding (getLine)

import           Interpreter.Data
import           Interpreter.Util


move, change :: Int -> ProgramState a -> ProgramState a
move n s@(getMemory -> mem) = flip setMemory s $ move' n mem
  where
    move' :: Int -> Memory a -> Memory a
    move' 0 mem = mem
    move' i Memory {..} =
      if i < 0
      then move' (i + 1) $ Memory (current:left) (unsafeTail right) (unsafeHead right)
      else move' (i - 1) $ Memory (unsafeTail left) (current:right) (unsafeHead left)
change i s@(getMemory -> Memory {..}) = flip setMemory s $ Memory left right (current + fromIntegral i)

seekLoopL, seekLoopR :: ProgramState a -> ProgramState a
seekLoopL s@(getCode -> code) = flip setCode s $ seekLoopL' 0 code
  where
    seekLoopL' :: Int -> Code -> Code
    seekLoopL' 0 c@(Code _ _ LoopL) = c
    -- if we see a '[' - that means that we need to skip the next ']'
    -- for this we use additional 'Int', which, basically, represents
    -- a nesting level(i.e. [ - first level [ - second level ...] - second level complete ] - first level complete)
    seekLoopL' n c@(Code _ _ LoopR) = seekLoopL' (succ n) $ shiftLCode c
    seekLoopL' n c@(Code _ _ LoopL) = seekLoopL' (pred n) $ shiftLCode c
    seekLoopL' n c                  = seekLoopL' n $ shiftLCode c
seekLoopR s@(getCode -> code) = flip setCode s $ seekLoopR' 0 code
  where
    seekLoopR' :: Int -> Code -> Code
    seekLoopR' 0 c@(Code _ _ LoopR) = c
    -- if we see a '[' - that means that we need to skip the next ']'
    -- for this we use additional 'Int', which, basically, represents
    -- a nesting level(i.e. [ - first level [ - second level ...] - second level complete ] - first level complete)
    seekLoopR' n c@(Code _ _ LoopL) = seekLoopR' (succ n) $ shiftRCode c
    seekLoopR' n c@(Code _ _ LoopR) = seekLoopR' (pred n) $ shiftRCode c
    seekLoopR' n c                  = seekLoopR' n $ shiftRCode c


printCell :: (Num a, Integral a) => ProgramState a -> Char
printCell (getMemory -> Memory {..}) = chr $ fromIntegral current

writeCell :: (Num a, Show a, Ord a) => Char -> ProgramState a -> ProgramState a
writeCell a ps = flip setMemory ps $ let Memory {..} = getMemory ps in Memory {current = fromIntegral $ ord a, ..}

type Interrupt r a m = ProgramState a -- ^ state of interpreter at the moment of interrupt
  -> (ProgramState a -> ContT r m (ProgramState a)) -- ^ return continuation
  -> ContT r m (ProgramState a)

-- step by step interpretation of the program from the given 'ProgramState a'
run :: (Num a, Ord a, Eq a) => ProgramState a
  -> Interrupt r a m -- ^ Print interrupt
  -> Interrupt r a m -- ^ Write interrupt
  -> ContT r m (ProgramState a)
run ps printI writeI = let code = getCode ps in
  case currentInstruction code of
    MoveCell n   -> return . execInstruction ps $ move (fromIntegral n)
    ChangeCell n -> return . execInstruction ps $ change (fromIntegral n)
    PrintCell    -> do
      ps' <- callCC $ \ret -> printI ps ret
      return $ shiftRProgram ps'
    WriteCell    -> do
      ps' <- callCC $ \ret -> writeI ps ret
      return $ shiftRProgram ps'
    LoopL        -> case current $ getMemory ps of
                      0 -> return . seekLoopR $ shiftRProgram ps
                      _ -> return $ shiftRProgram ps
    LoopR        -> case current $ getMemory ps of
                       0 -> return $ shiftRProgram ps
                       _ -> return . seekLoopL . flip setCode ps $ shiftLCode code
    End          -> return ps
  where
    shiftRProgram :: ProgramState a -> ProgramState a
    shiftRProgram = flip execInstruction id
    execInstruction :: ProgramState a -> (ProgramState a -> ProgramState a) -> ProgramState a
    execInstruction ps@(getCode -> code) instruction = setCode (shiftRCode code) $ instruction ps

execProgram :: forall a m r. (Num a, Ord a, Eq a, Show a, Monad m) => Code
  -> Interrupt (ProgramState a) a m -- ^ Print interrupt
  -> Interrupt (ProgramState a) a m -- ^ Write interrupt
  -> m (ProgramState a)
execProgram code printI writeI = loop $ ProgramState (emptyMemory, code)
  where
    loop :: ProgramState a -> m (ProgramState a)
    loop ps@(getCode -> Code _ _ End) = return ps
    loop ps                           = (`runContT` loop) (run ps printI writeI)

execProgramDebug :: forall a m r. (Num a, Ord a, Eq a, Show a, MonadIO m) => Code
  -> Interrupt (ProgramState a) a m -- ^ Print interrupt
  -> Interrupt (ProgramState a) a m -- ^ Write interrupt
  -> m (ProgramState a)
execProgramDebug code printI writeI = loop $ ProgramState (emptyMemory, code)
  where
    loop :: ProgramState a -> m (ProgramState a)
    loop ps@(getCode -> Code _ _ End) = return ps
    loop ps                           = do
      ps' <- (`runContT` return) (run ps printI writeI)
      print ps'
      _ <- liftIO getLine
      loop ps'
