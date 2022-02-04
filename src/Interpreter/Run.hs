module Interpreter.Run
  ( run
  , move
  , change
  , writeCell
  , Interrupt
  , execProgram
  )
where

import           Control.Monad.Cont
import           Relude

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

printCell :: ProgramState a -> a
printCell (getMemory -> Memory {..}) = current

writeCell :: (Num a, Show a, Ord a) => a -> ProgramState a -> ProgramState a
writeCell a ps = flip setMemory ps $ let Memory {..} = getMemory ps in Memory {current = a, ..}

type Interrupt r a = ProgramState a -- ^ state of program at the moment of interrupt
  -> (ProgramState a -> Cont r (ProgramState a)) -- ^ return point
  -> Cont r (ProgramState a)

run :: (Num a, Ord a, Eq a) => ProgramState a
  -> Interrupt r a -- ^ Print interrupt
  -> Interrupt r a -- ^ Write interrupt
  -> Cont r (ProgramState a)
run ps printI writeI = let code = getCode ps in
  case currentInstruction $ getCode ps of
    MoveCell n   -> return . setCode (shiftRCode code) $ move (fromIntegral n) ps
    ChangeCell n -> return . setCode (shiftRCode code) $ change (fromIntegral n) ps
    PrintCell    -> do
      ps' <- callCC $ \ret -> printI ps ret
      return $ setCode (shiftRCode code) ps'
    WriteCell    -> do
      ps' <- callCC $ \ret -> writeI ps ret
      return $ setCode (shiftRCode code) ps'
    LoopL        -> undefined
    LoopR        -> undefined
    End          -> return ps

execProgram :: forall a r. (Num a, Ord a, Eq a, Show a) => Code
  -> Interrupt (ProgramState a) a -- ^ Print interrupt
  -> Interrupt (ProgramState a) a -- ^ Write interrupt
  -> ProgramState a
execProgram code printI writeI = loop $ ProgramState (emptyMemory, code)
  where
    loop :: ProgramState a -> ProgramState a
    loop ps@(getCode -> Code _ _ End) = ps
    loop ps = loop . (`runCont` id) $ run ps printI writeI
