module Interpreter.Run
  ( run
  , move
  , change
  )
where

import           Control.Monad.Cont
import           Relude

import           Interpreter.Data
import           Interpreter.Util


move, change :: forall a. (Num a, Eq a, Ord a) => a -> ProgramState a -> ProgramState a
move n s@(getMemory -> mem) = flip setMemory s $ move' n mem
  where
    move' :: a -> Memory a -> Memory a
    move' 0 mem = mem
    move' i Memory {..} =
      if i < 0
      then move' (i + 1) $ Memory (current:left) (unsafeTail right) (unsafeHead right)
      else move' (i - 1) $ Memory (unsafeTail left) (current:right) (unsafeHead left)
change i s@(getMemory -> Memory {..}) = flip setMemory s $ Memory left right (current + i)

printCell :: Memory a -> a
printCell Memory {..} = current

writeCell :: a -> Memory a -> Memory a
writeCell a Memory {..} = Memory {current = a, ..}

type Interrupt r a = ProgramState a -> Cont r (ProgramState a) -> Cont r (ProgramState a)

run :: (Num a, Ord a, Eq a) => ProgramState a
  -> Interrupt r a -- ^ Print interrupt
  -> Interrupt r a -- ^ Write interrupt
  -> Cont r (ProgramState a)
run ps printC writeC = let code = getCode ps in
  case currentInstruction $ getCode ps of
    MoveCell n   -> return . setCode (shiftRCode code) $ move (fromIntegral n) ps
    ChangeCell n -> return . setCode (shiftRCode code) $ change (fromIntegral n) ps
    PrintCell    -> undefined
    WriteCell    -> undefined
    LoopL        -> undefined
    LoopR        -> undefined
    End          -> return ps
