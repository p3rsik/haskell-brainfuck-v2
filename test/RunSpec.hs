module RunSpec
  ( spec
  )
where

import           Control.Monad.Cont
import           Relude
import           Test.Hspec
import           Test.Hspec.QuickCheck

import           Interpreter.Data      (Code (..), Command (..),
                                        ProgramState (..), emptyMemory, getCode,
                                        setCode, shiftRCode)
import           Interpreter.Parse     (parse)
import           Interpreter.Run

-- dummy interrupts
printC :: Interrupt r a
printC ps f = f ps
writeC :: (Num a, Show a, Ord a) => Word8 -> Interrupt r a
writeC i ps f = f $ writeCell (fromIntegral i) ps
writeC' = writeC 0

spec :: Spec
spec = describe "Checking Run module" $ do
  context "step-by-step execution of run function" $ do
    it "'+' instruction" $ do
      let initialCode = parse "+"
          initialMem = emptyMemory @Word8
          initialPs = ProgramState (initialMem, initialCode)
          finalPs = let p = change 1 initialPs in flip setCode p . shiftRCode $ getCode p

          ps = (`runCont` id) $ run initialPs printC writeC'
      ps `shouldBe` finalPs
    it "'-' instruction" $ do
      let initialCode = parse "-"
          initialMem = emptyMemory @Word8
          initialPs = ProgramState (initialMem, initialCode)
          finalPs = let p = change (-1) initialPs in flip setCode p . shiftRCode $ getCode p

          ps = (`runCont` id) $ run initialPs printC writeC'
      ps `shouldBe` finalPs
    it "'>' instruction" $ do
      let initialCode = parse ">"
          initialMem = emptyMemory @Word8
          initialPs = ProgramState (initialMem, initialCode)
          finalPs = let p = move 1 initialPs in flip setCode p . shiftRCode $ getCode p

          ps = (`runCont` id) $ run initialPs printC writeC'
      ps `shouldBe` finalPs
    it "'<' instruction" $ do
      let initialCode = parse "<"
          initialMem = emptyMemory @Word8
          initialPs = ProgramState (initialMem, initialCode)
          finalPs = let p = move (-1) initialPs in flip setCode p . shiftRCode $ getCode p

          ps = (`runCont` id) $ run initialPs printC writeC'
      ps `shouldBe` finalPs
    it "'<' instruction" $ do
      let initialCode = parse "<"
          initialMem = emptyMemory @Word8
          initialPs = ProgramState (initialMem, initialCode)
          finalPs = let p = move (-1) initialPs in flip setCode p . shiftRCode $ getCode p

          ps = (`runCont` id) $ run initialPs printC writeC'
      ps `shouldBe` finalPs
    prop "',' instruction" $ \i -> do
      let initialCode = parse ","
          initialMem = emptyMemory @Word8
          initialPs = ProgramState (initialMem, initialCode)
          finalPs = let p = writeCell i initialPs in flip setCode p . shiftRCode $ getCode p

          ps = (`runCont` id) $ run initialPs printC (writeC i)
      ps `shouldBe` finalPs
