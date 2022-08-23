module RunSpec
  ( spec
  )
where

import           Control.Monad.Cont
import           Data.Text             (replicate)
import           Relude                hiding (replicate)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Interpreter.Parse     (parse)
import           Interpreter.Run
import           Interpreter.Types

-- dummy interrupts
printC :: PrintInterrupt r Identity a
printC _ f = f ()
writeC :: (Num a, Show a, Ord a) => Char -> WriteInterrupt r Identity a
writeC i f = f i
writeC' = writeC 'a'

shiftCodeToEnd :: Code -> Code
shiftCodeToEnd c@(Code _ _ End) = c
shiftCodeToEnd code             = shiftCodeToEnd $ shiftRCode code

spec :: Spec
spec = describe "Checking Run module with Word8 as a memory cell type" $ do
  context "run function" $ do
    it "'+' instruction" $ do
      let initialCode = parse $ ProgramVerified "+"
          initialMem = emptyMemory @Word8
          initialPs = ProgramState initialMem initialCode
          finalPs = let p = change 1 initialPs in flip setCode p . shiftRCode $ getCode p

          ps = (`runCont` id) $ run initialPs printC writeC'
      ps `shouldBe` finalPs
    it "'-' instruction" $ do
      let initialCode = parse $ ProgramVerified"-"
          initialMem = emptyMemory @Word8
          initialPs = ProgramState initialMem initialCode
          finalPs = let p = change (-1) initialPs in flip setCode p . shiftRCode $ getCode p

          ps = (`runCont` id) $ run initialPs printC writeC'
      ps `shouldBe` finalPs
    it "'>' instruction" $ do
      let initialCode = parse $ ProgramVerified ">"
          initialMem = emptyMemory @Word8
          initialPs = ProgramState initialMem initialCode
          finalPs = let p = move 1 initialPs in flip setCode p . shiftRCode $ getCode p

          ps = (`runCont` id) $ run initialPs printC writeC'
      ps `shouldBe` finalPs
    it "'<' instruction" $ do
      let initialCode = parse $ ProgramVerified "<"
          initialMem = emptyMemory @Word8
          initialPs = ProgramState initialMem initialCode
          finalPs = let p = move (-1) initialPs in flip setCode p . shiftRCode $ getCode p

          ps = (`runCont` id) $ run initialPs printC writeC'
      ps `shouldBe` finalPs
    it "'<' instruction" $ do
      let initialCode = parse $ ProgramVerified "<"
          initialMem = emptyMemory @Word8
          initialPs = ProgramState initialMem initialCode
          finalPs = let p = move (-1) initialPs in flip setCode p . shiftRCode $ getCode p

          ps = (`runCont` id) $ run initialPs printC writeC'
      ps `shouldBe` finalPs
    prop "',' instruction" $ \i -> do
      let initialCode = parse $ ProgramVerified ","
          initialMem = emptyMemory @Word8
          initialPs = ProgramState initialMem initialCode
          finalPs = let p = writeCell i initialPs in flip setCode p . shiftRCode $ getCode p

          ps = (`runCont` id) $ run initialPs printC (writeC i)
      ps `shouldBe` finalPs
    it "'.' instruction" $ do
      let initialCode = parse $ ProgramVerified "."
          initialMem = emptyMemory @Word8
          initialPs = ProgramState initialMem initialCode
          finalPs = flip setCode initialPs . shiftRCode $ getCode initialPs

          ps = (`runCont` id) $ run initialPs printC writeC'
      ps `shouldBe` finalPs
  context "execProgram function" $ do
    prop "arbitrary number of '+' as a code"
      . forAll (choose (1, 99))
      $ \n -> do
        let initialCode = parse . ProgramVerified $ replicate n "+"
            initialMem = emptyMemory @Word8
            initialPs = ProgramState initialMem initialCode
            finalPs = let p = change n initialPs in flip setCode p . shiftCodeToEnd $ getCode p
            ps = runIdentity $ execProgram initialCode printC writeC'
        ps `shouldBe` finalPs

