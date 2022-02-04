module RunSpec
  ( spec
  )
where

import           Control.Monad.Cont
import           Relude
import           Test.Hspec

import           Interpreter.Data   (Code (..), Command (..), ProgramState (..),
                                     emptyMemory, shiftRCode, setCode, getCode)
import           Interpreter.Parse  (parse)
import           Interpreter.Run

spec :: Spec
spec = describe "Checking Run module" $ do
  context "step-by-step execution of run function" $ do
    it "'>+' instruction" $ do
      let initialCode = parse ">+"
          initialMem = emptyMemory @Word8
          initialPs = ProgramState (initialMem, initialCode)
          firstStepPs = let p = move 1 initialPs in flip setCode p . shiftRCode $ getCode p
          secondStepPs = change 1 firstStepPs

          ps = (`runCont` id) $ run initialPs undefined undefined
          ps' = (`runCont` id) $ run ps undefined undefined
      print firstStepPs
      print ps
      ps `shouldBe` firstStepPs
      ps' `shouldBe` secondStepPs
