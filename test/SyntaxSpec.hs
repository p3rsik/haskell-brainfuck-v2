module SyntaxSpec
  ( spec
  )
where

import           Relude
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Interpreter.Data      (ProgramUnverified (..),
                                        ProgramVerified (..), SyntaxError (..))
import           Interpreter.Syntax    (checkSyntax)

spec :: Spec
spec = describe "Syntax module" $ do
  context "checkSyntax function" $ do
    let testCasesSuccess = ProgramUnverified <$> [ ""
                           , "[[]]"
                           , "+>>[+[-]]>>[]"
                           , "[ [][] ]"
                           ]
        testCasesFailure = ProgramUnverified <$> [ "["
                           , "[[]"
                           , "[[][]"
                           , "[]]["
                           , "["
                           , "]"
                           ]
    prop "when provided with correct input"
      . forAll (elements testCasesSuccess)
      $ \c -> checkSyntax c`shouldBe` Right (ProgramVerified $ unProgramUnverified c)

    prop "when provided with incorrect input"
      . forAll (elements testCasesFailure)
      $ \c -> checkSyntax c `shouldBe` Left NotMatchingBrackets
