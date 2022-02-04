module SyntaxSpec
  ( spec
  )
where

import           Relude
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Interpreter.Syntax    (SyntaxError (..), checkSyntax)

spec :: Spec
spec = describe "Syntax module" $ do
  context "checkSyntax function" $ do
    let testCasesSuccess = [ ""
                           , "[[]]"
                           , "+>>[+[-]]>>[]"
                           , "[ [][] ]"
                           ]
        testCasesFailure = [ "["
                           , "[[]"
                           , "[[][]"
                           , "[]]["
                           , "["
                           , "]"
                           ]
    prop "when provided with correct input"
      . forAll (elements testCasesSuccess)
      $ \c -> checkSyntax c `shouldBe` Right c

    prop "when provided with incorrect input"
      . forAll (elements testCasesFailure)
      $ \c -> checkSyntax c `shouldBe` Left NotMatchingBrackets
