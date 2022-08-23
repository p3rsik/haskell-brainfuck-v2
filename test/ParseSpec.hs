module ParseSpec
  ( spec
  )
where

import           Relude
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Interpreter.Parse
import           Interpreter.Types

spec :: Spec
spec = describe "Parse module" $ do
  context "parse function" $ do
    let testCases = [ (VerifiedProgram ""
                      , ProgramCode emptyCode emptyCode End)
                    , (VerifiedProgram "[]"
                      , ProgramCode (Code [LoopR, End]) emptyCode LoopL)
                    , (VerifiedProgram "[+]->+<,."
                      , ProgramCode (Code [ChangeCell 1, LoopR, ChangeCell (-1), MoveCell 1, ChangeCell 1, MoveCell (-1), WriteCell, PrintCell, End]) emptyCode LoopL)
                    ]
    prop "parses correctly"
      . forAll (elements testCases)
      $ \(testCase, result) -> parse testCase `shouldBe` result

