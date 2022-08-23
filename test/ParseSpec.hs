module ParseSpec
  ( spec
  )
where

import           Relude
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Interpreter.Data      (Code (..), Command (..), Commands (..),
                                        ProgramVerified (..))
import           Interpreter.Parse

spec :: Spec
spec = describe "Parse module" $ do
  context "parse function" $ do
    let testCases = [ (ProgramVerified ""
                      , Code (Commands []) (Commands []) End)
                    , (ProgramVerified "[]"
                      , Code (Commands [LoopR, End]) (Commands []) LoopL)
                    , (ProgramVerified "[+]->+<,."
                      , Code (Commands [ChangeCell 1, LoopR, ChangeCell (-1), MoveCell 1, ChangeCell 1, MoveCell (-1), WriteCell, PrintCell, End]) (Commands []) LoopL)
                    ]
    prop "parses correctly"
      . forAll (elements testCases)
      $ \(testCase, result) -> parse testCase `shouldBe` result

