module ParseSpec
  ( spec
  )
where

import           Relude
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Interpreter.Data      (Code (..), Command (..))
import           Interpreter.Parse

spec :: Spec
spec = describe "Parse module" $ do
  context "parse function" $ do
    let testCases = [ (""
                      , Code [] [] End)
                    , ("[]"
                      , Code [LoopR, End] [] LoopL)
                    , ("[+]->+<,."
                      , Code [ChangeCell 1, LoopR, ChangeCell (-1), MoveCell 1, ChangeCell 1, MoveCell (-1), WriteCell, PrintCell, End] [] LoopL)
                    ]
    modifyMaxSuccess (const $ length testCases)
      . prop "parses correctly"
      . forAll (elements testCases)
      $ \(testCase, result) -> parse testCase `shouldBe` result

