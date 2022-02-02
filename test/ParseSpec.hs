module ParseSpec
  ( spec
  )
where

import           Relude
import           Test.Hspec

import           Data       (Command (..))
import           Parse

spec :: Spec
spec =
  describe "parse" $ do
    it "parses correctly" $ do
      let testCases = [ ""
                      , "[]"
                      , "[+]->+<,."
                      ]
          results = [ []
                    , [LoopL, LoopR]
                    , [LoopL, ChangeCell 1, LoopR, ChangeCell (-1), MoveCell 1, ChangeCell 1, MoveCell (-1), WriteCell, PrintCell]
                    ]
      parse <$> testCases `shouldBe` results

