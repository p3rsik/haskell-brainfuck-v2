module ParseSpec
  ( spec
  )
where

import           Relude
import           Test.Hspec

import           Interpreter.Data  (Code (..), Command (..))
import           Interpreter.Parse

spec :: Spec
spec = describe "parse" $ do
  it "parses correctly" $ do
    let testCases = [ ""
                    , "[]"
                    , "[+]->+<,."
                    ]
        results = [ Code [] [] End
                  , Code [LoopR, End] [] LoopL
                  , Code [ChangeCell 1, LoopR, ChangeCell (-1), MoveCell 1, ChangeCell 1, MoveCell (-1), WriteCell, PrintCell, End] [] LoopL
                  ]
    parse <$> testCases `shouldBe` results

