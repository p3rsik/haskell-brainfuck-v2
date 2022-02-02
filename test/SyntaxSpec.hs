module SyntaxSpec
  ( spec
  )
where

import           Relude
import           Test.Hspec

import           Syntax     (SyntaxError (..), checkSyntax)

spec :: Spec
spec =
  describe "checkSyntax" $ do
    context "when provided with correct input" $ do
      it "returns code back" $ do
        let testCases = [ ""
                        , "[[]]"
                        , "+>>[+[-]]>>[]"
                        , "[ [][] ]"
                        ]
        checkSyntax <$> testCases `shouldBe` Right <$> testCases
    context "when provided with incorrect input" $ do
      it "returns error" $ do
        let testCases = [ "["
                        , "[[]"
                        , "[[][]"
                        , "[]]["
                        , "["
                        , "]"
                        ]
        forM_ (checkSyntax <$> testCases) (`shouldBe` (Left NotMatchingBrackets))
