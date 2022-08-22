module Interpreter.Syntax
  ( checkSyntax
  )
where

import qualified Data.Text        as T
import           Relude

import           Interpreter.Data (ProgramUnverified (..), ProgramVerified (..),
                                   SyntaxError (..))

-- Checks syntax for the given 'Text'
checkSyntax :: ProgramUnverified -> Either SyntaxError ProgramVerified
checkSyntax (ProgramUnverified code) =
  bool (Left NotMatchingBrackets) (Right $ ProgramVerified code)
  . (== Just 0) -- if acc is 0 then every '[' has closing ']'
  . T.foldl (\accM c -> do
                acc <- accM
                when (acc < 0) Nothing -- if acc is less than 0, than we got something like ][
                if c == '['
                  then return $ acc + 1 -- for every '[' increment the acc by one
                  else return $ acc - 1) -- for every ']' decrement the acc by one
            (Just 0)
  $ T.filter (\c -> c == '[' || c == ']') code -- leave only brackets
