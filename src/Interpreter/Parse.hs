module Interpreter.Parse
  ( parse
  )
where

import           Relude

import qualified Data.Text        as T
import           Interpreter.Data (Code (..), Command (..))

parse :: Text -> Code
parse = toCode . catMaybes . T.foldr (\el acc -> convert el : acc) []
  where
    convert :: Char -> Maybe Command
    convert '+' = return $ ChangeCell  1
    convert '-' = return . ChangeCell $  -1
    convert '[' = return LoopL
    convert ']' = return LoopR
    convert '.' = return PrintCell
    convert ',' = return WriteCell
    convert '>' = return $ MoveCell 1
    convert '<' = return . MoveCell $ -1
    convert _   = Nothing

    toCode :: [Command] -> Code
    toCode (x:xs) = Code (xs <> [End]) [] x
    toCode _      = Code [] [] End
