module Interpreter.Parse
  ( parse
  )
where

import           Relude

import qualified Data.Text        as T
import           Interpreter.Data (Code (..), Command (..))

-- Parses given 'Text' into a 'Code'
-- This operation does not check syntax, only converts given 'Text' into a 'Code'
-- skipping any charactes that aren't valid Brainfuck instructions
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
