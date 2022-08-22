module Interpreter.Parse
  ( parse,
  )
where

import qualified Data.Text        as T
import           Interpreter.Data (Code (..), Command (..), Commands (..),
                                   ProgramVerified (..))
import           Relude

-- Parses given 'Text' into a 'Code'
-- This operation does not check syntax, only converts given 'Text' into a 'Code'
-- skipping any charactes that aren't valid Brainfuck instructions
parse :: ProgramVerified -> Code
parse = toCode . catMaybes . T.foldr (\el acc -> convert el : acc) [] . unProgramVerified
  where
    convert :: Char -> Maybe Command
    convert '+' = return $ ChangeCell 1
    convert '-' = return . ChangeCell $ -1
    convert '[' = return LoopL
    convert ']' = return LoopR
    convert '.' = return PrintCell
    convert ',' = return WriteCell
    convert '>' = return $ MoveCell 1
    convert '<' = return . MoveCell $ -1
    convert _   = Nothing

    toCode :: [Command] -> Code
    toCode (x : xs) = Code (Commands $ xs <> [End]) (Commands []) x
    toCode _        = Code (Commands []) (Commands []) End
