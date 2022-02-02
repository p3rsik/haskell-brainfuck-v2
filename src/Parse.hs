module Parse
  ( parse
  )
where

import           Data      (Code, Command (..))
import qualified Data.Text as T
import           Relude

parse :: Text -> Code
parse = catMaybes . T.foldr (\el acc -> convert el : acc) []
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
