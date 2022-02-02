module Data
  ( Command (..)
  , Code (..)
  , Memory (..)
  , SyntaxError (..)
  , emptyMemoryWord8
  )
where

import           Relude

-- Commands that are used in brainfuck language
data Command = MoveCell Int -- > or <
             | ChangeCell Int -- + or -
             | PrintCell -- .
             | WriteCell -- ,
             | LoopL -- [
             | LoopR -- ]
             | End -- just the end of the commands
             deriving (Show, Eq)

-- handy type allias for sequence of commands
type Code = [Command]

-- Memory cells of our brainfuck interpreter represented as an isInfinite tape.
data Memory a = Memory { left, right :: [a], current :: !a } deriving (Show)

emptyMemoryWord8 :: Memory Word8
emptyMemoryWord8 = Memory { left = zeroes, current = 0, right = zeroes}
  where
    zeroes = 0 : zeroes

data SyntaxError = NotMatchingBrackets deriving (Show, Eq)
