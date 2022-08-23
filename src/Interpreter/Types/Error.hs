module Interpreter.Types.Error
  ( InterpreterError (..),
    SyntaxError (..)
  ) where

import           Relude


-- | Interpreter wide errors
data InterpreterError
  = SyntaxError SyntaxError -- Wrapper around 'SyntaxError'
  | FileError Text -- file not found, can't be read, etc
  | UnexpectedError Text
  deriving (Show, Eq, Generic, Exception)

-- | Error for 'checkSyntax' function
data SyntaxError = NotMatchingBrackets deriving (Show, Eq)
