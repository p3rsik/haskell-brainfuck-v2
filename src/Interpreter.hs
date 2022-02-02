module Interpreter
  ()
where

import           Control.Monad.Free.Church
import           Language
import           Relude

interpretCommandL :: CommandL a next -> IO next
interpretCommandL = undefined

interpretInterpreterL :: InterpreterL a -> IO a
interpretInterpreterL (CheckSyntax code next) = do
  undefined
interpretInterpreterL (EvalCommand code next) = do
  undefined

runInterpreter :: Interpreter a -> IO a
runInterpreter = foldF interpretInterpreterL
