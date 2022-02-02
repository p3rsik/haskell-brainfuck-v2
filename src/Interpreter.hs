module Interpreter
  ( runInterpreter
  , runCommand
  )
where

import           Control.Monad.Free.Church
import           Language
import           Relude

interpretCommandL :: CommandL a -> IO a
interpretCommandL = undefined

runCommand :: Command a -> IO a
runCommand = foldF interpretCommandL

interpretInterpreterL :: InterpreterL a -> IO a
interpretInterpreterL (CheckSyntax code next) = do
  undefined
interpretInterpreterL (EvalCommand code next) = do
  undefined

runInterpreter :: Interpreter a -> IO a
runInterpreter = foldF interpretInterpreterL
