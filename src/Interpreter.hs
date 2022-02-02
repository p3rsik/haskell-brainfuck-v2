module Interpreter
  ( runInterpreter
  , runCommand
  )
where

import           Control.Monad.Free.Church
import           Language
import           Relude
import           Syntax                    (checkSyntax)

interpretCommandL :: CommandL a -> IO a
interpretCommandL = error "Not Implemented"

runCommand :: Command a -> IO a
runCommand = foldF interpretCommandL

interpretInterpreterL :: InterpreterL a -> IO a
interpretInterpreterL (CheckSyntaxL code next) = do
  return . next $ checkSyntax code
interpretInterpreterL (ParseL code next) = do
  error "Not Implemented"
interpretInterpreterL (EvalCommandL code next) = do
  error "Not Implemented"

runInterpreter :: Interpreter a -> IO a
runInterpreter = foldF interpretInterpreterL
