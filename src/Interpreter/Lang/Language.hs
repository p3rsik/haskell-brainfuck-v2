module Interpreter.Lang.Language
  ( InterpreterL (..),
    Interpreter (..),
    printGreetings,
    checkSyntax,
    parse,
    runProgram,
    debugProgram,
    selectProgram,
    throwException,
  )
where

import           Control.Monad.Free.Church
import           Interpreter.Types
import           Relude

data InterpreterL next where
  PrintGreetingsL :: (() -> next) -> InterpreterL next
  CheckSyntaxL :: UnverifiedProgram -> (Either SyntaxError VerifiedProgram -> next) -> InterpreterL next
  ParseL :: VerifiedProgram -> (ProgramCode -> next) -> InterpreterL next
  RunProgramL :: ProgramCode -> (() -> next) -> InterpreterL next -- interpret program
  DebugProgramL :: ProgramCode -> (() -> next) -> InterpreterL next -- step-by-step execution for debug purpouses
  SelectProgramL :: (Either InterpreterError UnverifiedProgram -> next) -> InterpreterL next -- select program
  ThrowExceptionL :: forall a e next. Exception e => e -> (a -> next) -> InterpreterL next -- throw exception from inside the interpreter

instance Functor InterpreterL where
  fmap f (PrintGreetingsL next)     = PrintGreetingsL $ f . next
  fmap f (CheckSyntaxL code next)   = CheckSyntaxL code $ f . next
  fmap f (ParseL text next)         = ParseL text $ f . next
  fmap f (RunProgramL code next)    = RunProgramL code $ f . next
  fmap f (DebugProgramL code next)  = DebugProgramL code $ f . next
  fmap f (SelectProgramL next)      = SelectProgramL $ f . next
  fmap f (ThrowExceptionL exc next) = ThrowExceptionL exc $ f . next

type Interpreter a = F InterpreterL a

printGreetings :: Interpreter ()
printGreetings = liftF $ PrintGreetingsL id

checkSyntax :: UnverifiedProgram -> Interpreter (Either SyntaxError VerifiedProgram)
checkSyntax code = liftF $ CheckSyntaxL code id

parse :: VerifiedProgram -> Interpreter ProgramCode
parse code = liftF $ ParseL code id

runProgram :: ProgramCode -> Interpreter ()
runProgram code = liftF $ RunProgramL code id

debugProgram :: ProgramCode -> Interpreter ()
debugProgram code = liftF $ DebugProgramL code id

selectProgram :: Interpreter (Either InterpreterError UnverifiedProgram)
selectProgram = liftF $ SelectProgramL id

throwException :: forall a e. Exception e => e -> Interpreter a
throwException ex = liftF $ ThrowExceptionL ex id
