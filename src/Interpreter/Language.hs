module Interpreter.Language
  ( CommandL (..)
  , Command (..)
  , InterpreterL (..)
  , Interpreter (..)
  , InterpreterError (..)
  , printGreetings
  , checkSyntax
  , parse
  , evalCommand
  , runProgram
  , debugProgram
  , selectProgram
  , throwException
  )
where

import           Control.Monad.Free.Church
import           Relude

import           Interpreter.Data          (Code)
import           Interpreter.Syntax        (SyntaxError)

data CommandL next where
  MoveL :: Int -> (() -> next) -> CommandL next
  ChangeL :: Int -> (() -> next) -> CommandL next
  PrintL :: Show a => (a -> next) -> CommandL next
  WriteL :: (() -> next) -> CommandL next
  LoopL :: (() -> next) -> CommandL next

instance Functor CommandL where
  fmap f (MoveL i next)   = MoveL i $ f . next
  fmap f (ChangeL i next) = ChangeL i $ f . next
  fmap f (PrintL next)    = PrintL $ f . next
  fmap f (WriteL next)    =  WriteL $ f . next
  fmap f (LoopL next)     = LoopL $ f . next

type Command a = F CommandL a

data InterpreterError = SyntaxError SyntaxError -- Wrapper around 'SyntaxError'
  | FileError Text -- file not found, can't be read, etc
  deriving (Show, Eq, Generic, Exception)

data InterpreterL next where
  PrintGreetingsL :: (() -> next) -> InterpreterL next
  CheckSyntaxL :: Text -> (Either SyntaxError Text -> next) -> InterpreterL next
  ParseL :: Text -> (Code -> next) -> InterpreterL next
  EvalCommandL :: Command a -> (a -> next) -> InterpreterL next -- maybe repl?
  RunProgramL :: Code -> (() -> next) -> InterpreterL next -- interpret program
  DebugProgramL :: Code -> (() -> next) -> InterpreterL next -- step-by-step execution for debug purpouses
  SelectProgramL :: (Either InterpreterError Text -> next) -> InterpreterL next -- select program
  ThrowExceptionL :: forall a e next. Exception e => e -> (a -> next) -> InterpreterL next -- throw exception from inside the interpreter

instance Functor InterpreterL where
  fmap f (PrintGreetingsL next)      = PrintGreetingsL $ f . next
  fmap f (CheckSyntaxL code next)    = CheckSyntaxL code $ f . next
  fmap f (ParseL text next)          = ParseL text $ f . next
  fmap f (EvalCommandL command next) = EvalCommandL command $ f . next
  fmap f (RunProgramL code next)     = RunProgramL code $ f . next
  fmap f (DebugProgramL code next)   = DebugProgramL code $ f . next
  fmap f (SelectProgramL next)       = SelectProgramL $ f . next
  fmap f (ThrowExceptionL exc next)  = ThrowExceptionL exc $ f . next

type Interpreter a = F InterpreterL a

printGreetings :: Interpreter ()
printGreetings = liftF $ PrintGreetingsL id

checkSyntax :: Text -> Interpreter (Either SyntaxError Text)
checkSyntax code = liftF $ CheckSyntaxL code id

parse :: Text -> Interpreter Code
parse code = liftF $ ParseL code id

evalCommand :: Command a -> Interpreter a
evalCommand command = liftF $ EvalCommandL command id

runProgram :: Code -> Interpreter ()
runProgram code = liftF $ RunProgramL code id

debugProgram :: Code -> Interpreter ()
debugProgram code = liftF $ DebugProgramL code id

selectProgram :: Interpreter (Either InterpreterError Text)
selectProgram = liftF $ SelectProgramL id

throwException :: forall a e. Exception e => e -> Interpreter a
throwException ex = liftF $ ThrowExceptionL ex id
