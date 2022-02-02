{-# LANGUAGE GADTs #-}

module Language
  ( CommandL (..)
  , Command (..)
  , InterpreterL (..)
  , Interpreter (..)
  )
where

import           Control.Monad.Free.Church
import           Data                      (Code, SyntaxError)
import           Relude

data CommandL a next where
  MoveL :: Int -> (() -> next) -> CommandL a next
  ChangeL :: Int -> (() -> next) -> CommandL a next
  PrintL :: (a -> next) -> CommandL a next
  WriteL :: (() -> next) -> CommandL a next
  LoopL :: (() -> next) -> CommandL a next

instance Functor (CommandL a) where
  fmap f (MoveL i next)   = MoveL i $ f . next
  fmap f (ChangeL i next) = ChangeL i $ f . next
  fmap f (PrintL next)    = PrintL $ f . next
  fmap f (WriteL next)    =  WriteL $ f . next
  fmap f (LoopL next)     = LoopL $ f . next

type Command memType a = F (CommandL memType) a

data InterpreterL next where
  CheckSyntax :: Code -> (Either SyntaxError Code -> next) -> InterpreterL next
  EvalCommand :: Command memType () -> (() -> next) -> InterpreterL next

instance Functor InterpreterL where
  fmap f (CheckSyntax code next)    = CheckSyntax code $ f . next
  fmap f (EvalCommand command next) = EvalCommand command $ f . next

type Interpreter a = F InterpreterL a
