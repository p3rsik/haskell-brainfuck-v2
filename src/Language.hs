{-# LANGUAGE GADTs #-}

module Language
  ( CommandL (..)
  , Command (..)
  , InterpreterL (..)
  , Interpreter (..)
  )
where

import           Control.Monad.Free.Church
import           Data                      (Code)
import           Relude
import           Syntax                    (SyntaxError)

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

data InterpreterL next where
  CheckSyntaxL :: Text -> (Either SyntaxError Text -> next) -> InterpreterL next
  ParseL :: Text -> (Code -> next) -> InterpreterL next
  EvalCommandL :: Command () -> (() -> next) -> InterpreterL next

instance Functor InterpreterL where
  fmap f (CheckSyntaxL code next)    = CheckSyntaxL code $ f . next
  fmap f (ParseL text next)          = ParseL text $ f . next
  fmap f (EvalCommandL command next) = EvalCommandL command $ f . next

type Interpreter a = F InterpreterL a
