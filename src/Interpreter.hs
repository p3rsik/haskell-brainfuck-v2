module Interpreter
  ( runInterpreter
  , runCommand
  , module L
  )
where

import           Control.Exception         (throwIO)
import           Control.Monad.Free.Church
import           Data.Char                 (isNumber)
import           Data.List                 ((!!))
import           Relude
import           System.Directory          (listDirectory)

import           Interpreter.Language      as L
import           Interpreter.Parse         as P (parse)
import           Interpreter.Syntax        as S (checkSyntax)

interpretCommandL :: CommandL a -> IO a
interpretCommandL = error "Not Implemented"

runCommand :: L.Command a -> IO a
runCommand = foldF interpretCommandL

interpretInterpreterL :: InterpreterL a -> IO a
interpretInterpreterL (PrintGreetingsL next) = do
  putStrLn "Hello!\nThis is a brainfuck interpreter!"
  return $ next ()
interpretInterpreterL (CheckSyntaxL code next) = do
  return . next $ S.checkSyntax code
interpretInterpreterL (ParseL code next) = do
  return . next $ P.parse code
interpretInterpreterL (EvalCommandL code next) = do
  error "Not Implemented"

interpretInterpreterL (SelectProgramL next) = do
  putStrLn "Choose program to run(input number): "

  progs <- liftIO $ listDirectory "progs"
  forM_ (zip [1..] progs) $ \(num, prog) -> do
    putStrLn $ show num <> ") " <> prog

  choice <- getLine

  case readMaybe $ toString choice :: Maybe Int of
    Just num -> if num <= length progs
                then do
                  let prog = progs !! (num + 1)
                  next . Right <$> readFileText ("progs/" <> prog)
                else return . next . Left $ FileError "Wrong number!"
    Nothing -> return . next . Left $ FileError "It's not a number!"

interpretInterpreterL (RunProgramL code next) = do
  error "Not Implemented"
interpretInterpreterL (ThrowExceptionL exc next) = throwIO exc


runInterpreter :: Interpreter a -> IO a
runInterpreter = foldF interpretInterpreterL
