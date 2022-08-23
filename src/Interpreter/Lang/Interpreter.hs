module Interpreter.Lang.Interpreter
  ( runInterpreter,
  )
where

import           Control.Exception         (throwIO)
import           Control.Monad.Free.Church (foldF)
import           Data.Char                 (isNumber)
import           Data.List                 ((!!))
import           Interpreter.Lang.Language
import           Interpreter.Parse         as P (parse)
import           Interpreter.Run           (execProgram, execProgramDebug,
                                            printCell, run, writeCell)
import           Interpreter.Syntax        as S (checkSyntax)
import           Interpreter.Types
import           Relude
import           System.Directory          (listDirectory)
import           System.IO                 (getChar, putChar)

interpretInterpreterL :: InterpreterL a -> IO a
interpretInterpreterL (PrintGreetingsL next) = do
  putStrLn "Hello!\nThis is a brainfuck interpreter!"
  return $ next ()
interpretInterpreterL (CheckSyntaxL code next) = do
  return . next $ S.checkSyntax code
interpretInterpreterL (ParseL code next) = do
  return . next $ P.parse code
interpretInterpreterL (SelectProgramL next) = do
  putStrLn "Choose program to run(input number): "

  progs <- liftIO $ listDirectory "progs"
  forM_ (zip [1 ..] progs) $ \(num, prog) -> do
    putStrLn $ show num <> ") " <> prog

  choice <- getLine

  case readMaybe $ toString choice :: Maybe Int of
    Just num ->
      if num <= length progs
        then do
          let prog = progs !! (num - 1)
          next . Right . UnverifiedProgram <$> readFileText ("progs/" <> prog)
        else return . next . Left $ FileError "No file under this number!"
    Nothing -> return . next . Left $ FileError "It's not a number!"
interpretInterpreterL (RunProgramL code next) = do
  ps <- execProgram code printInterruptIO writeInterruptIO
  return $ next ()
interpretInterpreterL (DebugProgramL code next) = do
  execProgramDebug code printInterruptIO writeInterruptIO
  return $ next ()
interpretInterpreterL (ThrowExceptionL exc next) = throwIO exc

runInterpreter :: Interpreter a -> IO a
runInterpreter = foldF interpretInterpreterL
