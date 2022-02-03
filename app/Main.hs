module Main where

import           Relude

import           Interpreter (InterpreterError (..), checkSyntax, parse,
                              printGreetings, runInterpreter, runProgram,
                              selectProgram, throwException)

main :: IO ()
main = runInterpreter $ do
  printGreetings
  prog <- selectProgram >>= \case
                              Left err   -> throwException err
                              Right prog -> return prog
  verifiedProg <- checkSyntax prog >>= \case
                                         Left err   -> throwException $ SyntaxError err
                                         Right prog -> return prog
  code <- parse verifiedProg
  runProgram code
