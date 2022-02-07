module Main where

import           Control.Exception (try)
import           Relude

import           Interpreter

main :: IO ()
main = do
  eResult <- try $ runInterpreter $ do
    printGreetings
    prog <- selectProgram >>= \case
                                Left err   -> throwException err
                                Right prog -> return prog
    verifiedProg <- checkSyntax prog >>= \case
                                          Left err   -> throwException $ SyntaxError err
                                          Right prog' -> return prog'
    code <- parse verifiedProg
    runProgram code
  case eResult of
    Right ok                    -> return ok
    Left (err :: SomeException) -> putStrLn $ "Got exception: " <> show err
