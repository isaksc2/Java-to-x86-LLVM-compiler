{-# LANGUAGE PatternSynonyms #-}

import Control.Exception

import System.Environment (getArgs)
import System.Exit        (exitFailure)
import System.IO.Error    (isUserError, ioeGetErrorString)

import Javalette.Par            (pProg, myLexer)
import Javalette.ErrM           (pattern Ok, pattern Bad)

import TypeChecker
--import Interpreter

-- | Parse, type check, and interpret a program given by the @String@.

check :: String -> IO ()
check s = do
  case pProg (myLexer s) of
    Bad err  -> do
      putStrLn "SYNTAX ERROR"
      putStrLn err
      exitFailure
    Ok  tree -> do
      case typecheck tree of
        Bad err -> do
          putStrLn "TYPE ERROR"
          putStrLn err
          exitFailure
        Ok _ -> 
          putStrLn "OK"

-- | Main: read file passed by only command line argument and call 'check'.

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> readFile file >>= check
    _      -> do
      putStrLn "Usage: jlc <SourceFile>"
      exitFailure
