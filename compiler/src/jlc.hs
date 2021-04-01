{-# LANGUAGE PatternSynonyms #-}

import Control.Exception
import System.IO
import System.Exit        (exitSuccess)

import System.Environment (getArgs)
import System.Exit        
import System.IO.Error    (isUserError, ioeGetErrorString)

import Javalette.Par            (pProg, myLexer)
import Javalette.ErrM           (pattern Ok, pattern Bad)

import TypeChecker
--import Interpreter

-- | Parse, type check, and interpret a program given by the @String@.

check :: String -> IO ExitCode
check s = do
  case pProg (myLexer s) of
    Bad err  -> do
      hPutStrLn stderr "ERROR"
      --putStrLn err
      exitFailure
    Ok  tree -> do
      case typecheck tree of
        Bad err -> do
          hPutStrLn stderr "ERROR"
          --putStrLn err
          exitFailure
        Ok _ -> do
          hPutStrLn stderr "OK"
          exitSuccess

-- | Main: read file passed by only command line argument and call 'check'.

main :: IO ExitCode
main = do
  args <- getArgs
  case args of
    [file] -> readFile file >>= check
    _      -> do
      putStrLn "Usage: jlc <SourceFile>"
      exitFailure
