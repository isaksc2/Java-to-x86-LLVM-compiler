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
import LlvmBackend
--import Interpreter

-- take type annotates syntax tree, compile it to llvm code and write to file
llvm :: Prog -> IO ()
llvm p = do
  let code = compile "placeholder" p
  writefile "placeholder.ll" code

-- | Parse and type check a program given by the @String@.
check :: String -> IO ExitCode
check s = do
  case pProg (myLexer s) of
    Bad err  -> do
      hPutStrLn stderr "ERROR" -- syntax error
      exitFailure
    Ok  tree -> do
      case typecheck tree of
        Bad err -> do
          hPutStrLn stderr "ERROR" -- type error 
          exitFailure
        Ok tree -> do
          hPutStrLn stderr "OK"
          llvm tree
          exitSuccess

-- | Main: read file passed by only command line argument and call 'check'.

main :: IO ExitCode
main = do
  program <- hGetContents stdin
  check program
