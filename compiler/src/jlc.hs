{-# LANGUAGE PatternSynonyms #-}

import Control.Exception
import System.IO
import System.Exit        (exitSuccess)

import System.Environment (getArgs)
import System.Exit        
import System.IO.Error    (isUserError, ioeGetErrorString)

import Javalette.Par            (pProg, myLexer)
import Javalette.ErrM           (pattern Ok, pattern Bad)
import Javalette.Abs

import TypeChecker
--import writeX86Backend
import X86Backend

main :: IO ExitCode
main = do

  ----------- for running 1 specific test in the docker ------------------------
  program <- readFile "testsuite/good/core031.jl"


  -------------------------- for debugging ----------------------------------------
  --program <- readFile "../../test/testsuite/good/core031.jl"


  ---------------- for verifying that the register allocation works ---------------
  --program <- readFile "10.jl"


  ------------------ for using the docker like normal ----------------------------
  --program <- hGetContents stdin
  
  check program

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
          writeX86 tree

-- take type annotates syntax tree, compile it to llvm code and write to file + stdout
writeX86 :: Prog -> IO ExitCode
writeX86 p = do
  let code = compile p
  writeFile "foo.s" code -- file for debugging
  putStrLn code
  exitSuccess