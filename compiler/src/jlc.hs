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
--import LlvmBackend
import X86Backend

main :: IO ExitCode
main = do
  --program <- hGetContents stdin
  --program <- readFile "test.c"
  program <- readFile "../../test/testsuite/good/core031.jl"
  check program
  --check "int main() { if (true == true) {printInt(42);} return 0;}"

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
          --exitSuccess

-- take type annotates syntax tree, compile it to llvm code and write to file
llvm :: Prog -> IO ExitCode
llvm p = do
  let code = compile p
  --writeFile "main.ll" code -- file for debugging
  writeFile "assembly.asm" code -- file for debugging
  putStrLn code
  exitSuccess