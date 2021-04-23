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
import LlvmBackend
--import Interpreter

-- take type annotates syntax tree, compile it to llvm code and write to file
llvm :: Prog -> IO ExitCode
llvm p = do
  let code = compile "maaain" p
  writeFile "maaain.ll" code
  exitSuccess

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

-- | Main: read file passed by only command line argument and call 'check'.

main :: IO ExitCode
main = do
  program <- hGetContents stdin
  --check "int main(){ foo(); int a = 3; a = 2; double b = 3.0; double c = b*2.0;  return 0;} void lol(){while (false) {1; 2;} printString(\"foo2\"); printString(\"foasdasdo\"); printString(\"fssssssoo\");} void foo() { printString(\"foo\"); while (false) {printInt(1); printDouble(2.9);} return; int a = 3 + 3*4;}"
  --check "int main() {many_params(1,2,3,4,100.0,100.0,100.0,100.0,5,6,7,8,9,10,11,12,13,14,15,16);return 0;}void many_params(int x1, int x2, int x3, int x4,double d1, double d2, double d3, double d4,int y1, int y2, int y3, int y4,int z1, int z2, int z3, int z4,int q1, int q2, int q3, int q4) {printInt(x1);printInt(y1);printInt(z1);printInt(q1);printDouble(d1);if (x1 != 2) {many_params(q4,x1,x2,x3,d4/2.0,d1*2.0,d2+1.0,d3-0.0,x4,y1,y2,y3,y4,z1,z2,z3,z4,q1,q2,q3);}}"
  check program
