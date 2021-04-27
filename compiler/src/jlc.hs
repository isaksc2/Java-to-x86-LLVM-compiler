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
  let code = compile "main" p
  writeFile "main.ll" code
  putStrLn code
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
  --check "/* Test boolean operators */int main () {int x = 4;if (3 <= x && 4 != 2 && true) {printBool(true);} else {printString(\"apa\");}printBool(true == true || dontCallMe(1));printBool(4.0 < -50.0 && dontCallMe(2));printBool(4 == x && true == !false && true);printBool(implies(false,false));printBool(implies(false,true));printBool(implies(true,false));printBool(implies(true,true));return 0 ;}boolean dontCallMe(int x) {printInt(x);return true;}void printBool(boolean b) {if (b) {printString(\"true\");} else {printString(\"false\");}return;} boolean implies(boolean x, boolean y) {return !x || x == y;}"
  --check "int main() { if (true){} return 0; }"
  --check "int main() {p();printInt(1);return 0;}void p() {}"
  --check "int main() {int x;x = 5;while (x > 0) {printInt(x);x--;}printInt(x);return 0;}"
  --check "/* Test arithmetic and comparisons. */int main() {    int x = 56;    int y = 1+2+3+42+23;    printInt(x+y);    printInt(x-y);    printInt(x*y);    printInt(45/2);    printInt(78%3);    double z = -9.3;    double w = 5.1;    printBool(z+w > z-w);    printBool(z/w <= z*w);    return 0 ;}void printBool(boolean b) {  if (b) {    printString(\"true\");return;} else {printString(\"false\");return;}}"
  check "int f () {}int g () {if (false) {}else return 0;}void p () {}int main() { return 0;}"
  --check "int main() {return 0;}"
  --check program
