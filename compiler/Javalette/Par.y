-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Javalette.Par where
import qualified Javalette.Abs
import Javalette.Lex
}

%name pProg Prog
-- no lexer declaration
%monad { Either String } { (>>=) } { return }
%tokentype {Token}
%token
  '!' { PT _ (TS _ 1) }
  '!=' { PT _ (TS _ 2) }
  '%' { PT _ (TS _ 3) }
  '&&' { PT _ (TS _ 4) }
  '(' { PT _ (TS _ 5) }
  ')' { PT _ (TS _ 6) }
  '*' { PT _ (TS _ 7) }
  '+' { PT _ (TS _ 8) }
  '++' { PT _ (TS _ 9) }
  ',' { PT _ (TS _ 10) }
  '-' { PT _ (TS _ 11) }
  '--' { PT _ (TS _ 12) }
  '/' { PT _ (TS _ 13) }
  ';' { PT _ (TS _ 14) }
  '<' { PT _ (TS _ 15) }
  '<=' { PT _ (TS _ 16) }
  '=' { PT _ (TS _ 17) }
  '==' { PT _ (TS _ 18) }
  '>' { PT _ (TS _ 19) }
  '>=' { PT _ (TS _ 20) }
  'boolean' { PT _ (TS _ 21) }
  'double' { PT _ (TS _ 22) }
  'else' { PT _ (TS _ 23) }
  'false' { PT _ (TS _ 24) }
  'if' { PT _ (TS _ 25) }
  'int' { PT _ (TS _ 26) }
  'return' { PT _ (TS _ 27) }
  'true' { PT _ (TS _ 28) }
  'voi' { PT _ (TS _ 29) }
  'void' { PT _ (TS _ 30) }
  'while' { PT _ (TS _ 31) }
  '{' { PT _ (TS _ 32) }
  '||' { PT _ (TS _ 33) }
  '}' { PT _ (TS _ 34) }
  L_Ident  { PT _ (TV $$) }
  L_integ  { PT _ (TI $$) }
  L_doubl  { PT _ (TD $$) }
  L_quoted { PT _ (TL $$) }

%%

Ident :: { Javalette.Abs.Ident}
Ident  : L_Ident { Javalette.Abs.Ident $1 }

Integer :: { Integer }
Integer  : L_integ  { (read ($1)) :: Integer }

Double  :: { Double }
Double   : L_doubl  { (read ($1)) :: Double }

String  :: { String }
String   : L_quoted { $1 }

Prog :: { Javalette.Abs.Prog }
Prog : ListTopDef { Javalette.Abs.Program $1 }

TopDef :: { Javalette.Abs.TopDef }
TopDef : Type Ident '(' ListArg ')' Blk { Javalette.Abs.FnDef $1 $2 $4 $6 }

ListTopDef :: { [Javalette.Abs.TopDef] }
ListTopDef : TopDef { (:[]) $1 } | TopDef ListTopDef { (:) $1 $2 }

Arg :: { Javalette.Abs.Arg }
Arg : Type Ident { Javalette.Abs.Argument $1 $2 }

ListArg :: { [Javalette.Abs.Arg] }
ListArg : {- empty -} { [] }
        | Arg { (:[]) $1 }
        | Arg ',' ListArg { (:) $1 $3 }

Blk :: { Javalette.Abs.Blk }
Blk : '{' ListStmt '}' { Javalette.Abs.Block $2 }

ListStmt :: { [Javalette.Abs.Stmt] }
ListStmt : {- empty -} { [] } | Stmt ListStmt { (:) $1 $2 }

Stmt :: { Javalette.Abs.Stmt }
Stmt : ';' { Javalette.Abs.Empty }
     | Blk { Javalette.Abs.BStmt $1 }
     | Type ListItem ';' { Javalette.Abs.Decl $1 $2 }
     | Ident '=' Expr ';' { Javalette.Abs.Ass $1 $3 }
     | Ident '++' ';' { Javalette.Abs.Incr $1 }
     | Ident '--' ';' { Javalette.Abs.Decr $1 }
     | 'return' Expr ';' { Javalette.Abs.Ret $2 }
     | 'return' ';' { Javalette.Abs.VRet }
     | 'if' '(' Expr ')' Stmt { Javalette.Abs.Cond $3 $5 }
     | 'if' '(' Expr ')' Stmt 'else' Stmt { Javalette.Abs.CondElse $3 $5 $7 }
     | 'while' '(' Expr ')' Stmt { Javalette.Abs.While $3 $5 }
     | Expr ';' { Javalette.Abs.SExp $1 }

Item :: { Javalette.Abs.Item }
Item : Ident { Javalette.Abs.NoInit $1 }
     | Ident '=' Expr { Javalette.Abs.Init $1 $3 }

ListItem :: { [Javalette.Abs.Item] }
ListItem : Item { (:[]) $1 } | Item ',' ListItem { (:) $1 $3 }

Type :: { Javalette.Abs.Type }
Type : 'int' { Javalette.Abs.Int }
     | 'double' { Javalette.Abs.Doub }
     | 'boolean' { Javalette.Abs.Bool }
     | 'void' { Javalette.Abs.Void }
     | 'voi' { Javalette.Abs.Voi }

ListType :: { [Javalette.Abs.Type] }
ListType : {- empty -} { [] }
         | Type { (:[]) $1 }
         | Type ',' ListType { (:) $1 $3 }

Expr6 :: { Javalette.Abs.Expr }
Expr6 : Ident { Javalette.Abs.EVar $1 }
      | Integer { Javalette.Abs.ELitInt $1 }
      | Double { Javalette.Abs.ELitDoub $1 }
      | 'true' { Javalette.Abs.ELitTrue }
      | 'false' { Javalette.Abs.ELitFalse }
      | Ident '(' ListExpr ')' { Javalette.Abs.EApp $1 $3 }
      | String { Javalette.Abs.EString $1 }
      | '(' Expr ')' { $2 }

Expr5 :: { Javalette.Abs.Expr }
Expr5 : '-' Expr6 { Javalette.Abs.Neg $2 }
      | '!' Expr6 { Javalette.Abs.Not $2 }
      | Expr6 { $1 }

Expr4 :: { Javalette.Abs.Expr }
Expr4 : Expr4 MulOp Expr5 { Javalette.Abs.EMul $1 $2 $3 }
      | Expr5 { $1 }

Expr3 :: { Javalette.Abs.Expr }
Expr3 : Expr3 AddOp Expr4 { Javalette.Abs.EAdd $1 $2 $3 }
      | Expr4 { $1 }

Expr2 :: { Javalette.Abs.Expr }
Expr2 : Expr2 RelOp Expr3 { Javalette.Abs.ERel $1 $2 $3 }
      | Expr3 { $1 }

Expr1 :: { Javalette.Abs.Expr }
Expr1 : Expr2 '&&' Expr1 { Javalette.Abs.EAnd $1 $3 }
      | Expr2 { $1 }

Expr :: { Javalette.Abs.Expr }
Expr : Expr1 '||' Expr { Javalette.Abs.EOr $1 $3 } | Expr1 { $1 }

ListExpr :: { [Javalette.Abs.Expr] }
ListExpr : {- empty -} { [] }
         | Expr { (:[]) $1 }
         | Expr ',' ListExpr { (:) $1 $3 }

AddOp :: { Javalette.Abs.AddOp }
AddOp : '+' { Javalette.Abs.Plus } | '-' { Javalette.Abs.Minus }

MulOp :: { Javalette.Abs.MulOp }
MulOp : '*' { Javalette.Abs.Times }
      | '/' { Javalette.Abs.Div }
      | '%' { Javalette.Abs.Mod }

RelOp :: { Javalette.Abs.RelOp }
RelOp : '<' { Javalette.Abs.LTH }
      | '<=' { Javalette.Abs.LE }
      | '>' { Javalette.Abs.GTH }
      | '>=' { Javalette.Abs.GE }
      | '==' { Javalette.Abs.EQU }
      | '!=' { Javalette.Abs.NE }
{

happyError :: [Token] -> Either String a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer = tokens
}

