{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- | Pretty-printer for Javalette.
--   Generated by the BNF converter.

module Javalette.Print where

import qualified Javalette.Abs
import Data.Char

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    [";"]        -> showChar ';'
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : ts@(p:_) | closingOrPunctuation p -> showString t . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i     = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t s =
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print Javalette.Abs.Ident where
  prt _ (Javalette.Abs.Ident i) = doc $ showString $ i

instance Print Javalette.Abs.Prog where
  prt i e = case e of
    Javalette.Abs.Program topdefs -> prPrec i 0 (concatD [prt 0 topdefs])

instance Print Javalette.Abs.TopDef where
  prt i e = case e of
    Javalette.Abs.FnDef type_ id args blk -> prPrec i 0 (concatD [prt 0 type_, prt 0 id, doc (showString "("), prt 0 args, doc (showString ")"), prt 0 blk])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print [Javalette.Abs.TopDef] where
  prt = prtList

instance Print Javalette.Abs.Arg where
  prt i e = case e of
    Javalette.Abs.Argument type_ id -> prPrec i 0 (concatD [prt 0 type_, prt 0 id])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [Javalette.Abs.Arg] where
  prt = prtList

instance Print Javalette.Abs.Blk where
  prt i e = case e of
    Javalette.Abs.Block stmts -> prPrec i 0 (concatD [doc (showString "{"), prt 0 stmts, doc (showString "}")])

instance Print [Javalette.Abs.Stmt] where
  prt = prtList

instance Print Javalette.Abs.Stmt where
  prt i e = case e of
    Javalette.Abs.Empty -> prPrec i 0 (concatD [doc (showString ";")])
    Javalette.Abs.BStmt blk -> prPrec i 0 (concatD [prt 0 blk])
    Javalette.Abs.Decl type_ items -> prPrec i 0 (concatD [prt 0 type_, prt 0 items, doc (showString ";")])
    Javalette.Abs.Ass id expr -> prPrec i 0 (concatD [prt 0 id, doc (showString "="), prt 0 expr, doc (showString ";")])
    Javalette.Abs.Incr id -> prPrec i 0 (concatD [prt 0 id, doc (showString "++"), doc (showString ";")])
    Javalette.Abs.Decr id -> prPrec i 0 (concatD [prt 0 id, doc (showString "--"), doc (showString ";")])
    Javalette.Abs.Ret expr -> prPrec i 0 (concatD [doc (showString "return"), prt 0 expr, doc (showString ";")])
    Javalette.Abs.VRet -> prPrec i 0 (concatD [doc (showString "return"), doc (showString ";")])
    Javalette.Abs.Cond expr stmt -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 stmt])
    Javalette.Abs.CondElse expr stmt1 stmt2 -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 stmt1, doc (showString "else"), prt 0 stmt2])
    Javalette.Abs.While expr stmt -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 stmt])
    Javalette.Abs.SExp expr -> prPrec i 0 (concatD [prt 0 expr, doc (showString ";")])
    Javalette.Abs.Retting stmt n -> prPrec i 0 (concatD [doc (showString "["), prt 0 stmt, doc (showString ":"), prt 0 n, doc (showString "]")])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print Javalette.Abs.Item where
  prt i e = case e of
    Javalette.Abs.NoInit id -> prPrec i 0 (concatD [prt 0 id])
    Javalette.Abs.Init id expr -> prPrec i 0 (concatD [prt 0 id, doc (showString "="), prt 0 expr])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [Javalette.Abs.Item] where
  prt = prtList

instance Print Javalette.Abs.Type where
  prt i e = case e of
    Javalette.Abs.Int -> prPrec i 0 (concatD [doc (showString "int")])
    Javalette.Abs.Doub -> prPrec i 0 (concatD [doc (showString "double")])
    Javalette.Abs.Bool -> prPrec i 0 (concatD [doc (showString "boolean")])
    Javalette.Abs.Void -> prPrec i 0 (concatD [doc (showString "void")])
    Javalette.Abs.String -> prPrec i 0 (concatD [doc (showString "string")])
    Javalette.Abs.Fun type_ types -> prPrec i 0 (concatD [prt 0 type_, doc (showString "("), prt 0 types, doc (showString ")")])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [Javalette.Abs.Type] where
  prt = prtList

instance Print Javalette.Abs.Expr where
  prt i e = case e of
    Javalette.Abs.EVar id -> prPrec i 6 (concatD [prt 0 id])
    Javalette.Abs.ELitInt n -> prPrec i 6 (concatD [prt 0 n])
    Javalette.Abs.ELitDoub d -> prPrec i 6 (concatD [prt 0 d])
    Javalette.Abs.ELitTrue -> prPrec i 6 (concatD [doc (showString "true")])
    Javalette.Abs.ELitFalse -> prPrec i 6 (concatD [doc (showString "false")])
    Javalette.Abs.EApp id exprs -> prPrec i 6 (concatD [prt 0 id, doc (showString "("), prt 0 exprs, doc (showString ")")])
    Javalette.Abs.EString str -> prPrec i 6 (concatD [prt 0 str])
    Javalette.Abs.Neg expr -> prPrec i 5 (concatD [doc (showString "-"), prt 6 expr])
    Javalette.Abs.Not expr -> prPrec i 5 (concatD [doc (showString "!"), prt 6 expr])
    Javalette.Abs.EMul expr1 mulop expr2 -> prPrec i 4 (concatD [prt 4 expr1, prt 0 mulop, prt 5 expr2])
    Javalette.Abs.EAdd expr1 addop expr2 -> prPrec i 3 (concatD [prt 3 expr1, prt 0 addop, prt 4 expr2])
    Javalette.Abs.ERel expr1 relop expr2 -> prPrec i 2 (concatD [prt 2 expr1, prt 0 relop, prt 3 expr2])
    Javalette.Abs.EAnd expr1 expr2 -> prPrec i 1 (concatD [prt 2 expr1, doc (showString "&&"), prt 1 expr2])
    Javalette.Abs.EOr expr1 expr2 -> prPrec i 0 (concatD [prt 1 expr1, doc (showString "||"), prt 0 expr2])
    Javalette.Abs.ETyped expr type_ -> prPrec i 0 (concatD [doc (showString "["), prt 0 expr, doc (showString ":"), prt 0 type_, doc (showString "]")])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [Javalette.Abs.Expr] where
  prt = prtList

instance Print Javalette.Abs.AddOp where
  prt i e = case e of
    Javalette.Abs.Plus -> prPrec i 0 (concatD [doc (showString "+")])
    Javalette.Abs.Minus -> prPrec i 0 (concatD [doc (showString "-")])

instance Print Javalette.Abs.MulOp where
  prt i e = case e of
    Javalette.Abs.Times -> prPrec i 0 (concatD [doc (showString "*")])
    Javalette.Abs.Div -> prPrec i 0 (concatD [doc (showString "/")])
    Javalette.Abs.Mod -> prPrec i 0 (concatD [doc (showString "%")])

instance Print Javalette.Abs.RelOp where
  prt i e = case e of
    Javalette.Abs.LTH -> prPrec i 0 (concatD [doc (showString "<")])
    Javalette.Abs.LE -> prPrec i 0 (concatD [doc (showString "<=")])
    Javalette.Abs.GTH -> prPrec i 0 (concatD [doc (showString ">")])
    Javalette.Abs.GE -> prPrec i 0 (concatD [doc (showString ">=")])
    Javalette.Abs.EQU -> prPrec i 0 (concatD [doc (showString "==")])
    Javalette.Abs.NE -> prPrec i 0 (concatD [doc (showString "!=")])

