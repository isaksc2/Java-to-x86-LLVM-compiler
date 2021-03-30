-- Haskell data types for the abstract syntax.
-- Generated by the BNF converter.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AbsCMM where

import Prelude (Char, Double, Integer, String)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified Data.String

newtype Id = Id String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

data Program = PDefs [Def]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Def = DFun Type Id [Arg] [Stm]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Arg = ADecl Type Id
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Stm
    = SExp Exp
    | SDecls Type [Id]
    | SInit Type Id Exp
    | SReturn Exp
    | SWhile Exp Stm
    | SBlock [Stm]
    | SIfElse Exp Stm Stm
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Exp
    = EBool BoolLit
    | EInt Integer
    | EDouble Double
    | EId Id
    | EApp Id [Exp]
    | EPost Id IncDecOp
    | EPre IncDecOp Id
    | EMul Exp MulOp Exp
    | EAdd Exp AddOp Exp
    | ECmp Exp CmpOp Exp
    | EAnd Exp Exp
    | EOr Exp Exp
    | EAss Id Exp
    | ETyped Exp Type
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data IncDecOp = OInc | ODec
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data MulOp = OTimes | ODiv
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data AddOp = OPlus | OMinus
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data CmpOp = OLt | OGt | OLtEq | OGtEq | OEq | ONEq
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data BoolLit = LTrue | LFalse
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Type = Type_bool | Type_int | Type_double | Type_void
  deriving (C.Eq, C.Ord, C.Show, C.Read)

