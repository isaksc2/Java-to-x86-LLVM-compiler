









-- Optional: turn on warnings.
{-# OPTIONS_GHC -Wall #-}
{-#LANGUAGE TupleSections #-}
{-#LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Compiler for C--, producing symbolic JVM assembler.

module LlvmBackend where

import           Javalette.Abs
import           Javalette.Print

import           Control.Monad
import           Control.Monad.State

import           Data.Maybe
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map


-- | Entry point.

compile
  :: String  -- ^ Class name.
  -> Prog -- ^ Type-annotated program.
  -> String  -- ^ Generated jasmin source file content.
--compile name _prg = header
compile name (Program defs) =
  unlines $ concat (map (compileDef sig0) defs)
 where
  sig0 = Map.fromList $ builtin ++ map sigEntry defs
  sigEntry def@(FnDef _ f@(Ident x) _ _) =
    (f, ) $ FunHead (Ident $ name ++ "/" ++ x) $ funType def


-- | Indent non-empty lines.
indent :: String -> String
indent s = if null s then s else "\t" ++ s

type Sig = Map Ident FunHead
type Cxt = [Block]
type Block = [(Ident, Type)]

data St = St
  { sig          :: Sig
  , cxt          :: Cxt
  , limitLocals  :: Int
  , currentStack :: Int
  , limitStack   :: Int
  , nextLabel    :: Label
  , output       :: Output
  }

initSt :: Sig -> St
initSt s = St { sig          = s
              , cxt          = [[]]
              , limitLocals  = 0
              , currentStack = 0
              , limitStack   = 0
              , nextLabel    = L 0
              , output       = []
              }

type Addr = Int

-- convert function to a slightly different type.
funType :: TopDef -> FunType
funType (FnDef rt _ args _) = FunType rt $ map (\(Argument t _) -> t) args

data FunType = FunType Type [Type]
  deriving Show

data FunHead = FunHead
  { funId      :: Ident
  , funFunType :: FunType
  }
  deriving Show

newtype Label = L {theLabel :: Int}
  deriving (Eq, Enum, Show)

type Output = [Code]

type Compile = State St


instance Size Type where
  size Int    = 1
  size Doub   = 2
  size Bool   = 1
  size Void   = 0

instance Size Ident where
  size _ = 0

instance (Size a, Size b) => Size (a,b) where
  size (x, y) = size x + size y

instance Size a => Size [a] where
  size = sum . map size

instance Size FunType where
  size (FunType t ts) = size ts - size t

instance Size FunHead where
  size (FunHead _ ft) = size ft

class Size a where
    size :: a -> Int

class ToJVM a where
    toJVM :: a -> String

instance ToJVM Type where
  toJVM t = case t of
    Int    -> "I"
    Void   -> "V"
    Bool   -> "Z"
    Doub -> "D"

instance ToJVM MulOp where
  toJVM op = case op of
    Times -> "mul"
    Div   -> "div"

instance ToJVM AddOp where
  toJVM op = case op of
    Plus  -> "add"
    Minus -> "sub"


instance ToJVM RelOp where
  toJVM op = case op of
    LTH   -> "lt"
    GTH   -> "gt"
    LE -> "le"
    GE -> "ge"
    EQU   -> "eq"
    NE  -> "ne"

builtin :: [(Ident, FunHead)]
builtin =
  [ (Ident "printInt",     FunHead (Ident "Runtime/printInt") $ FunType Void [Int])
  , (Ident "printDouble",  FunHead (Ident "Runtime/printDouble") $ FunType Void [Doub])
  , (Ident "printString",  FunHead (Ident "Runtime/printString") $ FunType Void [String])
  , (Ident "readInt"   ,   FunHead (Ident "Runtime/readInt") $ FunType Int [])
  , (Ident "readDouble",   FunHead (Ident "Runtime/doubleInt") $ FunType Doub [])
  ]


data Code
  = Store Type Addr
  | Load Type Addr
  | IConst Integer
  | DConst Double
  | Dup Type
  | Pop Type
  | Return Type
  | Call FunHead

  | Label Label
  | Goto Label
  | If RelOp Label
  | IfCmp Type RelOp Label
  | DCmp
  | Inc Type Addr Int
  | Add Type AddOp
  | Mul Type MulOp
  | I2D
  | Comment String

    deriving (Show)


pattern IfZ :: Label -> Code
pattern IfZ l = If EQU l
pattern IfNZ :: Label -> Code
pattern IfNZ l = If NE l

negateCmp :: RelOp -> RelOp
negateCmp = \case
  EQU   -> NE
  NE  -> EQU
  LTH   -> GE
  GTH   -> LE
  LE -> GTH
  GE -> LTH

flipCmp :: RelOp -> RelOp
flipCmp = \case
  EQU   -> EQU
  NE  -> NE
  LTH   -> GTH
  GTH   -> LTH
  LE -> GE
  GE -> LE

instance ToJVM FunType where
  toJVM (FunType t ts) = "(" ++ (toJVM =<< ts) ++ ")" ++ toJVM t

instance ToJVM FunHead where
  toJVM (FunHead (Ident f) t) = f ++ toJVM t

instance ToJVM Label where
  toJVM (L l) = "L" ++ show l

prefix :: Type -> String
prefix Int    = "i"
prefix Bool   = "i"
prefix Doub = "d"
prefix Void   = ""

sep :: Int -> Int -> String
sep i n | i >= n    = "_"
        | otherwise = " "

isByte :: Integer -> Bool
isByte n | n < 256 || (-128) <= n && n < 128 = True
         | otherwise                         = False

impossible :: Code -> String
impossible a = error $ "impossible code " ++ toJVM a

instance ToJVM Code where
  toJVM = \case
    Store t n -> prefix t ++ "store" ++ sep 3 n ++ show n
    Load  t n -> prefix t ++ "load" ++ sep 3 n ++ show n
    Return t  -> prefix t ++ "return"
    Call   f  -> "invokestatic " ++ toJVM f
    DConst d  -> "ldc2_w " ++ show d

    IConst i | i == -1          -> "iconst_m1"
             | i >= 0 && i <= 5 -> "iconst_" ++ show i
             | isByte i         -> "bipush " ++ show i
             | otherwise        -> "ldc " ++ show i

    Dup   Doub         -> "dup2"
    Dup   _                   -> "dup"
    Pop   Doub         -> "pop2"
    Pop   _                   -> "pop"

    Label l                   -> toJVM l ++ ":"
    Goto  l                   -> "goto " ++ toJVM l
    If op                   l -> "if" ++ toJVM op ++ " " ++ toJVM l

    c@(IfCmp Doub _ _) -> impossible c
    c@(IfCmp Void   _ _) -> impossible c

    IfCmp _ op l              -> "if_icmp" ++ toJVM op ++ " " ++ toJVM l
    DCmp                      -> "dcmpg"

    Inc Int a k          -> "iinc " ++ show a ++ " " ++ show k
    c@Inc{}                   -> impossible c

    Add t op                  -> prefix t ++ toJVM op
    Mul t op                  -> prefix t ++ toJVM op

    I2D                       -> "i2d"

    Comment ""                -> ""
    Comment s                 -> ";; " ++ s

compileDef :: Sig -> TopDef -> [String]
compileDef sig0 def@(FnDef t f args ss) = concat
  [ ["", ".method public static " ++ toJVM (FunHead f $ funType def)]
  , [ ".limit locals " ++ show (limitLocals st)
    , ".limit stack " ++ show (limitStack st)
    ]
  , map (indent . toJVM) $ reverse (output st)
  , ["", ".end method"]
  ]
  where st = execState (compileFun t args ss) $ initSt sig0

compileFun :: Type -> [Arg] -> [Stmt] -> Compile ()
compileFun _ args ss = do
  mapM_ (\(Argument t' x) -> newVar x t') args
  mapM_ compileStm                     ss
  emit $ Return Void


stmTop :: Stmt -> String
stmTop = \case
  (Ret e    ) -> "return " ++ printTree e  -- ?
  (While e _   ) -> "while (" ++ printTree e ++ ")" -- ++ printTree s
  (CondElse e _ _) -> "if (" ++ printTree e ++ ")"
  (BStmt _     ) -> ""
  s               -> printTree s

comment :: String -> Compile ()
comment = emit . Comment

blank :: Compile ()
blank = comment ""

compileStm :: Stmt -> Compile ()
compileStm s0 = do
  let top = stmTop s0
  unless (null top) $ do
    blank
    mapM_ comment $ lines top
  case s0 of
    Ret et@(ETyped _ t) -> do
      compileExp et
      emit $ Return t

    Init t x (ETyped e _) -> do
         --error $ printTree m
      newVar x t
      (a, _) <- lookupVar x
      compileExp (ETyped e t)
      emit $ Store t a
    SExp e@(ETyped _ t) -> do
      compileExp e
      emit $ Pop t
    Decl t ids -> mapM_ (declVar t) ids
     where
      declVar :: Type -> Ident -> Compile ()
      declVar t' i = do
        newVar i t'
    While e s -> do
      l  <- newLabel
      l2 <- newLabel
      emit $ Label l
      compileCond False l2 e
      inNewBlock $ compileStm s

      emit $ Goto l
      emit $ Label l2
    BStmt ss -> do
      inNewBlock $ compileStms ss
     where
      compileStms :: [Stmt] -> Compile ()
      compileStms []        = return ()
      compileStms (s : ss') = do
        compileStm s
        compileStms ss'
    CondElse e s1 s2 -> do
      l1 <- newLabel
      l2 <- newLabel
      compileCond False l1 e
      inNewBlock $ compileStm s1
      emit $ Goto l2
      emit $ Label l1
      inNewBlock $ compileStm s2
      emit $ Label l2
    s -> error $ "not implemented compileStm " ++ printTree s

boolLitToBool :: Expr -> Bool
boolLitToBool ELitTrue = True
boolLitToBool ELitFalse = False

compileCond :: Bool -> Label -> Expr -> Compile ()
compileCond cond l = \case
  b -> when (boolLitToBool b == cond) $ emit $ Goto l
  ETyped (ERel e1@(ETyped _ Doub) op e2@(ETyped _ Doub)) _-> do
    compileExp e1
    compileExp e2
    emit $ IfCmp Doub (if cond then op else negateCmp op) l
  ETyped (ERel e1@(ETyped _ _) op e2@(ETyped _ _)) t -> do
    compileExp e1
    compileExp e2
    emit $ IfCmp t (if cond then op else negateCmp op) l
  e -> do
    compileExp e
    emit $ (if cond then IfNZ else IfZ) l

compileExp :: Expr -> Compile ()
compileExp  = \case
  --ETyped e Doub -> error $ printTree e --compileExp e
  ELitInt i                      -> emit $ IConst i

  ETyped (ELitInt i) Doub -> do
    emit $ IConst i
    emit I2D

  ETyped (ETyped e Int) Doub -> do
    compileExp (ETyped e Int)
    emit I2D

  EVar x -> do
    (a, t) <- lookupVar x
    emit $ Load t a

  EApp x@(Ident _) es -> do
    mapM_ compileExp es
    f <- gets ((fromMaybe (error "undefined") . Map.lookup x) . sig)
    emit $ Call f

  ELitTrue -> emit $ IConst 1
  ELitFalse -> emit $ IConst 0

  ELitDoub d  -> emit $ DConst d

  --EPost i op -> do
    --(a, t) <- lookupVar i
    --emit $ Load t a
    --case op of
      --OInc -> emit $ Inc t a 1
      --ODec -> emit $ Inc t a (-1)

  --EPre op i -> do
    --(a, t) <- lookupVar i
    --case op of
      --OInc -> emit $ Inc t a 1
      --ODec -> emit $ Inc t a (-1)
    --emit $ Load t a

  ETyped (EMul e1 op e2) t -> do
    compileExp e1
    compileExp e2
    emit $ Mul t op

  ETyped (EAdd e1 op e2) t -> do
    compileExp e1
    compileExp e2
    emit $ Add t op

  ETyped (ERel e1@(ETyped _ t1) op e2) _ -> do
    case t1 of
      Doub -> do
        compileExp e1
        compileExp e2
      _ -> do
        compileExp e1
        emit I2D
        compileExp e2
        emit I2D
    emit DCmp
    dCond op

  EAnd e1 e2 -> do
    f   <- newLabel
    end <- newLabel

    compileCond False f e1

    compileCond False f e2
    emit $ IConst 1
    emit $ Goto end
    emit $ Label f
    emit $ IConst 0
    emit $ Label end

  EOr e1 e2 -> do
    t   <- newLabel
    end <- newLabel

    compileCond True t e1

    compileCond True t e2
    emit $ IConst 0
    emit $ Goto end
    emit $ Label t
    emit $ IConst 1
    emit $ Label end

  Ass x e -> do
    compileExp e
    (a, t) <- lookupVar x
    emit $ Store t a
    emit $ Load t a


  ETyped e _ -> compileExp e

  e          -> error $ "not implemented compileexp " ++ show e

dCond :: RelOp -> Compile ()
dCond op = case op of
  GTH -> do
    emit I2D
    emit $ IConst 1
    emit I2D
    emit DCmp
    emit $ IConst 1
    emit $ Add Int Plus
    return ()
  EQU -> do
    emit $ Dup Int
    emit $ Mul Int Times
    emit $ IConst (-1)
    emit $ Add Int Plus
    emit $ IConst (-1)
    emit $ Mul Int Times
  NE -> do
    emit $ Dup Int
    emit $ Mul Int Times
  LTH -> do
    emit I2D
    emit $ IConst (-1)
    emit I2D
    emit DCmp
    emit I2D
    emit $ IConst 1
    emit I2D
    emit DCmp
    emit $ IConst (-1)
    emit $ Mul Int Times

  LE -> do
    emit I2D
    emit $ IConst 1
    emit I2D
    emit DCmp
    emit $ IConst (-1)
    emit $ Mul Int Times
  GE -> do
    emit I2D
    emit $ IConst (-1)
    emit I2D
    emit DCmp

newLabel :: Compile Label
newLabel = do
  l <- gets nextLabel
  modify $ \st -> st { nextLabel = succ l }
  return l

inNewBlock :: Compile a -> Compile a
inNewBlock cont = do
  modify $ \st -> st { cxt = [] : cxt st }
  a <- cont
  modify $ \st -> st { cxt = tail $ cxt st }
  return a


newVar :: Ident -> Type -> Compile ()
newVar x t = do
  modify $ \st@St { cxt = (b : bs) } -> st { cxt = ((x, t) : b) : bs }
  updateLimitLocals


lookupVar :: Ident -> Compile (Addr, Type)
lookupVar x = gets ((loop . concat) . cxt)
 where
  loop [] = error $ "unbound variable " ++ printTree x
  loop ((y, t) : bs) | x == y    = (size bs, t)
                     | otherwise = loop bs

updateLimitLocals :: Compile ()
updateLimitLocals = do
  old <- gets limitLocals
  new <- gets (size . cxt)
  when (new > old) $ modify $ \st -> st { limitLocals = new }

incStack :: Size t => t -> Compile ()
incStack t = modStack (size t)

decStack :: Size t => t -> Compile ()
decStack t = modStack (-(size t))

modStack :: Int -> Compile ()
modStack n = do
  new <- gets ((n +) . currentStack)
  modify $ \st -> st { currentStack = new }
  old <- gets limitStack
  when (new > old) $ modify $ \st -> st { limitStack = new }


emit :: Code -> Compile ()
emit (Store Void _    ) = return ()
emit (Load  Void _    ) = return ()
emit (Dup Void        ) = return ()
emit (Pop Void        ) = return ()

emit (Inc t@Doub a k) = do
  emit $ Load t a
  emit $ DConst $ fromIntegral k
  emit $ Add t Plus
  emit $ Store t a

emit (IfCmp Doub o l) = do
  emit DCmp
  emit $ If o l

emit c = do
  modify $ \st@St { output = cs } -> st { output = c : cs }
  adjustStack c


adjustStack :: Code -> Compile ()
adjustStack = \case
  Store t _   -> decStack t
  Load  t _   -> incStack t
  IConst _    -> incStack Int
  DConst _    -> incStack Doub
  Dup    t    -> incStack t
  Pop    t    -> decStack t
  Return t    -> decStack t
  Call   f    -> decStack f
  Label{}     -> return ()
  Goto{}      -> return ()
  If _ _      -> decStack Int
  IfCmp t _ _ -> decStack t >> decStack t
  DCmp -> decStack Doub >> decStack Doub >> incStack Int
  Inc{}       -> return ()
  Add t _     -> decStack t
  Mul t _     -> decStack t
  I2D         -> incStack Int
  Comment _   -> return ()
