-- Optional: turn on warnings.
{-# OPTIONS_GHC -Wall #-}
{-#LANGUAGE TupleSections #-}
{-#LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Compiler for C--, producing symbolic JVM assembler.

module Compiler where

import           CMM.Abs
import           CMM.Print

import           Control.Monad
import           Control.Monad.State

import           Data.Maybe
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map


-- | Entry point.

compile
  :: String  -- ^ Class name.
  -> Program -- ^ Type-annotated program.
  -> String  -- ^ Generated jasmin source file content.
--compile name _prg = header
compile name (PDefs defs) =
  unlines $ concat $ header : map (compileDef sig0) defs
 where
  sig0 = Map.fromList $ builtin ++ map sigEntry defs
  sigEntry def@(DFun _ f@(Id x) _ _) =
    (f, ) $ Fun (Id $ name ++ "/" ++ x) $ funType def
  header :: [String]
  header = concat -- unlines $ concat --
    [ [ ";; BEGIN HEADER"
      , ""
      , ".class public " ++ name
      , ".super java/lang/Object"
      , ""
      , ".method public <init>()V"
      , ".limit locals 1"
      , ""
      ]
    , map indent
          ["aload_0", "invokespecial java/lang/Object/<init>()V", "return"]
    , [ ""
      , ".end method"
      , ""
      , ".method public static main([Ljava/lang/String;)V"
      , ".limit locals 1"
      , ".limit stack 1"
      , ""
      ]
    , map indent ["invokestatic " ++ name ++ "/main()I", "pop", "return"]
    , ["", ".end method", "", ";; END HEADER"]
    ]

-- | Indent non-empty lines.
indent :: String -> String
indent s = if null s then s else "\t" ++ s

type Sig = Map Id Fun

type Cxt = [Block]
type Block = [(Id, Type)]

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

funType :: Def -> FunType
funType (DFun rt _ args _) = FunType rt $ map (\(ADecl t _) -> t) args

data FunType = FunType Type [Type]
  deriving Show

data Fun = Fun
  { funId      :: Id
  , funFunType :: FunType
  }
  deriving Show

newtype Label = L {theLabel :: Int}
  deriving (Eq, Enum, Show)

type Output = [Code]

type Compile = State St


instance Size Type where
  size Type_int    = 1
  size Type_double = 2
  size Type_bool   = 1
  size Type_void   = 0

instance Size Id where
  size _ = 0

instance (Size a, Size b) => Size (a,b) where
  size (x, y) = size x + size y

instance Size a => Size [a] where
  size = sum . map size

instance Size FunType where
  size (FunType t ts) = size ts - size t

instance Size Fun where
  size (Fun _ ft) = size ft

class Size a where
    size :: a -> Int

class ToJVM a where
    toJVM :: a -> String

instance ToJVM Type where
  toJVM t = case t of
    Type_int    -> "I"
    Type_void   -> "V"
    Type_bool   -> "Z"
    Type_double -> "D"

instance ToJVM MulOp where
  toJVM op = case op of
    OTimes -> "mul"
    ODiv   -> "div"

instance ToJVM AddOp where
  toJVM op = case op of
    OPlus  -> "add"
    OMinus -> "sub"


instance ToJVM CmpOp where
  toJVM op = case op of
    OLt   -> "lt"
    OGt   -> "gt"
    OLtEq -> "le"
    OGtEq -> "ge"
    OEq   -> "eq"
    ONEq  -> "ne"

builtin :: [(Id, Fun)]
builtin =
  [ (Id "printInt", Fun (Id "Runtime/printInt") $ FunType Type_void [Type_int])
  , ( Id "printDouble"
    , Fun (Id "Runtime/printDouble") $ FunType Type_void [Type_double]
    )
  , (Id "readInt"   , Fun (Id "Runtime/readInt") $ FunType Type_int [])
  , (Id "readDouble", Fun (Id "Runtime/doubleInt") $ FunType Type_double [])
  ]


data Code
  = Store Type Addr
  | Load Type Addr
  | IConst Integer
  | DConst Double
  | Dup Type
  | Pop Type
  | Return Type
  | Call Fun

  | Label Label
  | Goto Label
  | If CmpOp Label
  | IfCmp Type CmpOp Label
  | DCmp
  | Inc Type Addr Int
  | Add Type AddOp
  | Mul Type MulOp
  | I2D
  | Comment String

    deriving (Show)


pattern IfZ :: Label -> Code
pattern IfZ l = If OEq l
pattern IfNZ :: Label -> Code
pattern IfNZ l = If ONEq l

negateCmp :: CmpOp -> CmpOp
negateCmp = \case
  OEq   -> ONEq
  ONEq  -> OEq
  OLt   -> OGtEq
  OGt   -> OLtEq
  OLtEq -> OGt
  OGtEq -> OLt

flipCmp :: CmpOp -> CmpOp
flipCmp = \case
  OEq   -> OEq
  ONEq  -> ONEq
  OLt   -> OGt
  OGt   -> OLt
  OLtEq -> OGtEq
  OGtEq -> OLtEq

instance ToJVM FunType where
  toJVM (FunType t ts) = "(" ++ (toJVM =<< ts) ++ ")" ++ toJVM t

instance ToJVM Fun where
  toJVM (Fun (Id f) t) = f ++ toJVM t

instance ToJVM Label where
  toJVM (L l) = "L" ++ show l

prefix :: Type -> String
prefix Type_int    = "i"
prefix Type_bool   = "i"
prefix Type_double = "d"
prefix Type_void   = ""

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

    Dup   Type_double         -> "dup2"
    Dup   _                   -> "dup"
    Pop   Type_double         -> "pop2"
    Pop   _                   -> "pop"

    Label l                   -> toJVM l ++ ":"
    Goto  l                   -> "goto " ++ toJVM l
    If op                   l -> "if" ++ toJVM op ++ " " ++ toJVM l

    c@(IfCmp Type_double _ _) -> impossible c
    c@(IfCmp Type_void   _ _) -> impossible c

    IfCmp _ op l              -> "if_icmp" ++ toJVM op ++ " " ++ toJVM l
    DCmp                      -> "dcmpg"

    Inc Type_int a k          -> "iinc " ++ show a ++ " " ++ show k
    c@Inc{}                   -> impossible c

    Add t op                  -> prefix t ++ toJVM op
    Mul t op                  -> prefix t ++ toJVM op

    I2D                       -> "i2d"

    Comment ""                -> ""
    Comment s                 -> ";; " ++ s

compileDef :: Sig -> Def -> [String]
compileDef sig0 def@(DFun t f args ss) = concat
  [ ["", ".method public static " ++ toJVM (Fun f $ funType def)]
  , [ ".limit locals " ++ show (limitLocals st)
    , ".limit stack " ++ show (limitStack st)
    ]
  , map (indent . toJVM) $ reverse (output st)
  , ["", ".end method"]
  ]
  where st = execState (compileFun t args ss) $ initSt sig0

compileFun :: Type -> [Arg] -> [Stm] -> Compile ()
compileFun _ args ss = do
  mapM_ (\(ADecl t' x) -> newVar x t') args
  mapM_ compileStm                     ss
  emit $ Return Type_void


stmTop :: Stm -> String
stmTop = \case
  (SReturn e    ) -> "return " ++ printTree e  -- ?
  (SWhile e _   ) -> "while (" ++ printTree e ++ ")" -- ++ printTree s
  (SIfElse e _ _) -> "if (" ++ printTree e ++ ")"
  (SBlock _     ) -> ""
  s               -> printTree s

comment :: String -> Compile ()
comment = emit . Comment

blank :: Compile ()
blank = comment ""

compileStm :: Stm -> Compile ()
compileStm s0 = do
  let top = stmTop s0
  unless (null top) $ do
    blank
    mapM_ comment $ lines top
  case s0 of
    SReturn et@(ETyped _ t) -> do
      compileExp et
      emit $ Return t

    SInit t x (ETyped e _) -> do
         --error $ printTree m
      newVar x t
      (a, _) <- lookupVar x
      compileExp (ETyped e t)
      emit $ Store t a
    SExp e@(ETyped _ t) -> do
      compileExp e
      emit $ Pop t
    SDecls t ids -> mapM_ (declVar t) ids
     where
      declVar :: Type -> Id -> Compile ()
      declVar t' i = do
        newVar i t'
    SWhile e s -> do
      l  <- newLabel
      l2 <- newLabel
      emit $ Label l
      compileCond False l2 e
      inNewBlock $ compileStm s

      emit $ Goto l
      emit $ Label l2
    SBlock ss -> do
      inNewBlock $ compileStms ss
     where
      compileStms :: [Stm] -> Compile ()
      compileStms []        = return ()
      compileStms (s : ss') = do
        compileStm s
        compileStms ss'
    SIfElse e s1 s2 -> do
      l1 <- newLabel
      l2 <- newLabel
      compileCond False l1 e
      inNewBlock $ compileStm s1
      emit $ Goto l2
      emit $ Label l1
      inNewBlock $ compileStm s2
      emit $ Label l2
    s -> error $ "not implemented compileStm " ++ printTree s

boolLitToBool :: BoolLit -> Bool
boolLitToBool LTrue = True
boolLitToBool _     = False

compileCond :: Bool -> Label -> Exp -> Compile ()
compileCond cond l = \case
  EBool b -> when (boolLitToBool b == cond) $ emit $ Goto l
  ETyped (ECmp e1@(ETyped _ Type_double) op e2@(ETyped _ Type_double)) _-> do
    compileExp e1
    compileExp e2
    emit $ IfCmp Type_double (if cond then op else negateCmp op) l
  ETyped (ECmp e1@(ETyped _ _) op e2@(ETyped _ _)) t -> do
    compileExp e1
    compileExp e2
    emit $ IfCmp t (if cond then op else negateCmp op) l
  e -> do
    compileExp e
    emit $ (if cond then IfNZ else IfZ) l

compileExp :: Exp -> Compile ()
compileExp  = \case
  --ETyped e Type_double -> error $ printTree e --compileExp e
  EInt i                      -> emit $ IConst i

  ETyped (EInt i) Type_double -> do
    emit $ IConst i
    emit I2D

  ETyped (ETyped e Type_int) Type_double -> do
    compileExp (ETyped e Type_int)
    emit I2D

  EId x -> do
    (a, t) <- lookupVar x
    emit $ Load t a

  EApp x@(Id _) es -> do
    mapM_ compileExp es
    f <- gets ((fromMaybe (error "undefined") . Map.lookup x) . sig)
    emit $ Call f

  EBool b -> emit $ IConst $ b2I b
   where
    b2I :: BoolLit -> Integer
    b2I LTrue  = 1
    b2I LFalse = 0

  EDouble d  -> emit $ DConst d

  EPost i op -> do
    (a, t) <- lookupVar i
    emit $ Load t a
    case op of
      OInc -> emit $ Inc t a 1
      ODec -> emit $ Inc t a (-1)

  EPre op i -> do
    (a, t) <- lookupVar i
    case op of
      OInc -> emit $ Inc t a 1
      ODec -> emit $ Inc t a (-1)
    emit $ Load t a

  ETyped (EMul e1 op e2) t -> do
    compileExp e1
    compileExp e2
    emit $ Mul t op

  ETyped (EAdd e1 op e2) t -> do
    compileExp e1
    compileExp e2
    emit $ Add t op

  ETyped (ECmp e1@(ETyped _ t1) op e2) _ -> do
    case t1 of
      Type_double -> do
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

  EAss x e -> do
    compileExp e
    (a, t) <- lookupVar x
    emit $ Store t a
    emit $ Load t a


  ETyped e _ -> compileExp e

  e          -> error $ "not implemented compileexp " ++ show e

dCond :: CmpOp -> Compile ()
dCond op = case op of
  OGt -> do
    emit I2D
    emit $ IConst 1
    emit I2D
    emit DCmp
    emit $ IConst 1
    emit $ Add Type_int OPlus
    return ()
  OEq -> do
    emit $ Dup Type_int
    emit $ Mul Type_int OTimes
    emit $ IConst (-1)
    emit $ Add Type_int OPlus
    emit $ IConst (-1)
    emit $ Mul Type_int OTimes
  ONEq -> do
    emit $ Dup Type_int
    emit $ Mul Type_int OTimes
  OLt -> do
    emit I2D
    emit $ IConst (-1)
    emit I2D
    emit DCmp
    emit I2D
    emit $ IConst 1
    emit I2D
    emit DCmp
    emit $ IConst (-1)
    emit $ Mul Type_int OTimes

  OLtEq -> do
    emit I2D
    emit $ IConst 1
    emit I2D
    emit DCmp
    emit $ IConst (-1)
    emit $ Mul Type_int OTimes
  OGtEq -> do
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


newVar :: Id -> Type -> Compile ()
newVar x t = do
  modify $ \st@St { cxt = (b : bs) } -> st { cxt = ((x, t) : b) : bs }
  updateLimitLocals


lookupVar :: Id -> Compile (Addr, Type)
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
emit (Store Type_void _    ) = return ()
emit (Load  Type_void _    ) = return ()
emit (Dup Type_void        ) = return ()
emit (Pop Type_void        ) = return ()

emit (Inc t@Type_double a k) = do
  emit $ Load t a
  emit $ DConst $ fromIntegral k
  emit $ Add t OPlus
  emit $ Store t a

emit (IfCmp Type_double o l) = do
  emit DCmp
  emit $ If o l

emit c = do
  modify $ \st@St { output = cs } -> st { output = c : cs }
  adjustStack c


adjustStack :: Code -> Compile ()
adjustStack = \case
  Store t _   -> decStack t
  Load  t _   -> incStack t
  IConst _    -> incStack Type_int
  DConst _    -> incStack Type_double
  Dup    t    -> incStack t
  Pop    t    -> decStack t
  Return t    -> decStack t
  Call   f    -> decStack f
  Label{}     -> return ()
  Goto{}      -> return ()
  If _ _      -> decStack Type_int
  IfCmp t _ _ -> decStack t >> decStack t
  DCmp -> decStack Type_double >> decStack Type_double >> incStack Type_int
  Inc{}       -> return ()
  Add t _     -> decStack t
  Mul t _     -> decStack t
  I2D         -> incStack Type_int
  Comment _   -> return ()
