









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
import           Data.List


-- | Entry point.

compile
  :: String  -- ^ Class name.
  -> Prog -- ^ Type-annotated program.
  -> String  -- ^ Generated jasmin source file content.
--compile name _prg = header
compile name (Program defs) = do
  --unlines $ concat (map (compileDef sig0) defs)
  let prog = unlines (map (compileDef sig0) defs) 
  reverse $ drop 2 $ reverse $ prog -- remove the last 2 extra newlines
  where
    sig0 = Map.fromList $ builtin ++ map sigEntry defs
    sigEntry def@(FnDef _ f@(Ident x) _ _) =
      (f, ) $ FunHead (Ident x) $ funType def


-- | Indent non-empty, non label lines.
indent :: String -> String
indent s | null s = s
indent s | last s == ':' = s
indent s = "\t" ++ s

type Sig = Map Ident FunHead
type Cxt = [Block]
type Block = [(Ident, LLVMType)]


data St = St
  { sig          :: Sig
  , cxt          :: Cxt
  , limitLocals  :: Int
  , currentStack :: Int
  , limitStack   :: Int
  , nextLabel    :: Label
  , nextVar      :: Register
  , output       :: Output
  , prevResult    :: [Value]
  }

initSt :: Sig -> St
initSt s = St { sig          = s
              , cxt          = [[]]
              , limitLocals  = 0
              , currentStack = 0
              , limitStack   = 0
              , nextLabel    = L 0
              , nextVar      = R 0
              , output       = []
              , prevResult   = []
              }

type Addr = Int

data LLVMType = Lit Type | Ptr LLVMType
  deriving(Show, Eq)

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

{-
instance Size Type where
  size Int    = 1
  size Doub   = 2
  size Bool   = 1
  size Void   = 0

instance Size Ident where
  size _ = 0
  -}

-- update or add argument
setPrevVal :: Value -> Bool -> Compile ()
setPrevVal v False = modify $ \st@St { prevResult = vs } -> st { prevResult = v : (tail vs) } ------------ reverse?
setPrevVal v True  = modify $ \st@St { prevResult = vs } -> st { prevResult = v : vs } ------------ reverse?

{-
instance (Size a, Size b) => Size (a,b) where
  size (x, y) = size x + size y

instance Size a => Size [a] where
  size = sum . map size

instance Size FunType where
  size (FunType t ts) = size ts - size t

instance Size FunHead where
  size (FunHead _ ft) = size ft
 c
class Size a where
    size :: a -> Int
-}

class ToJVM a where
    toJVM :: a -> String

instance ToJVM LLVMType where
  toJVM = \case
    Ptr ptr -> toJVM ptr ++ "*" 
    Lit lit -> toJVM lit

instance ToJVM Type where
  toJVM t = case t of
    Int    -> "i32"
    Void   -> "void"
    Bool   -> "i1"
    Doub   -> "double"
    String -> "i8*"

instance ToJVM MulOp where
  toJVM op = case op of
    Times -> "mul"
    Div   -> "div"
    Mod   -> "srem"

instance ToJVM AddOp where
  toJVM op = case op of
    Plus  -> "add"
    Minus -> "sub"


instance ToJVM RelOp where
  toJVM op = case op of
    LTH   -> "slt"
    GTH   -> "sgt"
    LE    -> "sle"
    GE    -> "sge"
    EQU   -> "eq"
    NE    -> "ne"

builtin :: [(Ident, FunHead)]
builtin =
  [ (Ident "printInt",     FunHead (Ident "printInt") $ FunType Void [Int])
  , (Ident "printDouble",  FunHead (Ident "printDouble") $ FunType Void [Doub])
  , (Ident "printString",  FunHead (Ident "printString") $ FunType Void [String])
  , (Ident "readInt"   ,   FunHead (Ident "readInt") $ FunType Int [])
  , (Ident "readDouble",   FunHead (Ident "readDouble") $ FunType Doub [])
  ]

newtype Register = R {theRegister :: Int}
  deriving (Eq, Enum, Show)

-- more like value or register
data Value = LitInt Integer | LitDoub Double | LitBool Bool | LitString String | Reg Register
  deriving(Show)

data Code
  = Store LLVMType Value LLVMType Register
  | Load Register LLVMType LLVMType Register
  -- | IConst Integer
  -- | DConst Double
  -- | Dup Type
  -- | Pop Type
  | Return LLVMType Value
  | ReturnVoid
  | Call Register LLVMType Ident Arguments
  | CallVoid LLVMType Ident Arguments

  | Label Label
  -- | Goto Label
  -- | If RelOp Label
  -- | IfCmp Type RelOp Label
  -- | DCmp
  -- | Inc Type Addr Int
  | Add Register AddOp LLVMType Value Value
  | Mul Register MulOp LLVMType Value Value
  -- | I2D
  | Compare Register RelOp LLVMType Value Value
  | Alloca Register LLVMType
  -- | Assign Value Code
  | Branch Label
  | BranchCond Value Label Label
  | Comment String
    deriving (Show)

{-
pattern IfZ :: Label -> Code
pattern IfZ l = If EQU l
pattern IfNZ :: Label -> Code
pattern IfNZ l = If NE l
-}

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

--instance ToJVM FunType where
  --toJVM (FunType t ts) = "(" ++ (toJVM =<< ts) ++ ")" ++ toJVM t

instance ToJVM FunHead where
  toJVM (FunHead (Ident f) (FunType t ts)) = "define " ++ toJVM t ++ " @" ++ f ++ "(" ++ (toJVM =<< ts) ++ ")"

instance ToJVM Label where
  toJVM (L l) = show l

prefix :: Type -> String
prefix Int    = ""
prefix Bool   = ""
prefix Doub   = "f"
prefix Void   = ""
prefix String = ""

{-
sep :: Int -> Int -> String
sep i n | i >= n    = "_"
        | otherwise = " "

isByte :: Integer -> Bool
isByte n | n < 256 || (-128) <= n && n < 128 = True
         | otherwise                         = False
-}

impossible :: Code -> String
impossible a = error $ "impossible code " ++ toJVM a

instance ToJVM Value where
  toJVM = \case
    LitInt i -> show i
    LitDoub d -> show d
    LitBool True -> "1" 
    LitBool False -> "0" 
    LitString s -> show s 
    Reg r -> toJVM r
    
instance ToJVM Register where
  toJVM (R r) = show r

newtype Arguments = Args [(LLVMType, Value)]
  deriving(Show)

instance ToJVM Arguments where
  toJVM (Args []) = ""
  toJVM (Args [(t, v)]) = toJVM t ++ " " ++ toJVM v
  toJVM (Args ((t, v):as)) = toJVM t ++ " " ++ toJVM v ++ ", " ++ toJVM (Args as)

instance ToJVM Code where
  toJVM = \case
    Store t1 from t2 to                     -> "store " ++ toJVM t1 ++ toJVM from ++ " , " ++ toJVM t2  ++ " " ++ toJVM to
    Load adr t1 t2 reg                      -> toJVM adr ++ " = load " ++ toJVM t1 ++ " , " ++ toJVM t2 ++ " " ++ toJVM reg
    Return t v                              -> "ret " ++ toJVM t ++ " " ++ toJVM v
    ReturnVoid                              -> "ret"
    Call adr t (Ident f) args               -> toJVM adr ++ " = call " ++ toJVM t ++ " @" ++ f ++ "(" ++ toJVM args ++ ")"
    CallVoid t (Ident f) args               -> "call " ++ toJVM t ++ " @" ++ f ++ "(" ++ toJVM args ++ ")"
    --DConst d  -> "ldc2_w " ++ show d

    --IConst i | i == -1          -> "iconst_m1"
   --          | i >= 0 && i <= 5 -> "iconst_" ++ show i
    --         | isByte i         -> "bipush " ++ show i
     --        | otherwise        -> "ldc " ++ show i

    --Dup   Doub         -> "dup2"
    --Dup   _                   -> "dup"
    --Pop   Doub         -> "pop2"
    --Pop   _                   -> "pop"

    Label l                               -> toJVM l ++ ":"
    --Goto  l                   -> "goto " ++ toJVM l
    Compare adr op t v1 v2 | t == Lit Int   -> toJVM adr ++ " = icmp " ++ toJVM op ++ " " ++ toJVM v1 ++ ", " ++ toJVM v2
                           | t == Lit Doub  -> toJVM adr ++ " = fcmp " ++ toJVM op ++ " " ++ toJVM v1 ++ ", " ++ toJVM v2
    --If op                   l -> "if" ++ toJVM op ++ " " ++ toJVM l

    --c@(IfCmp Doub _ _) -> impossible c
    --c@(IfCmp Void   _ _) -> impossible c

    --IfCmp _ op l              -> "if_icmp" ++ toJVM op ++ " " ++ toJVM l
    --DCmp                      -> "dcmpg"

     --Inc Int a k          -> "iinc " ++ show a ++ " " ++ show k
    --c@Inc{}                   -> impossible c

    Add adr op t v1 v2 | t == Lit Int           -> toJVM adr ++ " = "        ++ toJVM op ++ " " ++ toJVM t ++ " " ++ toJVM v1 ++ ", " ++ toJVM v2
                       | t == Lit Doub          -> toJVM adr ++ " = " ++ "f" ++ toJVM op ++ " " ++ toJVM t ++ " " ++ toJVM v1 ++ ", " ++ toJVM v2
    Mul adr op t v1 v2 | t == Lit Int           -> toJVM adr ++ " = "        ++ toJVM op ++ " " ++ toJVM t ++ " " ++ toJVM v1 ++ ", " ++ toJVM v2
                       | t == Lit Doub          -> toJVM adr ++ " = " ++ "f" ++ toJVM op ++ " " ++ toJVM t ++ " " ++ toJVM v1 ++ ", " ++ toJVM v2
    
    Alloca adr t                                -> toJVM adr ++ " = alloca " ++ toJVM t

    --Assign adr c                          -> ToJVM adr ++ " = " ++ ToJVM c 

    Branch lb                               -> "br Label " ++ toJVM lb
    BranchCond c lb1 lb2                    -> "br i1 " ++ toJVM c ++ " , Label " ++ toJVM lb1 ++ " , Label " ++ toJVM lb1
 
    --I2D                       -> "i2d"

    Comment ""                              -> ""
    Comment s                               -> "; " ++ s

compileDef :: Sig -> TopDef -> String
compileDef sig0 def@(FnDef t f args (Block ss)) = intercalate ""
  [ toJVM (FunHead f $ funType def), " {\n"
  , "entry:\n"
  --, [ ".limit locals " ++ show (limitLocals st)
   -- , ".limit stack " ++ show (limitStack st)
   -- ]
  , unlines $ map (indent . toJVM) $ reverse (output st)
  , "}\n"
  ]
  where st = execState (compileFun t args ss) $ initSt sig0

-- compile the given function
compileFun :: Type -> [Arg] -> [Stmt] -> Compile ()
compileFun _ args ss = do
  mapM_ (\(Argument t' x) -> newVar x (Lit t')) args
  mapM_ compileStm ss
  --emit $ ReturnVoid --------------------- todo (was used to fix shit that end with if else?)

-- creates a comment 
stmTop :: Stmt -> String
stmTop = \case
  (Ret e    ) -> "return " ++ printTree e  -- ?
  (While e _   ) -> "while (" ++ printTree e ++ ")" -- ++ printTree s
  (CondElse e _ _) -> "if (" ++ printTree e ++ ")"
  (BStmt _     ) -> ""
  s               -> printTree s

-- string comment
comment :: String -> Compile ()
comment = emit . Comment

-- blank line
blank :: Compile ()
blank = comment ""

getPrevResult :: Compile Value
getPrevResult = do
  allArgs <- gets prevResult
  return $ head (take 1 allArgs)

-- help function for compiling variable declaration
compileDecl :: Type -> Item -> Compile ()
compileDecl t (Init id (ETyped e _)) = do
  newVar id (Lit t)
  (a, _) <- lookupVar id
  compileExp (ETyped e t) False
  p <- getPrevResult
  r <- newRegister
  emit $ Alloca r (Lit t)
  emit $ Store (Lit t) p (Ptr (Lit t)) r

  --emit $ Store t a
compileDecls t (NoInit id) = do
  newVar id (Lit t)      -------------------------------- todo should be t*?, i guess newvar should turn it into * by default
  r <- newRegister
  emit $ Alloca r (Ptr (Lit t))

-- compile statement
compileStm :: Stmt -> Compile ()
compileStm s0 = do
  --let top = stmTop s0
  --unless (null top) $ do
    --blank
    --mapM_ comment $ lines top
  case s0 of
    Ret e@(ETyped _ t) -> do
      compileExp e False
      r <- getPrevResult
      emit $ Return (Lit t) r
    VRet -> do
      emit $ ReturnVoid ----------------------------------- rätt?

    Decl t ds -> do
      mapM_ (compileDecl t) ds
    SExp e@(ETyped _ t) -> do
      compileExp e False
      --emit $ Pop t
      -- removeArgs 1? -------------------------- todo
    While e s -> do
      start <- newLabel
      t     <- newLabel
      f     <- newLabel
      emit $ Label start
      --compileCond False l2 e
      compileExp e False -------------  todo true? idk
      r <- getPrevResult
      emit $ BranchCond r t f
      emit $ Label t
      inNewBlock $ compileStm s
      --emit $ Goto start
      emit $ Label f
    BStmt (Block ss) -> do
      inNewBlock $ compileStms ss
     where
      compileStms :: [Stmt] -> Compile ()
      compileStms [] = return ()
      compileStms (s : ss') = do
        compileStm s
        compileStms ss'

    CondElse e s1 s2 -> do
      t   <- newLabel
      f   <- newLabel
      end <- newLabel
      compileExp e False
      r <- getPrevResult
      emit $ BranchCond r t f
      emit $ Label t
      inNewBlock $ compileStm s1
      emit $ Branch end
      emit $ Label f
      inNewBlock $ compileStm s2
      emit $ Label end

    Cond e s -> do
      t   <- newLabel
      f   <- newLabel
      compileExp e False
      r <- getPrevResult
      emit $ BranchCond r t f
      emit $ Label t
      inNewBlock $ compileStm s
      emit $ Label f

    Ass x e -> do
      compileExp e False
      (a, t) <- lookupVar x
      r <- getPrevResult
      emit $ Store t r (Ptr t) a

    Empty -> return ()
    Incr i -> do
      (adr, t) <- lookupVar i
      adr' <- newRegister
      if (t == (Lit Int))
        then do
          emit $ Add adr' Plus t (Reg adr) (LitInt 1)
          emit $ Store t (Reg adr') (Ptr t) adr
        else do
          emit $ Add adr' Plus t (Reg adr) (LitDoub 1.0)
          emit $ Store t (Reg adr') (Ptr t) adr

    Decr i -> do
      (adr, t) <- lookupVar i
      adr' <- newRegister
      if (t == (Lit Int))
        then do
          emit $ Add adr' Minus t (Reg adr) (LitInt 1)
          emit $ Store t (Reg adr') (Ptr t) adr
        else do
          emit $ Add adr' Minus t (Reg adr) (LitDoub 1.0)
          emit $ Store t (Reg adr') (Ptr t) adr

    s -> error $ "not implemented compileStm " ++ printTree s

boolLitToBool :: Expr -> Bool
boolLitToBool ELitTrue = True
boolLitToBool ELitFalse = False

{-
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
-}

-- remove arguments from argument stack
removeArgs :: Int -> Compile()
removeArgs n = do
  modify $ \st@St { prevResult = vs } -> st { prevResult = drop n vs } ------------ reverse?

--convert Type to LLVMType
type2LLVMType :: Type -> LLVMType
type2LLVMType t = Lit t

-- compile expression
-- Bool: this expr is an argument to a function
compileExp :: Expr -> Bool -> Compile ()
compileExp e0 b = case e0 of
  --ETyped e Doub -> error $ printTree e --compileExp e
  ELitInt i  -> setPrevVal (LitInt i) b
  ELitTrue   -> setPrevVal (LitBool True) b
  ELitFalse  -> setPrevVal (LitBool False) b ---------- 1/ 0 ?
  ELitDoub d -> setPrevVal (LitDoub d) b
  EString s  -> setPrevVal (LitString s) b

{-
  ETyped (ELitInt i) Doub -> do
    emit $ IConst i
    emit I2D
    -}
{-
  ETyped (ETyped e Int) Doub -> do
    compileExp (ETyped e Int)
    emit I2D
    -}

  EVar x -> do
    (a, t) <- lookupVar x
    --emit $ Load t a
    setPrevVal (Reg a) b

  EApp x@(Ident _) es -> do
    mapM_ (\e -> compileExp e True) es
    FunHead id (FunType t ts) <- gets ((fromMaybe (error "undefined") . Map.lookup x) . sig)
    let n_args = length ts
    allArgs <- gets prevResult
    let args = take n_args allArgs ---------------------------- reverse? todo
    let ts' = map (\x -> (Lit x)) ts
    let args' = zip ts' args
    if (t == Void)
      then do
        emit $ CallVoid (Lit t) id (Args args')
        removeArgs n_args
      else do
        r <- newRegister
        emit $ Call r   (type2LLVMType t) id (Args args')
        removeArgs n_args
        setPrevVal (Reg r) b


  --EPre op i -> do
    --(a, t) <- lookupVar i
    --case op of
      --OInc -> emit $ Inc t a 1
      --ODec -> emit $ Inc t a (-1)
    --emit $ Load t a

  ETyped (EMul e1 op e2) t -> do
    compileExp e1 True
    compileExp e2 True
    r <- newRegister
    allArgs <- gets prevResult
    let args = take 2 allArgs
    emit $ Mul r op (type2LLVMType t) (head args) (head (tail args))
    removeArgs 2
    setPrevVal (Reg r) b

  ETyped (EAdd e1 op e2) t -> do
    compileExp e1 True
    compileExp e2 True
    r <- newRegister
    allArgs <- gets prevResult
    let args = take 2 allArgs
    emit $ Add r op (type2LLVMType t) (head args) (head (tail args))
    removeArgs 2
    setPrevVal (Reg r) b

  ETyped (ERel e1@(ETyped _ t1) op e2) _ -> do
    compileExp e1 True
    compileExp e2 True
    r <- newRegister
    allArgs <- gets prevResult
    let args = take 2 allArgs
    emit $ Compare r op (type2LLVMType t1) (head args) (head (tail args))
    removeArgs 2
    setPrevVal (Reg r) b
    -- br i1 %t2, label %lab2, label %lab1

  EAnd e1 e2 -> do
    t  <- newLabel
    t2 <- newLabel
    f  <- newLabel

    -- e1 true?
    compileExp e1 True
    allArgs <- gets prevResult
    let e1_result = head allArgs
    emit $ BranchCond e1_result t f 

    -- e2 true?
    emit $ Label t 
    compileExp e2 False -- ok to overwrite e1_result
    allArgs2 <- gets prevResult
    let e2_result = head allArgs2
    emit $ BranchCond e2_result t2 f 

    emit $ Label t2
    removeArgs 1
    setPrevVal (LitBool True) b
    emit $ Label f
    removeArgs 1
    setPrevVal (LitBool False) b

  EOr e1 e2 -> do
    t  <- newLabel
    f  <- newLabel
    f2 <- newLabel

    -- e1 true?
    compileExp e1 True
    allArgs <- gets prevResult
    let e1_result = head allArgs
    emit $ BranchCond e1_result t f 

    -- e2 true?
    emit $ Label f 
    compileExp e2 False -- ok to overwrite e1_result
    allArgs2 <- gets prevResult
    let e2_result = head allArgs2
    emit $ BranchCond e2_result t f2 

    emit $ Label f2
    removeArgs 1
    setPrevVal (LitBool False) b
    emit $ Label t
    removeArgs 1
    setPrevVal (LitBool True) b

  Neg (ETyped e t) -> do
    if (t == Int)
      then compileExp (ETyped (EMul e Times (ELitInt  (-1)  ) ) t) b
      else compileExp (ETyped (EMul e Times (ELitDoub (-1.0)) ) t) b
    

  Not (ETyped e Bool) -> do
    t <- newLabel
    f <- newLabel
    compileExp e True
    allArgs <- gets prevResult
    let e_result = head allArgs
    emit $ BranchCond e_result t f

    emit $ Label t
    removeArgs 1
    setPrevVal (LitBool False) b

    emit $ Label f
    removeArgs 1
    setPrevVal (LitBool True) b --- todo, save to adress?

  ETyped e _ -> compileExp e b

  e          -> error $ "not implemented compileexp " ++ show e
{-
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
-}

-- create the next label name
newLabel :: Compile Label
newLabel = do
  l <- gets nextLabel
  modify $ \st -> st { nextLabel = succ l }
  return l

-- create the next register name
newRegister :: Compile Register
newRegister = do
  v <- gets nextVar
  modify $ \st -> st { nextVar = succ v }
  return v

-- create new block for if while loops etc
inNewBlock :: Compile a -> Compile a
inNewBlock cont = do
  modify $ \st -> st { cxt = [] : cxt st }
  a <- cont
  modify $ \st -> st { cxt = tail $ cxt st }
  return a

-- add new variable to the state
newVar :: Ident -> LLVMType -> Compile ()
newVar x t = do
  modify $ \st@St { cxt = (b : bs) } -> st { cxt = ((x, t) : b) : bs }
  --updateLimitLocals

-- get type and register for a variable
lookupVar :: Ident -> Compile (Register, LLVMType)
lookupVar x = gets ((loop . concat) . cxt)
 where
  loop [] = error $ "unbound variable " ++ printTree x
  loop ((y, t) : bs) | x == y    = (R $ length bs, t)
                     | otherwise = loop bs

{-
lookupVar x = do
  (v:vars) <- gets cxt
  searchBlocks vars
  where
    searchBlocks [] = Nothing
    searchBlocks (b:blocks) = do
      s = searchBlock b
      if (not isNothing s) 
        then s
        else searchBlocks blocks
    where
      searchBlock [] = Nothing
      searchBlock ((id, t):vars) = do
        if (id == x)
          then Just 
-}


{-
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
-}


-- add llvm code line to output
emit :: Code -> Compile ()
emit (Store (Lit Void) _ _ _) = return ()
emit (Load  _ (Lit Void) _ _) = return ()
emit c = do
  modify $ \st@St { output = cs } -> st { output = c : cs }
  --adjustStack c
--emit (Dup Void        ) = return ()
--emit (Pop Void        ) = return ()

{-}
emit (Inc t@Doub a k) = do
  emit $ Load t a
  emit $ DConst $ fromIntegral k
  emit $ Add t Plus
  emit $ Store t a

emit (IfCmp Doub o l) = do
  emit DCmp
  emit $ If o l
  -}


{-
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
-}
