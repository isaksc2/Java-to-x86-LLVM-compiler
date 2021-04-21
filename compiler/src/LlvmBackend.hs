









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


-- | Indent non-empty, non label lines.
indent :: String -> String
indent s | null s = s
indent s | last s == ":" = s
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
  , nextVar      :: Value
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
              , nextVar      = Reg 0
              , output       = []
              , prevResult   = []
              }

type Addr = Int

data LLVMType = Lit Type | Ptr Type

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

-- update or add argument
setPrevVal :: Value -> Bool -> Compile ()
setPrevVal v False = modify $ \st@St { prevResult = v0:vs } -> st { prevResult = v : vs } ------------ reverse?
setPrevVal v True  = modify $ \st@St { prevResult =    vs } -> st { prevResult = v : vs } ------------ reverse?

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

instance ToJVM LLVMType where
  toJVM = \case
    Ptr ptr -> ToJVM ptr ++ "*" 
    Lit lit -> ToJVM lit

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
  [ (Ident "printInt",     FunHead (Ident "Runtime/printInt") $ FunType Void [Int])
  , (Ident "printDouble",  FunHead (Ident "Runtime/printDouble") $ FunType Void [Doub])
  , (Ident "printString",  FunHead (Ident "Runtime/printString") $ FunType Void [String])
  , (Ident "readInt"   ,   FunHead (Ident "Runtime/readInt") $ FunType Int [])
  , (Ident "readDouble",   FunHead (Ident "Runtime/doubleInt") $ FunType Doub [])
  ]

newtype Register = R {theRegister :: Int}
  deriving (Eq, Enum, Show)

-- more like value or register
data Value = LitInt Integer | LitDoub Double | Reg Register

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
  | Alloca Register Type
  -- | Assign Value Code
  | Branch Label
  | BranchCond Register Label Label
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

--instance ToJVM FunType where
  --toJVM (FunType t ts) = "(" ++ (toJVM =<< ts) ++ ")" ++ toJVM t

instance ToJVM FunHead where
  toJVM (FunHead (Ident f) (FunType t ts)) = "define " ++ ToJVM t ++ " @" ++ show f ++ "(" ++ (toJVM =<< ts) ++ ")"

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
    Reg r -> ToJVM R
    
instance ToJVM Register where
  toJVM (R r) = show r

newtype Arguments = Args [(LLVMType, Value)]

instance ToJVM Arguments where
  toJVM [] = ""
  toJVM [(t, v)] = ToJVM t ++ " " ++ ToJVM v
  toJVM ((t, v):as) = ToJVM t ++ " " ++ ToJVM v ++ ", " ++ ToJVM as

instance ToJVM Code where
  toJVM = \case
    Store t1 from t2 to                     -> "store " ++ ToJVM t1 ++ ToJVM from " , " ++ ToJVM t2  ++ " " ++ ToJVM to
    Load adr t1 t2 reg                      -> ToJVM adr ++ " = load " ++ toJVM t1 ++ " , " ++ toJVM t2 ++ " " ++ toJVM reg
    Return t v                              -> "ret " ++ ToJVM t ++ " " ++ ToJVM v
    ReturnVoid                              -> "ret"
    Call adr t (Ident f) args               -> ToJVM adr ++ " = call " ++ ToJVM t ++ " @" ++ show f ++ "(" ++ ToJVM args ")"
    CallVoid t (Ident f) args               -> "call " ++ ToJVM t ++ " @" ++ show f ++ "(" ++ ToJVM args ")"
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
    Compare adr op t v1 v2 | t == Lit Int   -> ToJVM adr ++ " = icmp " ++ ToJVM op ++ " " ++ ToJVM v1 ++ ", " ++ ToJVM v2
                           | t == Lit Doub  -> ToJVM adr ++ " = fcmp " ++ ToJVM op ++ " " ++ ToJVM v1 ++ ", " ++ ToJVM v2
    --If op                   l -> "if" ++ toJVM op ++ " " ++ toJVM l

    --c@(IfCmp Doub _ _) -> impossible c
    --c@(IfCmp Void   _ _) -> impossible c

    --IfCmp _ op l              -> "if_icmp" ++ toJVM op ++ " " ++ toJVM l
    --DCmp                      -> "dcmpg"

     --Inc Int a k          -> "iinc " ++ show a ++ " " ++ show k
    --c@Inc{}                   -> impossible c

    Add adr op t v1 v2 | t1 == Lit Int          -> ToJVM adr ++ " = "        ++ ToJVM op ++ " " ++ ToJVM t ++ " " ++ ToJVM v1 ++ ", " ++ ToJVM v2
                       | t1 == Lit Doub         -> ToJVM adr ++ " = " ++ "f" ++ ToJVM op ++ " " ++ ToJVM t ++ " " ++ ToJVM v1 ++ ", " ++ ToJVM v2
    Mul adr op t v1 v2 | t1 == Lit Int          -> ToJVM adr ++ " = "        ++ ToJVM op ++ " " ++ ToJVM t ++ " " ++ ToJVM v1 ++ ", " ++ ToJVM v2
                       | t1 == Lit Doub         -> ToJVM adr ++ " = " ++ "f" ++ ToJVM op ++ " " ++ ToJVM t ++ " " ++ ToJVM v1 ++ ", " ++ ToJVM v2
    
    Alloca adr t                                -> ToJVM adr ++ " = alloca " ++ ToJVM t

    --Assign adr c                          -> ToJVM adr ++ " = " ++ ToJVM c 

    Branch lb                               -> "br Label " ++ ToJVM lb
    BranchCond c lb1 lb2                    -> "br i1 " ++ ToJVM c ++ " , Label " ++ ToJVM lb1 ++ " , Label " ++ ToJVM lb1
 
    --I2D                       -> "i2d"

    Comment ""                              -> ""
    Comment s                               -> "; " ++ s

compileDef :: Sig -> TopDef -> [String]
compileDef sig0 def@(FnDef t f args (Block ss)) = concat
  [ ["", toJVM (FunHead f $ funType def), " {"]
  , ["", "entry:"]
  --, [ ".limit locals " ++ show (limitLocals st)
   -- , ".limit stack " ++ show (limitStack st)
   -- ]
  , map (indent . toJVM) $ reverse (output st)
  , ["", " }"]
  ]
  where st = execState (compileFun t args ss) $ initSt sig0

-- compile the given function
compileFun :: Type -> [Arg] -> [Stmt] -> Compile ()
compileFun _ args ss = do
  mapM_ (\(Argument t' x) -> newVar x t') args
  mapM_ compileStm ss
  emit $ Return Void -----------------------------------------------------------------------

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
  take 1 allArgs

-- help function for compiling variable declaration
compileDecl :: Type -> Item -> Compile ()
compileDecl t (Init id (ETyped e _)) = do
  newVar id t
  (a, _) <- lookupVar id
  compileExp (ETyped e t)
  p <- getPrevResult
  r <- newRegister
  emit $ Alloca r t
  emit $ Store t p (Ptr t) r

  --emit $ Store t a
compileDecls t (NoInit id) = do
  newVar id t
  r <- newRegister
  emit $ Alloca r t

-- compile statement
compileStm :: Stmt -> Compile ()
compileStm s0 = do
  let top = stmTop s0
  unless (null top) $ do
    blank
    mapM_ comment $ lines top
  case s0 of
    Ret e@(ETyped _ t) -> do
      compileExp e
      emit $ Return t getPrevResult
    VRet -> do
      emit $ ReturnVoid ----------------------------------- rätt?

    Decl t ds -> do
      mapM_ (compileDecl t) ds
    SExp e@(ETyped _ t) -> do
      compileExp e
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
      emit $ Goto start
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
      inNewBlock $ compileStm s1
      emit $ Label f

    Ass x e -> do
      compileExp e False
      (a, pt@(Ptr t)) <- lookupVar x
      r <- getPrevResult
      emit $ Store t r pt a

    Empty -> return ()
    Incr i -> do
      (adr, (Ptr t)) <- lookupVar i
      adr' <- newRegister
      if (t == Int)
        then v = 1
        else v = 1.0
      emit $ Add adr' Plus t adr v
      emit $ Store t adr' (Ptr t) adr

    Decr i -> do
      (adr, (Ptr t)) <- lookupVar i
      adr' <- newRegister
      if (t == Int)
        then v = 1
        else v = 1.0
      emit $ Add adr' Minus t adr v
      emit $ Store t adr' (Ptr t) adr

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
removeArgs :: Integer -> Compile()
removeArgs n -> do
  modify $ \st@St { prevResult = vs } -> st { prevResult = drop n vs } ------------ reverse?

--convert Type to LLVMType
Type2LLVMType :: Type -> LLVMType
Type2LLVMType t = Lit t

-- compile expression
-- Bool: this expr is an argument to a function
compileExp :: Expr -> Bool -> Compile ()
compileExp  = \case
  --ETyped e Doub -> error $ printTree e --compileExp e
  (ELitInt i) b -> setPrevVal (Lit i) b
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

  (EVar x) b -> do
    (a, t) <- lookupVar x
    --emit $ Load t a
    setPrevVal a b

  (EApp x@(Ident _) es) b -> do
    mapM_ compileExp es True
    (id (FunType t ts)) <- gets ((fromMaybe (error "undefined") . Map.lookup x) . sig)
    let n_args = length ts
    allArgs <- gets prevResults
    let args = take n_args allArgs
    let args' = zip ts args
    if (t == Void)
      then
        emit $ CallVoid (Type2LLVMType t) id args'
        removeArgs n_args
      else do
        r <- newRegister
        emit $ Call r   (Type2LLVMType t) id args'
        removeArgs n_args
        setPrevVal r b

  ELitTrue b -> setPrevVal (Lit 1) b
  ELitFalse b -> setPrevVal (Lit 0) b

  (ELitDoub d) b  -> setPrevVal (Lit d) b

  (EString s) b -> setPrevVal (Lit s) b

  --EPre op i -> do
    --(a, t) <- lookupVar i
    --case op of
      --OInc -> emit $ Inc t a 1
      --ODec -> emit $ Inc t a (-1)
    --emit $ Load t a

  (ETyped (EMul e1 op e2) t) b -> do
    compileExp e1 True
    compileExp e2 True
    r <- newRegister
    allArgs <- gets prevResults
    let args = take 2 allArgs
    emit $ Mul r op (Type2LLVMType t) args[0] args[1]
    removeArgs 2
    setPrevVal r b

  (ETyped (EAdd e1 op e2) t) b -> do
    compileExp e1 True
    compileExp e2 True
    r <- newRegister
    allArgs <- gets prevResults
    let args = take 2 allArgs
    emit $ Add r op (Type2LLVMType t) args[0] args[1]
    removeArgs 2
    setPrevVal r b

  (ETyped (ERel e1@(ETyped _ t1) op e2) _) b -> do
    compileExp e1 True
    compileExp e2 True
    r <- newRegister
    allArgs <- gets prevResults
    let args = take 2 allArgs
    emit $ Compare r op (Type2LLVMType t) args[0] args[1]
    removeArgs 2
    setPrevVal r b
    -- br i1 %t2, label %lab2, label %lab1

  EAnd e1 e2 -> do
    t  <- newLabel
    t2 <- newLabel
    f  <- newLabel

    -- e1 true?
    compileExp e1 True
    allArgs <- gets prevResults
    let e1_result = take 1 allArgs
    emit $ BranchCond e1_result t f 

    -- e2 true?
    emit $ Label t 
    compileExp e2 False -- ok to overwrite e1_result
    allArgs <- gets prevResults
    let e2_result = take 1 allArgs
    emit $ BranchCond e2_result t2 f 

    emit $ Label t2
    removeArgs 1
    setPrevVal (Lit true) b
    emit $ Label f
    removeArgs 1
    setPrevVal (Lit false) b

  EOr e1 e2 -> do
    t  <- newLabel
    f  <- newLabel
    f2 <- newLabel

    -- e1 true?
    compileExp e1 True
    allArgs <- gets prevResults
    let e1_result = take 1 allArgs
    emit $ BranchCond e1_result t f 

    -- e2 true?
    emit $ Label f 
    compileExp e2 False -- ok to overwrite e1_result
    allArgs <- gets prevResults
    let e2_result = take 1 allArgs
    emit $ BranchCond e2_result t f2 

    emit $ Label f2
    removeArgs 1
    setPrevVal (Lit false) b
    emit $ Label t
    removeArgs 1
    setPrevVal (Lit true) b

  Neg (ETyped e t) -> do
    if (t == Int)
      then 
        e2 = (ELitInt -1)
      else
        e2 = (ELitDoub -1.0)
    compileExp (ETyped (EMul e Times e2 ) t) b

  Not (ETyped e Bool) -> do
    t <- newLabel
    f <- newLabel
    compileExp e True
    allArgs <- gets prevResults
    let e_result = take 1 allArgs
    emit $ BranchCond e_result t f

    emit $ Label t
    removeArgs 1
    setPrevVal (Lit false) b

    emit $ Label f
    removeArgs 1
    setPrevVal (Lit true) b --- todo, save to adress?

  ETyped e _ -> compileExp e

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
newRegister :: Compile Value
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
  updateLimitLocals

-- get type and register for a variable
lookupVar :: Ident -> Compile (Register, LLVMType)
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


-- add llvm code line to output
emit :: Code -> Compile ()
emit (Store Void _    ) = return ()
emit (Load  Void _    ) = return ()
emit c = do
  modify $ \st@St { output = cs } -> st { output = c : cs }
  adjustStack c
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
