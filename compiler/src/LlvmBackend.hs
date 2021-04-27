









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
  --let code = map (compileDef sig0) defs
  let code = compileDefs (initSt sig0) defs
  let globs = intercalate "" (map snd code)
  let funcs = unlines (map fst code)
  let decl = unlines ["declare void @printInt(i32)", "declare void @printDouble(double)", "declare void @printString(i8*)", "declare i32 @readInt()", "declare double @readDouble()"]
  let prog = decl ++ globs ++ "\n" ++ funcs
  reverse $ drop 2 $ reverse $ prog -- remove the last 2 extra newlines
  where
    sig0 = Map.fromList $ builtin ++ map sigEntry defs
    sigEntry def@(FnDef _ f@(Ident x) _ _) =
      (f, ) $ FunHead (Ident x) $ funType def


-- | Indent non-empty, non label lines.
indent :: String -> String
indent s | null s        = s
indent ""                = ""
indent s | last s == ':' = s
indent s | head s == '@' = s
indent s                 = "\t" ++ s

type Sig = Map Ident FunHead
type Cxt = [Block]
type Block = [(Ident, Register, LLVMType)]


data St = St
  { sig          :: Sig
  , cxt          :: Cxt
  , limitLocals  :: Int
  , currentStack :: Int
  , limitStack   :: Int
  , nextLabel    :: Label
  , nextReg      :: Register
  , nextVar      :: Variable
  , output       :: Output
  , prevResult   :: [(Value, LLVMType)]
  , globalOut    :: Output
  , globals      :: Block
  , nextGlobal   :: GlobalRegister
  , params       :: [Register]
  }

initSt :: Sig -> St
initSt s = St { sig          = s
              , cxt          = [[]]
              , limitLocals  = 0
              , currentStack = 0
              , limitStack   = 0
              , nextLabel    = L 0
              , nextReg      = R 0
              , nextVar      = V 0
              , output       = []
              , prevResult   = []
              , globalOut    = []
              , globals      = []
              , nextGlobal   = G 0
              , params       = []
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
setPrevVal :: (Value, LLVMType) -> Bool -> Compile ()
setPrevVal v False = do 
  vs <- gets prevResult
  if (length vs == 0)
    then modify $ \st@St { prevResult = vs } -> st { prevResult = [v] }
    else modify $ \st@St { prevResult = vs } -> st { prevResult = v : (tail vs) }
  --modify $ \st@St { prevResult = vs } -> st { prevResult = v : (tail vs) } ------------ reverse?
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

prefixMulOp :: LLVMType -> MulOp -> String
prefixMulOp (Lit Int) Times = "mul"
prefixMulOp (Lit Doub) Times = "fmul"
prefixMulOp (Lit Doub) Div = "fdiv"
prefixMulOp (Lit Int) Div = "sdiv"
prefixMulOp (Lit Int) Mod = "srem"

{-
instance ToJVM MulOp where
  toJVM op = case op of
    Times -> "mul"
    Div   -> "div"
    Mod   -> "srem"
-}
instance ToJVM AddOp where
  toJVM op = case op of
    Plus  -> "add"
    Minus -> "sub"

prefixRelOp :: LLVMType -> RelOp -> String
prefixRelOp (Lit Doub) op                         = "o" ++ toJVM op
prefixRelOp (Lit Int)  op | op == EQU || op == NE =        toJVM op
prefixRelOp (Lit Int)  op                         = "s" ++ toJVM op
prefixRelOp (Lit Bool) op                         = prefixRelOp (Lit Int) op

instance ToJVM RelOp where
  toJVM op = case op of
    LTH   -> "lt"
    GTH   -> "gt"
    LE    -> "le"
    GE    -> "ge"
    EQU   -> "eq"
    NE    -> "ne"

builtin :: [(Ident, FunHead)] ------------------todo remove?
builtin =
  [ (Ident "printInt",     FunHead (Ident "printInt") $ FunType Void [Int])
  , (Ident "printDouble",  FunHead (Ident "printDouble") $ FunType Void [Doub])
  , (Ident "printString",  FunHead (Ident "printString") $ FunType Void [String])
  , (Ident "readInt"   ,   FunHead (Ident "readInt") $ FunType Int [])
  , (Ident "readDouble",   FunHead (Ident "readDouble") $ FunType Doub [])
  ]

newtype Register = R {theRegister :: Int}
  deriving (Eq, Enum, Show)

newtype Variable = V {theVariable :: Int}
  deriving (Eq, Enum, Show)

newtype GlobalRegister = G Int
  deriving (Eq, Enum, Show)

-- more like value or register
data Value = LitInt Integer | LitDoub Double | LitBool Bool | LitString String | Reg Register | Glob GlobalRegister
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
  | Global GlobalRegister LLVMType Value
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
  toJVM (FunHead (Ident f) (FunType t ts)) = "define " ++ toJVM t ++ " @" ++ f ++ "(" ++ ( reverse ( drop 2 ( reverse (((\t -> t ++ ", ") . toJVM) =<< ts)))) ++ ")"

instance ToJVM Label where
  toJVM (L l) = "lab" ++ show l

prefix :: Type -> String
prefix Int    = "s"
prefix Bool   = ""
prefix Doub   = "o"
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
    LitInt i      -> show i
    LitDoub d     -> show d
    LitBool True  -> "1" 
    LitBool False -> "0" 
    LitString s   -> error $ "can only print adress to string, not string directly"
    Reg r         -> toJVM r
    Glob g        -> toJVM g 
    
instance ToJVM Register where
  toJVM (R r) = do
    let f = ""
    "%r" ++ f ++ show r

instance ToJVM GlobalRegister where
  toJVM (G g) = "@g" ++ show g

newtype Arguments = Args [(LLVMType, Value)]
  deriving(Show)


instance ToJVM Arguments where
  toJVM (Args []) = ""
  toJVM (Args [(t, v)]) = toJVM t ++ " " ++ toJVM v
  toJVM (Args ((t, v):as)) = toJVM t ++ " " ++ toJVM v ++ ", " ++ toJVM (Args as)

instance ToJVM Code where
  toJVM = \case
    Store t1 from t2 to                     -> "store " ++ toJVM t1 ++ " " ++  toJVM from ++ " , " ++ toJVM t2  ++ " " ++ toJVM to
    Load adr t1 t2 reg                      -> toJVM adr ++ " = load " ++ toJVM t1 ++ " , " ++ toJVM t2 ++ " " ++ toJVM reg
    Return t v                              -> "ret " ++ toJVM t ++ " " ++ toJVM v
    ReturnVoid                              -> "ret void"
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

    Label l                                -> toJVM l ++ ":"
    --Goto  l                   -> "goto " ++ toJVM l
    Compare adr op t v1 v2 | t == Lit Doub -> toJVM adr ++ " = fcmp " ++ prefixRelOp t op ++ " " ++ toJVM t ++ " " ++ toJVM v1 ++ ", " ++ toJVM v2
                           | otherwise     -> toJVM adr ++ " = icmp " ++ prefixRelOp t op ++ " " ++ toJVM t ++ " " ++ toJVM v1 ++ ", " ++ toJVM v2
    --If op                   l -> "if" ++ toJVM op ++ " " ++ toJVM l

    --c@(IfCmp Doub _ _) -> impossible c
    --c@(IfCmp Void   _ _) -> impossible c

    --IfCmp _ op l              -> "if_icmp" ++ toJVM op ++ " " ++ toJVM l
    --DCmp                      -> "dcmpg"

     --Inc Int a k          -> "iinc " ++ show a ++ " " ++ show k
    --c@Inc{}                   -> impossible c

    Add adr op t v1 v2 | t == Lit Int           -> toJVM adr ++ " = "        ++ toJVM op    ++ " " ++ toJVM t ++ " " ++ toJVM v1 ++ ", " ++ toJVM v2
                       | t == Lit Doub          -> toJVM adr ++ " = " ++ "f" ++ toJVM op    ++ " " ++ toJVM t ++ " " ++ toJVM v1 ++ ", " ++ toJVM v2
    Mul adr op t v1 v2                          -> toJVM adr ++ " = " ++ (prefixMulOp t op) ++ " " ++ toJVM t ++ " " ++ toJVM v1 ++ ", " ++ toJVM v2
    
    Alloca adr t                                -> toJVM adr ++ " = alloca " ++ toJVM t

    --Assign adr c                          -> ToJVM adr ++ " = " ++ ToJVM c 

    Branch lb                               -> "br label %" ++ toJVM lb
    BranchCond c lb1 lb2                  -> "br i1 " ++ toJVM c ++ ", label %" ++ toJVM lb1 ++ ", label %" ++ toJVM lb2 ----- todo remove t, has to be i1
    Global adr t (LitString s)              -> toJVM adr ++ " = internal constant [" ++ show ( (length s) + 1) ++ " x i8] c\"" ++ s ++ "\\00\""
 
    --I2D                       -> "i2d"

    Comment ""                              -> ""
    Comment s                               -> "; " ++ s
    c -> show c

-- compile function
compileDef :: St -> TopDef -> (String, String, St)
compileDef st def@(FnDef t (Ident f) args (Block ss)) = do
  let args' = Args $ zip (map (\(Argument t id) -> Lit t) args) (map (\x -> Reg x) (params st'))
  let func = intercalate "" [ "define " ++ toJVM t ++ " @" ++ f ++ "(" ++ toJVM args' ++ " ) {\n", "entry:\n", unlines $ map (indent . toJVM) $ reverse $ (output st'), "}\n"]
  let glob = unlines $ map toJVM $ reverse (globalOut st')
  (func, glob, st')
  where st' = execState (compileFun (Ident f) t args ss) st

-- compile functions
compileDefs :: St -> [TopDef] -> [(String, String)]
compileDefs st [] = []
compileDefs st (d:ds) = do
  let (s1,s2,st') = compileDef st d
  let sss = compileDefs st' ds
  (s1,s2):sss
 

-- helper that adds ret void if needed before a label
fixTerminator :: Compile ()
fixTerminator = do
  prev <- gets output
  if (length prev == 0)
    then return ()
    else do
      let keywords = words $ toJVM $ head prev
      if (keywords == [])
        then return ()
        else do
          let instruction = head keywords 
          if ( instruction == "br" || instruction == "ret" ) 
            then return ()
            else return () --emit $ ReturnVoid

-- compile function helper
compileFun :: Ident -> Type -> [Arg] -> [Stmt] -> Compile ()
compileFun (Ident f) t0 args ss = do
  modify $ \st -> st { output = []}
  modify $ \st -> st { globalOut = []}
  regs <- mapM (\(Argument t' x) -> newRegister) args
  let arg_reg = zip args regs
  mapM_ (\(Argument t' x, r) -> newVar x r (Lit t')) arg_reg
  modify $ \st -> st { params = regs}
  mapM_ compileStm ss
  -- add "ret void" if no return statement at the end
  if (t0 == Void)
    then do 
      prevStm <- gets output
      if (length prevStm /= 0 && (head $ words $ toJVM $ head prevStm) == "ret")
        then return ()
        else emit ReturnVoid
    else return ()

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

getPrevResult :: Compile (Value, LLVMType)
getPrevResult = do
  allArgs <- gets prevResult
  return $ head allArgs

-- help function for compiling variable declaration
compileDecl :: Type -> Item -> Compile ()
compileDecl t (Init id (ETyped e _)) = do
  compileExp (ETyped e t) False
  r <- newRegister
  newVar id r (Ptr (Lit t)) ---------------- todo used to only be lit
  emit $ Alloca r (Lit t)
  p <- getPrevResult
  p' <- loadReg p
  emit $ Store (Lit t) p' (Ptr (Lit t)) r

compileDecl t (NoInit id) = do
  r <- newRegister
  newVar id r (Ptr (Lit t))      -------------------------------- todo should be t*?, i guess newvar should turn it into * by default
  emit $ Alloca r (Lit t)



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
      r' <- loadReg r
      emit $ Return (Lit t) r'
    VRet -> do
      emit $ ReturnVoid ----------------------------------- rätt?

    Decl t ds -> do
      mapM_ (compileDecl t) ds
    SExp e@(ETyped _ t) -> do
      compileExp e False
      --emit $ Pop t
      -- removeArgs 1? -------------------------- todo
    While e@(ETyped _ typ) s -> do
      start <- newLabel
      t     <- newLabel
      f     <- newLabel
      emit $ Branch start
      emit $ Label start
      --compileCond False l2 e
      compileExp e False -------------  todo true? idk
      r <- getPrevResult
      r' <- loadReg r
      emit $ BranchCond r' t f
      emit $ Label t
      inNewBlock $ compileStm s
      emit $ Branch start
      emit $ Label f
    BStmt (Block ss) -> do
      inNewBlock $ compileStms ss
     where
      compileStms :: [Stmt] -> Compile ()
      compileStms [] = return ()
      compileStms (s : ss') = do
        compileStm s
        compileStms ss'

    CondElse e@(ETyped _ typ) s1 s2 -> do
      compileExp e False ------------------- todo maybe issue?
      t   <- newLabel
      f   <- newLabel
      r <- getPrevResult
      r' <- loadReg r
      emit $ BranchCond r' t f
      emit $ Label t
      inNewBlock $ compileStm s1
      end <- newLabel
      emit $ Branch end
      emit $ Label f
      inNewBlock $ compileStm s2
      emit $ Branch end
      emit $ Label end

    Cond e@(ETyped _ typ) s -> do
      t   <- newLabel
      f   <- newLabel
      compileExp e False
      r <- getPrevResult
      r' <- loadReg r
      emit $ BranchCond r' t f
      emit $ Label t
      inNewBlock $ compileStm s
      emit $ Branch f
      emit $ Label f

    Ass x e@(ETyped _ typ) -> do
      compileExp e False
      (a, t) <- lookupVar x
      r <- getPrevResult
      r' <- loadReg r
      emit $ Store (Lit typ) r' (Ptr (Lit typ)) a

    Empty -> return ()
    Incr i -> do
      (adr, t) <- lookupVar i
      adr''' <- loadReg (Reg adr, t)
      adr' <- newRegister
      let adr'' = (\(Reg x) -> x) adr'''
      if (t == (Lit Int) || t == (Ptr (Lit Int)))
        then do
          emit $ Add adr' Plus (Lit Int) (Reg adr'') (LitInt 1)
          emit $ Store (Lit Int) (Reg adr') t $ adr ----- todo probably only need to supply 1 type to add / mul
        else do
          emit $ Add adr' Plus (Lit Doub) (Reg adr'') (LitDoub 1.0)
          emit $ Store (Lit Doub) (Reg adr') t adr

    Decr i -> do
      (adr, t) <- lookupVar i
      adr''' <- loadReg (Reg adr, t)
      adr' <- newRegister
      let adr'' = (\(Reg x) -> x) adr'''
      if (t == (Lit Int) || t == (Ptr (Lit Int)))
        then do
          emit $ Add adr' Minus (Lit Int) (Reg adr'') (LitInt 1)
          emit $ Store (Lit Int) (Reg adr') t adr
        else do
          emit $ Add adr' Minus (Lit Doub) (Reg adr'') (LitDoub 1.0)
          emit $ Store (Lit Doub) (Reg adr') t adr

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
  ELitInt i  -> setPrevVal (LitInt i, Lit Int) b
  ELitTrue   -> setPrevVal (LitBool True, Lit Bool) b
  ELitFalse  -> setPrevVal (LitBool False, Lit Bool) b ---------- 1/ 0 ?
  ELitDoub d -> setPrevVal (LitDoub d, Lit Doub) b
  EString s  -> do
    adr <- newGlobalRegister
    emitGlobal $ Global adr (Lit String) (LitString s) 
    setPrevVal (Glob adr, Lit String) b

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
    setPrevVal (Reg a, t) b

  EApp x@(Ident _) es -> do
    mapM_ (\e -> compileExp e True) es
    FunHead id (FunType t ts) <- gets ((fromMaybe (error "undefined") . Map.lookup x) . sig)
    let n_args = length ts
    allArgs <- gets prevResult
    args <- mapM (\x -> loadReg x) (reverse $ take n_args allArgs)
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
        setPrevVal (Reg r, (Lit t)) b --- ptr? todo


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
    args' <- mapM (\x -> loadReg x) $ take 2 allArgs
    let [arg1, arg2] = args'
    

{-
    (arg1', arg2', changed) <- matchTypes t arg1 arg2

    let t' = if (changed)
                then Ptr ((type2LLVMType t))
                else (type2LLVMType t)
-}

    emit $ Mul r op (Lit t) arg2 arg1
    removeArgs 2
    setPrevVal (Reg r, (Lit t)) b --- ptr? todo

  ETyped (EAdd e1 op e2) t -> do
    compileExp e1 True
    compileExp e2 True
    r <- newRegister
    allArgs <- gets prevResult
    args' <- mapM (\x -> loadReg x) $ take 2 allArgs
    let [arg1, arg2] = args'
    

{-
    (arg1', arg2', changed) <- matchTypes t arg1 arg2

    let t' = if (changed)
                then Ptr ((type2LLVMType t))
                else (type2LLVMType t)
-}

    emit $ Add r op (Lit t) arg2 arg1
    removeArgs 2
    setPrevVal (Reg r, (Lit t)) b ---- todo fel, gör som i add.

  ETyped (ERel e1@(ETyped _ t1) op e2) _ -> do
    compileExp e1 True
    compileExp e2 True
    allArgs <- gets prevResult
    args' <- mapM (\x -> loadReg x) $ take 2 allArgs
    let [arg1, arg2] = args'
    
    {-
    if (arg1 /= (Reg x) && arg2 == (Reg y))
      then do
        let arg1 = (Reg lit)
        lit <- newRegister
        emit $ Alloca lit (type2LLVMType t1)
        emit $ Store (Lit t1) (Val x) (Ptr (Lit t1)) lit
      else do
        if (arg1 == (Reg x) && arg2 /= (Reg y))
        then do
          let arg2 = (Reg lit)
          lit <- newRegister
          emit $ Alloca lit (type2LLVMType t1)
          emit $ Store (Lit t1) (Val y) (Ptr (Lit t1)) lit
        else return ()
-}
{-
    (arg1', arg2', changed) <- matchTypes t1 arg1 arg2

    let t1' = if (changed)
                then Ptr ((type2LLVMType t1))
                else (type2LLVMType t1)
  -}    
    --let arg1' = arg1
    --let arg2' = arg2
    --let t1' = type2LLVMType t1
    r <- newRegister
    emit $ Compare r op (Lit t1) arg2 arg1
    removeArgs 2
    setPrevVal (Reg r, (Lit Bool)) b --------------- Lit r if not changed? todo
    -- br i1 %t2, label %lab2, label %lab1

  EAnd e1 e2 -> do

    -- e1 true?
    compileExp e1 True
    r1 <- getPrevResult
    e1_result <- loadReg r1
    t  <- newLabel
    f  <- newLabel
    result <- newRegister
    emit $ Alloca result (Lit Bool)
    emit $ BranchCond e1_result t f 

    -- e2 true?
    --fixTerminator
    emit $ Label t 
    compileExp e2 False -- ok to overwrite e1_result
    r2 <- getPrevResult
    e2_result <- loadReg r2
    t2 <- newLabel
    emit $ BranchCond e2_result t2 f
    --fixTerminator
    emit $ Label t2
    --removeArgs 1
    --setPrevVal (LitBool True) b ------------------ todo wrong
    --compileExp (ELitTrue) b
    emit $ Store (Lit Bool) (LitBool True) (Ptr (Lit Bool)) result
    end <- newLabel
    emit $ Branch end
    --fixTerminator
    emit $ Label f
    emit $ Store (Lit Bool) (LitBool False) (Ptr (Lit Bool)) result
    emit $ Branch end
    emit $ Label end
    removeArgs 1
    setPrevVal (Reg result, Ptr (Lit Bool)) b ---------------- litbool result? todo

  EOr e1 e2 -> do

    -- e1 true?
    compileExp e1 True
    r1 <- getPrevResult
    e1_result <- loadReg r1
    t  <- newLabel
    f  <- newLabel
    result <- newRegister
    emit $ Alloca result (Lit Bool)
    emit $ BranchCond e1_result t f 

    -- e2 true?
    --fixTerminator
    emit $ Label f 
    compileExp e2 False -- ok to overwrite e1_result
    r2 <- getPrevResult
    e2_result <- loadReg r2
    f2 <- newLabel
    emit $ BranchCond e2_result t f2 
    --fixTerminator
    emit $ Label f2
    --removeArgs 1
    --setPrevVal (LitBool False) b
    --fixTerminator
    emit $ Store (Lit Bool) (LitBool False) (Ptr (Lit Bool)) result
    end <- newLabel
    emit $ Branch end
    emit $ Label t
    emit $ Store (Lit Bool) (LitBool True) (Ptr (Lit Bool)) result
    emit $ Branch end
    emit $ Label end
    removeArgs 1
    setPrevVal (Reg result, Ptr (Lit Bool)) b --------- todo

  Neg (ETyped e t) -> do
    if (t == Int)
      then compileExp (ETyped (EMul e Times (ELitInt  (-1)  ) ) t) b
      else compileExp (ETyped (EMul e Times (ELitDoub (-1.0)) ) t) b
    

  Not (ETyped e Bool) -> do
    compileExp e True
    r <- getPrevResult
    e_result <- loadReg r
    t <- newLabel
    f <- newLabel
    end <- newLabel
    result <- newRegister
    emit $ Alloca result (Lit Bool)
    emit $ BranchCond e_result t f
    --fixTerminator
    emit $ Label t
    --removeArgs 1
    --setPrevVal (LitBool False) b
    emit $ Store (Lit Bool) (LitBool False) (Ptr (Lit Bool)) result
    emit $ Branch end
    emit $ Label f
    emit $ Store (Lit Bool) (LitBool True) (Ptr (Lit Bool)) result
    emit $ Branch end
    emit $ Label end
    removeArgs 1
    setPrevVal (Reg result, Ptr (Lit Bool)) b --- todo

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
  v <- gets nextReg
  modify $ \st -> st { nextReg = succ v }
  return v

-- create the next global register name
newGlobalRegister :: Compile GlobalRegister
newGlobalRegister = do
  v <- gets nextGlobal
  modify $ \st -> st { nextGlobal = succ v }
  return v

-- create new block for if while loops etc
inNewBlock :: Compile a -> Compile a
inNewBlock cont = do
  modify $ \st -> st { cxt = [] : cxt st }
  a <- cont
  modify $ \st -> st { cxt = tail $ cxt st }
  return a

-- add new variable to the state
newVar :: Ident -> Register -> LLVMType -> Compile ()
newVar x r t = do
  modify $ \st@St { cxt = (b : bs) } -> st { cxt = ((x, r, t) : b) : bs }
  --updateLimitLocals

{-
-- get type and register for a variable
lookupVar :: Ident -> Compile (Register, LLVMType)
lookupVar x = gets ((loop . concat) . cxt)
 where
  loop [] = error $ "unbound variable " ++ printTree x
  loop ((y, t) : bs) | x == y    = (R $ length bs, t)
                     | otherwise = loop bs
                     -}

-- get type and register for a variable
lookupVar :: Ident -> Compile (Register, LLVMType)
lookupVar id = do 
  c <- gets cxt
  return (fromJust $ cxtContains id c)
  where
    cxtContains :: Ident -> [[(Ident, Register, LLVMType)]] -> Maybe (Register, LLVMType)
    cxtContains id [] = Nothing
    cxtContains id (b:bs) = do 
      let firstSearch = contains id b 
      if (isNothing firstSearch )
        then cxtContains id bs
        else firstSearch
      where
        contains :: Ident -> [(Ident, Register, LLVMType)] -> Maybe (Register, LLVMType)
        contains id [] = Nothing
        contains id ((id', r, t):vs) = if (id == id')
          then Just (r, t)
          else contains id vs


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

-- add global constant string to output
emitGlobal :: Code -> Compile ()
emitGlobal c = modify $ \st@St { globalOut = cs } -> st { globalOut = c : cs }

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

-- convert literal operand to register if the other operand is register 
matchTypes :: Type -> Value -> Value -> Compile (Value, Value, Bool)
matchTypes _ (Reg r1) (Reg r2) = return ((Reg r1), (Reg r2), False)
matchTypes _ (LitInt r1) (LitInt r2) = return ((LitInt r1), (LitInt r2), False)
matchTypes _ (LitDoub r1) (LitDoub r2) = return ((LitDoub r1), (LitDoub r2), False)
matchTypes _ (LitBool r1) (LitBool r2) = return ((LitBool r1), (LitBool r2), False)
matchTypes t (Reg r1) r2 = do
  r2' <- newRegister
  emit $ Alloca r2' (type2LLVMType t)
  emit $ Store (Lit t) r2 (Ptr (Lit t)) r2'
  return (Reg r1, Reg r2', True)
matchTypes t r1 (Reg r2) = do
  (_, r1', _) <- matchTypes t (Reg r2) r1
  return (r1',(Reg r2), True)



-- load register
loadReg :: (Value, LLVMType) -> Compile Value ---------- todo, var = reg ptr, temp = reg lit
loadReg (Reg r, Ptr (Lit t)) = do 
  r' <- newRegister
  --let (Reg r'') = r
  emit $ Load r' (Lit t) (Ptr (Lit t)) r
  return (Reg r')
loadReg (r, Lit t) = return r