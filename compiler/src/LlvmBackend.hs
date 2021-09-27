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
  :: Prog -- ^ Type-annotated program.
  -> String  -- ^ Generated jasmin source file content.
compile (Program defs) = do
  -- all code
  let code = compileDefs (initSt sig0) defs
  -- strings
  let globs = intercalate "" (map snd code)
  -- functions
  let funcs = unlines (map fst code)
  -- built in functions
  let decl = unlines ["declare void @printInt(i32)", "declare void @printDouble(double)", "declare void @printString(i8*)", "declare i32 @readInt()", "declare double @readDouble()", "declare i8* @calloc(i32, i32)"]
  -- append everything and remove the last 2 extra newlines
  reverse $ drop 2 $ reverse $ decl ++ globs ++ "\n" ++ funcs
  where
    -- initial state with only builtin
    sig0 = Map.fromList $ builtin ++ map sigEntry defs
    sigEntry def@(FnDef _ f@(Ident x) _ _) =
      (f, ) $ FunHead (Ident x) $ funType def







--------------------------------------- Types

type Sig = Map Ident FunHead -- functions
type Cxt = [Block]           -- variables
type Block = [(Ident, Register, LLVMType)]
type Output = [Code]         -- llvm code
type Compile = State St      -- state



----------------------- newtypes

newtype Label = L {theLabel :: Int}
  deriving (Eq, Enum, Show)

newtype Register = R {theRegister :: Int}
  deriving (Eq, Enum, Show, Ord)

-- register for strings
newtype GlobalRegister = G Int
  deriving (Eq, Enum, Show)

-- index for getelementptr
newtype Index = I [Value]
  deriving (Show)



---------------------- data

data St = St
  { sig          :: Sig
  , cxt          :: Cxt
  , nextLabel    :: Label
  , nextReg      :: Register
  , output       :: Output
  , prevResult   :: [(Value, LLVMType)]
  , globalOut    :: Output
  , globals      :: Block
  , nextGlobal   :: GlobalRegister
  , params       :: [Register]
  }

-- initial state
initSt :: Sig -> St
initSt s = St { sig          = s
              , cxt          = [[]]
              , nextLabel    = L 0
              , nextReg      = R 0
              , output       = []
              , prevResult   = []
              , globalOut    = []
              , globals      = []
              , nextGlobal   = G 0
              , params       = []
              }

builtin :: [(Ident, FunHead)]
builtin =
  [ (Ident "printInt",     FunHead (Ident "printInt"   ) $ FunType Void  [Int])
  , (Ident "printDouble",  FunHead (Ident "printDouble") $ FunType Void  [Doub])
  , (Ident "printString",  FunHead (Ident "printString") $ FunType Void  [String])
  , (Ident "readInt"   ,   FunHead (Ident "readInt"    ) $ FunType Int   [])
  , (Ident "readDouble",   FunHead (Ident "readDouble" ) $ FunType Doub  [])
  , (Ident "calloc"    ,   FunHead (Ident "calloc"     ) $ FunType Tuple [Int, Int])
  ]


data LLVMType = Lit Type | Ptr LLVMType | Struct Type
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

-- value or register
data Value = LitInt Integer | LitDoub Double | LitBool Bool | LitString String | Reg Register | Glob GlobalRegister
  deriving(Show)

data Operator = Mo MulOp | Ao AddOp | Ro RelOp
  deriving(Show)

data Code
  = Store LLVMType Value Register
  | Load Register LLVMType Register
  | Return LLVMType Value
  | ReturnVoid
  | Call Register LLVMType Ident Arguments
  | CallVoid LLVMType Ident Arguments
  | Label Label
  | Add Register AddOp LLVMType Value Value
  | Mul Register MulOp LLVMType Value Value
  | Compare Register RelOp LLVMType Value Value
  | Alloca Register LLVMType
  | Branch Label
  | BranchCond Value Label Label
  | Global GlobalRegister LLVMType Value
  | GetElementPointer  Register String GlobalRegister Index -- get ptr for string
  | GetElementPointerL Register Type Register Index         -- get ptr for arrays 
  | Cast Register LLVMType Register LLVMType                -- bit cast
  | Comment String
    deriving (Show)






----------------------------------------------------------------------- interract with state

-- update or add argument from previous calculation
setPrevVal :: (Value, LLVMType) -> Bool -> Compile ()
setPrevVal v False = do 
  vs <- gets prevResult
  if (length vs == 0)
    then modify $ \st@St { prevResult = vs } -> st { prevResult = [v] }
    else modify $ \st@St { prevResult = vs } -> st { prevResult = v : (tail vs) }
setPrevVal v True  = modify $ \st@St { prevResult = vs } -> st { prevResult = v : vs }


-- get value or register from previous statement
getPrevResult :: Compile (Value, LLVMType)
getPrevResult = do
  allArgs <- gets prevResult
  return $ head allArgs


-- dereference pointer types
loadReg :: (Value, LLVMType) -> Compile Value
-- lists should always have 1 ptr
loadReg (    r,      Ptr (Lit (List a))) = return r
-- unnest list with 2 pointers
loadReg (    Reg r, Ptr (Ptr (Lit (List a)))) = do
  r' <- newRegister
  emit $ Load r' (Ptr (Lit (List a))) r
  return (Reg r')
-- cant unnest 
loadReg (r, Lit t) = return r
-- unnest non-list values
loadReg (Reg r, Ptr (Lit t)) = do 
  r' <- newRegister
  emit $ Load r' (Lit t) r
  return (Reg r')


-- remove arguments / previous results
removeArgs :: Int -> Compile()
removeArgs n = modify $ \st@St { prevResult = vs } -> st { prevResult = drop n vs }


-- create new block for if while loops etc
inNewBlock :: Compile a -> Compile a
inNewBlock cont = do
  modify $ \st -> st { cxt = [] : cxt st }
  a <- cont
  modify $ \st -> st { cxt = tail $ cxt st }
  return a


-- create the next label name
newLabel :: Compile Label
newLabel = do
  l <- gets nextLabel
  modify $ \st -> st { nextLabel = succ l }
  return l


-- create the next global register name
newGlobalRegister :: Compile GlobalRegister
newGlobalRegister = do
  v <- gets nextGlobal
  modify $ \st -> st { nextGlobal = succ v }
  return v


-- create the next register name
newRegister :: Compile Register
newRegister = do
  v <- gets nextReg
  modify $ \st -> st { nextReg = succ v }
  return v


-- add new variable to the state
newVar :: Ident -> Register -> LLVMType -> Compile ()
newVar x r t = modify $ \st@St { cxt = (b : bs) } -> st { cxt = ((x, r, t) : b) : bs }


-- get type and register for a variable
lookupVar :: Ident -> Compile (Register, LLVMType)
lookupVar id = do 
  c <- gets cxt
  let r = cxtContains id c
  if (isNothing r)
    then error (show id)
    else return $ fromJust r
  where
    -- context contains var?
    cxtContains :: Ident -> [[(Ident, Register, LLVMType)]] -> Maybe (Register, LLVMType)
    cxtContains id []     = Nothing
    cxtContains id (b:bs) = do 
      let firstSearch = contains id b 
      if (isNothing firstSearch )
        then cxtContains id bs
        else firstSearch
      where
        -- block containts var?
        contains :: Ident -> [(Ident, Register, LLVMType)] -> Maybe (Register, LLVMType)
        contains id []               = Nothing
        contains id ((id', r, t):vs) = if (id == id')
          then Just (r, t)
          else contains id vs






--------------------------------------------------------------------------- convert code to string



------------------------------------------------------ helpers


-- | Indent non-empty, non label lines.
indent :: String -> String
indent s | null s        = s
indent s | last s == ':' = s
indent s | head s == '@' = s
indent s                 = "\t" ++ s

impossible :: Code -> String
impossible a = error $ "impossible code " ++ toLLVM a


-- get the amount of space each type needs
bytes :: Type -> Int
bytes Int  = 4
bytes Bool = 1
bytes Doub = 4
bytes _    = error "bytes unimplemented"


-- wrap lists with a pointer, do nothing to other values
pointerList :: Type -> LLVMType
pointerList (List t) = Ptr (Lit (List t))
pointerList       t  =      Lit       t

-- get the register of a value
val2Reg :: Value -> Register
val2Reg (Reg r) = r

-- get the inner type of a list
typeOfArray :: LLVMType -> Type
typeOfArray      (Ptr (Lit (List t)))  = t
typeOfArray (Ptr (Ptr (Lit (List t)))) = t


-------------------------------------------------- convert code


class ToLLVM a where
    toLLVM :: a -> String

instance ToLLVM LLVMType where
  toLLVM = \case
    Ptr ptr -> toLLVM ptr ++ "*" 
    Lit lit -> toLLVM lit

instance ToLLVM Type where
  toLLVM t = case t of
    Int    -> "i32"
    Void   -> "void"
    Bool   -> "i1"
    Doub   -> "double"
    String -> "i8*"
    Tuple  -> "i8*"
    List t -> listType t

-- add proper prefix to mulop
prefixMulOp :: LLVMType -> MulOp -> String
prefixMulOp (Lit Int) Times  = "mul"
prefixMulOp (Lit Doub) Times = "fmul"
prefixMulOp (Lit Doub) Div   = "fdiv"
prefixMulOp (Lit Int) Div    = "sdiv"
prefixMulOp (Lit Int) Mod    = "srem"


instance ToLLVM AddOp where
  toLLVM op = case op of
    Plus  -> "add"
    Minus -> "sub"

-- add proper prefix to relop
prefixRelOp :: LLVMType -> RelOp -> String
prefixRelOp (Lit Doub) op                         = "o" ++ toLLVM op
prefixRelOp (Lit Int)  op | op == EQU || op == NE =        toLLVM op
prefixRelOp (Lit Int)  op                         = "s" ++ toLLVM op
prefixRelOp (Lit Bool) op                         = prefixRelOp (Lit Int) op

instance ToLLVM RelOp where
  toLLVM op = case op of
    LTH   -> "lt"
    GTH   -> "gt"
    LE    -> "le"
    GE    -> "ge"
    EQU   -> "eq"
    NE    -> "ne"

instance ToLLVM Index where
  toLLVM (I (i:[])) = toLLVM (Lit Int) ++ " " ++ toLLVM i
  toLLVM (I (i:is)) = toLLVM (Lit Int) ++ " " ++ toLLVM i ++ ", " ++ toLLVM (I is)


instance ToLLVM FunHead where
  toLLVM (FunHead (Ident f) (FunType (List t) ts)) = error (show t)
  toLLVM (FunHead (Ident f) (FunType t ts)) = "define " ++ toLLVM t ++ " @" ++ f ++ "(" ++ ( reverse ( drop 2 ( reverse (((\t -> t ++ ", ") . toLLVM) =<< ts)))) ++ ")"

instance ToLLVM Label where
  toLLVM (L l) = "lab" ++ show l

instance ToLLVM Value where
  toLLVM = \case
    LitInt i      -> show i
    LitDoub d     -> show d
    LitBool True  -> "1" 
    LitBool False -> "0" 
    LitString s   -> error $ "can only print adress to string, not string directly"
    Reg r         -> toLLVM r
    Glob g        -> toLLVM g 
    
instance ToLLVM Register where
  toLLVM (R r) = "%r" ++ show r

instance ToLLVM GlobalRegister where
  toLLVM (G g) = "@g" ++ show g

newtype Arguments = Args [(LLVMType, Value)]
  deriving(Show)

instance ToLLVM Arguments where
  toLLVM (Args [])          = ""
  toLLVM (Args [(t, v)])    = toLLVM t ++ " " ++ toLLVM v
  toLLVM (Args ((t, v):as)) = toLLVM t ++ " " ++ toLLVM v ++ ", " ++ toLLVM (Args as)

-- print string in array form
stringType :: String -> String
stringType s = "[" ++ show ( (length s) + 1) ++ " x i8]"

-- print type of getelementptr argument
listType :: Type -> String
listType (List t) = error (show t)
listType t    =  "{i32, [ 0 x " ++ toLLVM t ++ " ]}"

instance ToLLVM Code where
  toLLVM = \case
    Store t from to                        -> "store " ++ toLLVM t ++ " "  ++ toLLVM from ++ " , " ++ toLLVM (Ptr t) ++ " " ++ toLLVM to
    Load adr t reg                         -> toLLVM adr ++ " = load "     ++ toLLVM t    ++ " , " ++ toLLVM (Ptr t) ++ " " ++ toLLVM reg
    Return t v                             -> "ret "   ++ toLLVM t ++ " "  ++ toLLVM v
    ReturnVoid                             -> "ret void"
    Call adr t (Ident f) args              -> toLLVM adr ++ " = call " ++ toLLVM t ++ " @" ++ f ++ "(" ++ toLLVM args ++ ")"
    CallVoid t (Ident f) args              -> "call " ++ toLLVM t ++ " @" ++ f ++ "(" ++ toLLVM args ++ ")"
    Label l                                -> toLLVM l ++ ":"
    Compare adr op t v1 v2 | t == Lit Doub -> toLLVM adr ++ " = fcmp "   ++ prefixRelOp t op ++ " " ++ toLLVM t ++ " " ++ toLLVM v1 ++ ", " ++ toLLVM v2
                           | otherwise     -> toLLVM adr ++ " = icmp "   ++ prefixRelOp t op ++ " " ++ toLLVM t ++ " " ++ toLLVM v1 ++ ", " ++ toLLVM v2
    Add adr op t v1 v2 | t == Lit Int      -> toLLVM adr ++ " = "        ++ toLLVM op        ++ " " ++ toLLVM t ++ " " ++ toLLVM v1 ++ ", " ++ toLLVM v2
                       | t == Lit Doub     -> toLLVM adr ++ " = " ++ "f" ++ toLLVM op        ++ " " ++ toLLVM t ++ " " ++ toLLVM v1 ++ ", " ++ toLLVM v2
    Mul adr op t v1 v2                     -> toLLVM adr ++ " = " ++ (prefixMulOp t op)      ++ " " ++ toLLVM t ++ " " ++ toLLVM v1 ++ ", " ++ toLLVM v2
    Alloca adr t                           -> toLLVM adr ++ " = alloca " ++ toLLVM t
    Branch lb                              -> "br label %" ++ toLLVM lb
    BranchCond c lb1 lb2                   -> "br i1 " ++ toLLVM c ++ ", label %" ++ toLLVM lb1 ++ ", label %" ++ toLLVM lb2
    Global adr t (LitString s)             -> toLLVM adr ++ " = internal constant " ++ stringType s ++  " c\"" ++ s ++ "\\00\""
    GetElementPointer r' s r i             -> toLLVM r' ++ " = getelementptr " ++ stringType s ++ ", " ++ stringType s ++ "* " ++ toLLVM r ++ ", " ++ toLLVM i
    GetElementPointerL r' t r i            -> toLLVM r' ++ " = getelementptr " ++ listType t ++ ", " ++ listType t ++ "* " ++ toLLVM r ++ ", " ++ toLLVM i
    Cast r1 t1 r2 t2                       -> toLLVM r1 ++ " = bitcast " ++ toLLVM t1 ++ " " ++ toLLVM r2 ++ " to " ++ toLLVM t2
    Comment ""                             -> ""
    Comment s                              -> "; " ++ s








---------------------------------------------------------------------------------- emit code



-- add llvm code line to output
emit :: Code -> Compile ()
emit (Store (Lit Void) _ _) = return ()
emit (Load  _ (Lit Void) _) = return ()
emit c                      = modify $ \st@St { output = cs } -> st { output = c : cs }

-- add global constant string to output
emitGlobal :: Code -> Compile ()
emitGlobal c = modify $ \st@St { globalOut = cs } -> st { globalOut = c : cs }

-- string comment
comment :: String -> Compile ()
comment = emit . Comment

-- blank line
blank :: Compile ()
blank = comment ""









------------------------------------------------------------------------------- compile functions



-- compile functions
compileDefs :: St -> [TopDef] -> [(String, String)]
compileDefs st [] = []
compileDefs st (d:ds) = do
  let (s1,s2,st') = compileDef st d
  let sss         = compileDefs st' ds
  (s1,s2):sss


-- compile function
compileDef :: St -> TopDef -> (String, String, St)
compileDef st def@(FnDef t (Ident f) args (Block ss)) = do
  -- pair each argument with a parameter name
  let args' = Args $ zip (map (\(Argument t id) -> pointerList t) args) (map (\x -> Reg x) (params st')) -- kinda ugly to have have params here, just do it in compilefun todo
  -- print the function header
  -- if list, then print as pointer
  let t2 = pointerList t 
  let func  = intercalate "" [ "define " ++ toLLVM t2 ++ " @" ++ f ++ "(" ++ toLLVM args' ++ " ) {\n", "entry:\n", unlines $ map (indent . toLLVM) $ reverse $ (output st'), "}\n"]
  -- print the string constants
  let glob  = unlines $ map toLLVM $ reverse (globalOut st')
  (func, glob, st')
  -- compile the function
  where st' = execState (compileFun (Ident f) t args ss) st


-- compile function helper
compileFun :: Ident -> Type -> [Arg] -> [Stmt] -> Compile ()
compileFun (Ident f) t0 args ss = do
  -- reset code output
  modify $ \st -> st { output = []}
  modify $ \st -> st { globalOut = []}
  -- make parameter registers
  regs <- mapM (\(Argument t' x) -> newRegister) args
  let arg_reg = zip args regs
  -- make a new variable and alloc memory for each parameter:
  mapM_ (\(Argument t' x, r) -> do
                                  r' <- newRegister
                                  newVar x r' (Ptr (pointerList t'))--(Ptr (Lit t'))
                                  emit $ Alloca r' (pointerList t') -- (Lit t')
                                  emit $ Store     (pointerList t') (Reg r) r'--(Lit t') (Reg r) r'
                                ) arg_reg
  -- store current parameters
  modify $ \st -> st { params = regs}
  compileStms ss
  -- add "ret void" if no return statement at the end
  if (t0 == Void)
    then do 
      prevStm <- gets output
      if (length prevStm /= 0 && (head $ words $ toLLVM $ head prevStm) == "ret")
        then return ()
        else emit ReturnVoid
    else return ()











---------------------------------------------------------------------------------- compile statements



-- help function for compiling variable declaration
compileDecl :: Type -> Item -> Compile ()
compileDecl t (Init id (ETyped e _)) = do
  -- wrap type in pointer if its a list
  (typ, isList) <- case t of
                    List t' -> return ((Ptr (Lit t)), True)
                    t'       -> return      (Lit t , False)
  -- compile expression and make new variable
  compileExp (ETyped e t) False
  r <- newRegister
  newVar id r (Ptr typ)
  emit $ Alloca r typ
  (p, pp)  <- getPrevResult
  -- dereference pointer if needed
  source <- if (isList)
              then return p
              else loadReg (p,pp)
  -- store in new variable
  emit $ Store typ source r




compileDecl t (NoInit id) = do
  -- wrap type in pointer if its a list
  let typ = pointerList t
  -- just create new variable
  r <- newRegister
  newVar id r (Ptr typ)
  emit $ Alloca r typ



-- compile list of statements 
-- Bool result: do these statements guarantee a return statement?
compileStms :: [Stmt] -> Compile Bool
compileStms []        = return False
compileStms (s : ss') = do
  returns <- compileStm s
  -- stop if you found a statement that guaranteed a return statement
  if (returns)
    then return True
    else compileStms ss'


-- compile statement
-- Bool: does this statement guarantee a return statement?
compileStm :: Stmt -> Compile Bool
compileStm (Retting s0 ret) = do
  case s0 of


    Ret e@(ETyped _ t) -> do
      compileExp e False
      r  <- getPrevResult
      r' <- loadReg r
      let t' = pointerList t
      emit $ Return t' r'
      return True


    VRet -> do
      emit $ ReturnVoid
      return True


    Decl t ds -> do
      mapM_ (compileDecl t) ds
      return False


    SExp e@(ETyped _ t) -> do
      compileExp e False
      return False


    While e@(ETyped e' typ) s -> do
      -- if guaranteed return, only compile inner stmt
      -- otherwise, do as usual
      if (ret > 0)
        then do 
          inNewBlock $ compileStm s
          return True
        else do
          start <- newLabel
          t     <- newLabel
          f     <- newLabel
          emit $ Branch start
          -- evaluate expression
          emit $ Label start
          compileExp e False
          r     <- getPrevResult
          r'    <- loadReg r
          emit $ BranchCond r' t f
          -- inside loop
          emit $ Label t
          inNewBlock $ compileStm s
          emit $ Branch start
          -- after loop
          emit $ Label f
          return False


    BStmt (Block ss) -> do
      inNewBlock $ compileStms ss


    CondElse e@(ETyped e' typ) s1 s2 -> do
      -- if return guaranteed, then only compile s1 or s2
      if (ret > 0)
        then case e' of
          ELitTrue  -> do
            inNewBlock $ compileStm s1
            return True
          ELitFalse -> do
            inNewBlock $ compileStm s2
            return True
          _         -> standard True
        else standard False
        where
          standard returns = do
            -- evaluate expression
            compileExp e False
            t   <- newLabel
            f   <- newLabel
            r   <- getPrevResult
            r'  <- loadReg r
            emit $ BranchCond r' t f
            -- statement 1
            emit $ Label t
            inNewBlock $ compileStm s1
            -- if doesnt guarantee return, then include the end label
            if (not returns)
              then do 
                end <- newLabel
                emit $ Branch end
                -- statement 2
                emit $ Label f
                inNewBlock $ compileStm s2
                -- end 
                emit $ Branch end
                emit $ Label end
                return False
              else do
                -- statement 2
                emit $ Label f
                inNewBlock $ compileStm s2         
                return True


    Cond e@(ETyped _ typ) s -> do
      -- if guarantees return, then only compile the statement
      if (ret > 0)
        then do
          inNewBlock $ compileStm s
          return True
        else do
          t  <- newLabel
          f  <- newLabel
          -- check expression
          compileExp e False
          r  <- getPrevResult
          r' <- loadReg r
          emit $ BranchCond r' t f
          -- compile statement
          emit $ Label t
          inNewBlock $ compileStm s
          emit $ Branch f
          -- end
          emit $ Label f
          return False


    Ass x e@(ETyped _ typ) -> do
      compileExp e False
      (a, t) <- lookupVar x
      r      <- getPrevResult
      r'     <- loadReg r
      emit $ Store (pointerList typ) r' a
      return False


    Incr i -> incDecr i Plus
    Decr i -> incDecr i Minus


    
    SFor typ id e s -> do
      -- labels
      start <- newLabel
      t     <- newLabel
      f     <- newLabel
      -- create variable for counting
      count  <- newRegister
      emit $ Alloca count (Lit Int)
      emit $ Store (Lit Int) (LitInt 0) count
      emit $ Branch start

      -- check condition
      emit $ Label start
      -- load count
      count' <- newRegister
      emit $ Load count' (Lit Int) count
      -- load length
      compileExp (ELen e) False
      len <- getPrevResult
      len' <- loadReg len
      -- count < length ?
      cmp <- newRegister
      emit $ Compare cmp LTH (Lit Int) (Reg count') len'
      emit $ BranchCond (Reg cmp) t f

      -- inside loop
      emit $ Label t
      -- add empy block for varaibles
      modify $ \st -> st { cxt = [] : cxt st }
      -- create temp variable for element
      elem <- newRegister
      newVar id elem (Ptr (Lit typ))
      emit $ Alloca elem (Lit typ)
      -- load array
      compileExp e False
      (e', t0) <- getPrevResult
      e0 <- loadReg (e', t0)
      -- get the register
      let e'' = val2Reg e0
      -- calculate index
      r <- newRegister
      let index = (I [LitInt 0, LitInt 1, (Reg count')]) 
      -- get the element
      emit $ GetElementPointerL r typ e'' index
      r' <- newRegister
      emit $ Load r' (Lit typ) r
      -- store element in temp var
      emit $ Store (Lit typ) (Reg r') elem

      -- compile the actual loop statements
      compileStm s
      -- remove outer most block
      modify $ \st -> st { cxt = tail $ cxt st }
      -- next index
      count'' <- newRegister
      emit $ Add count'' Plus (Lit Int) (Reg count') (LitInt 1)
      emit $ Store (Lit Int) (Reg count'') count
      -- go back to condition
      emit $ Branch start
      -- after loop
      emit $ Label f
      return False





    AssIndex id e2 new -> do
      -- get array
      (e1, t) <- lookupVar id
      -- get the type of the list
      let t' = typeOfArray t
      -- get element address
      compileExp (EElem (EVar id) e2) False
      (adr, t0) <- getPrevResult
      -- get the address
      let adr' = val2Reg adr
      -- compile new value
      compileExp new False
      new' <- getPrevResult
      new'' <- loadReg new'
      -- store new value
      emit $ Store (Lit t') new'' adr'
      return False
      


    Empty -> return False


    s -> error $ "not implemented compileStm " ++ printTree s



-- helper for incrementing / decrementing statement
incDecr :: Ident -> AddOp -> Compile Bool
incDecr i op = do
  (adr, t) <- lookupVar i
  adr'''   <- loadReg (Reg adr, t)
  adr'     <- newRegister
  -- convert value to register
  let adr'' = (\(Reg x) -> x) adr'''
  let v = if (t == (Lit Int) || t == (Ptr (Lit Int)))
            then (LitInt 1)
            else (LitDoub 1.0)
  emit $ Add adr' op (Lit Int) (Reg adr'') v
  emit $ Store (Lit Int) (Reg adr') adr
  return False






---------------------------------------------------------------------------------- compile expression



-- helper: emit add / mul / rel expression.
emitBinaryOp :: Type -> Operator -> Expr -> Expr -> Bool -> Compile ()
emitBinaryOp t op' e1 e2 b = do
  -- compile arg 1
  compileExp e1 True
  r1 <- getPrevResult
  arg1 <- loadReg r1
  -- compile arg 2
  compileExp e2 True
  r2 <- getPrevResult
  arg2 <- loadReg r2
  r <- newRegister
  -- compile and remove arguments
  case op' of
    Ao op -> emit $ Add     r op (Lit t) arg1 arg2
    Mo op -> emit $ Mul     r op (Lit t) arg1 arg2
    Ro op -> emit $ Compare r op (Lit t) arg1 arg2
  removeArgs 2
  setPrevVal (Reg r, (Lit t)) b




-- compile expression
-- Bool: this expr is an argument to a function, 
-- the bool is needed for knowing whether or not to save or override the previous result in the state 
compileExp :: Expr -> Bool -> Compile ()
compileExp e0 b = case e0 of


  ELitInt i  -> setPrevVal (LitInt i     , Lit Int)  b
  ELitDoub d -> setPrevVal (LitDoub d    , Lit Doub) b
  ELitTrue   -> setPrevVal (LitBool True , Lit Bool) b
  ELitFalse  -> setPrevVal (LitBool False, Lit Bool) b


  EString s  -> do
    adr  <- newGlobalRegister
    emitGlobal $ Global adr (Lit String) (LitString s)
    adr' <- newRegister
    emit $ GetElementPointer adr' s adr $ I [LitInt 0, LitInt 0]
    setPrevVal (Reg adr', Lit String) b


  EVar x -> do
    (a, t) <- lookupVar x
    setPrevVal (Reg a, t) b


  EApp x@(Ident _) es -> do
    -- get function header
    FunHead id (FunType t ts) <- gets ((fromMaybe (error "undefined") . Map.lookup x) . sig)
    let n_args = length ts
    -- compile arguments and and make sure they dont override each other
    mapM_ (\e -> compileExp e True) es
    allArgs <- gets prevResult
    args    <- mapM (\x -> loadReg x) (reverse $ take n_args allArgs)
    -- translate types to LLVMType
    let ts' = map (\x -> (pointerList x)) ts
    let args' = zip ts' args
    -- if void function, then no need to save the result
    if (t == Void)
      then do
        emit $ CallVoid (pointerList t) id (Args args')
        removeArgs n_args
      else do
        r <- newRegister
        emit $ Call r (pointerList t) id (Args args')
        removeArgs n_args
        setPrevVal (Reg r, (Lit t)) b


  ETyped (EAdd e1 op e2) t              -> emitBinaryOp t (Ao op) e1 e2 b
  ETyped (EMul e1 op e2) t              -> emitBinaryOp t (Mo op) e1 e2 b
  ETyped (ERel e1@(ETyped _ t) op e2) _ -> emitBinaryOp t (Ro op) e1 e2 b


  EAnd e1 e2 -> do
    -- e1 true?
    compileExp e1 True
    r1        <- getPrevResult
    e1_result <- loadReg r1
    t         <- newLabel
    f         <- newLabel
    -- create result variable
    result    <- newRegister
    emit $ Alloca result (Lit Bool)
    -- if e1 true, then compile e2, otherwise skip (lazy eval)
    emit $ BranchCond e1_result t f 

    -- e2 true?
    emit $ Label t 
    compileExp e2 False -- it is ok to overwrite e1_result
    r2        <- getPrevResult
    e2_result <- loadReg r2
    t2        <- newLabel
    -- if e2 true, emit true, otherwise false
    emit $ BranchCond e2_result t2 f

    -- emit true
    emit $ Label t2
    emit $ Store (Lit Bool) (LitBool True) result
    end <- newLabel
    emit $ Branch end

    -- emit false
    emit $ Label f
    emit $ Store (Lit Bool) (LitBool False) result
    emit $ Branch end

    -- end
    emit $ Label end
    removeArgs 1
    setPrevVal (Reg result, Ptr (Lit Bool)) b


  EOr e1 e2 -> do
    -- e1 true?
    compileExp e1 True
    r1        <- getPrevResult
    e1_result <- loadReg r1
    t         <- newLabel
    f         <- newLabel
    result    <- newRegister
    -- create result variable
    emit $ Alloca result (Lit Bool)
    -- if e1 true, then emit true, otherwise check e2
    emit $ BranchCond e1_result t f 

    -- e2 true?
    emit $ Label f 
    compileExp e2 False -- ok to overwrite e1_result
    r2        <- getPrevResult
    e2_result <- loadReg r2
    f2        <- newLabel
    -- if e2 true, then emit true, otherwise emit false
    emit $ BranchCond e2_result t f2 

    -- both were false
    emit $ Label f2
    emit $ Store (Lit Bool) (LitBool False) result
    end <- newLabel
    emit $ Branch end
    emit $ Label t
    emit $ Store (Lit Bool) (LitBool True) result
    emit $ Branch end

    -- end
    emit $ Label end
    removeArgs 1
    setPrevVal (Reg result, Ptr (Lit Bool)) b


  Neg (ETyped e t) -> do
    if (t == Int)
      then compileExp (ETyped (EMul e Times (ELitInt  (-1)  ) ) t) b
      else compileExp (ETyped (EMul e Times (ELitDoub (-1.0)) ) t) b
    

  Not (ETyped e Bool) -> do
    compileExp e True
    r        <- getPrevResult
    e_result <- loadReg r
    t        <- newLabel
    f        <- newLabel
    end      <- newLabel
    -- create result variable
    result   <- newRegister
    emit $ Alloca result (Lit Bool)
    -- if true, emit false and vice versa
    emit $ BranchCond e_result t f

    -- e was true
    emit $ Label t
    emit $ Store (Lit Bool) (LitBool False) result
    emit $ Branch end

    -- e was false
    emit $ Label f
    emit $ Store (Lit Bool) (LitBool True) result
    emit $ Branch end

    -- end
    emit $ Label end
    removeArgs 1
    setPrevVal (Reg result, Ptr (Lit Bool)) b

  EList t e -> do
    -- size of each element
    let size' = ELitInt (fromIntegral $ bytes t)
    -- extra array length = length + 1 (for .length variable), do +4 for bools since they are only 1 byte long
    offset <- case t of 
                Bool -> return 4
                _ -> return 1
    -- calculate final length
    let len = case t of 
                -- for doub: (length*2 + 1)*4
                Doub -> (ETyped (EAdd (ETyped (EMul e Times (ETyped (ELitInt 2) Int)) Int) Plus (ETyped (ELitInt offset) Int)) Int)
                -- for int:  (length + 1)*4 
                -- for bool: (length + 4)*1 
                _    -> (ETyped (EAdd e Plus (ETyped (ELitInt offset) Int)) Int)
    -- call calloc
    let args      = [len, size']    
    let call = (ETyped (EApp (Ident "calloc") args) Tuple)
    compileExp call b 
    (c', ct') <- getPrevResult
    -- get the location of the heap memory
    let c''' = val2Reg c'
    -- cast i8* to {i32, [...]}
    c'' <- newRegister
    emit $ Cast c'' ct' c''' (Ptr $ Lit (List t))
    -- set length variable
    compileExp e b 
    e' <- getPrevResult
    e'' <- loadReg e'
    l <- newRegister
    emit $ GetElementPointerL l t c'' (I [LitInt 0, LitInt 0])
    emit $ Store (Lit Int) e'' l
    -- done
    setPrevVal (Reg c'', Ptr (Lit (List t))) b 



  EElem e1 e2 -> do
    -- compile array
    compileExp e1 b 
    (e1', t) <- getPrevResult
    e11 <- loadReg (e1', t)
    -- get the address of the array
    let e1''' = val2Reg e11
    -- get the type
    let t'' = typeOfArray t
    -- compile index
    compileExp (ETyped e2 Int) b 
    e2' <- getPrevResult
    e2'' <- loadReg e2'
    -- get elem
    r <- newRegister
    let index = (I [LitInt 0, LitInt 1, e2''])
    emit $ GetElementPointerL r t'' e1''' index
    setPrevVal (Reg r, Ptr (Lit t'')) b



  ELen e1 -> do
    -- compile array
    compileExp e1 b
    (e1', t) <- getPrevResult
    res <- loadReg (e1', t)
    -- get the address of the array
    let e1'' = val2Reg res
    -- get the type of the array
    let t'' = typeOfArray t
    -- get the length
    r <- newRegister
    emit $ GetElementPointerL r t'' e1'' (I [LitInt 0, LitInt 0])
    setPrevVal (Reg r, Ptr (Lit Int)) b


  ETyped e _ -> compileExp e b


  e          -> error $ "not implemented compileexp " ++ show e

