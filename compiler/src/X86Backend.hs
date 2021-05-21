-- Optional: turn on warnings.
{-# OPTIONS_GHC -Wall #-}
{-#LANGUAGE TupleSections #-}
{-#LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Compiler for C--, producing symbolic JVM assembler.

module X86Backend where

import           Javalette.Abs
import           Javalette.Print

import           Control.Monad
import           Control.Monad.State

import           Data.Maybe
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.List
import           Data.Char


-- | Entry point.

compile
  :: Prog -- ^ Type-annotated program.
  -> String  -- ^ Generated jasmin source file content.
compile (Program defs) = do
  -- built in functions (todo not hardcode)
  let decl = unlines ["extern printInt", "extern printDouble", "extern printString", "extern readInt", "extern readDouble"]
  -- all code
  let code = compileDefs (initSt sig0) defs
  -- strings
  let globs = intercalate "" (map snd code)
  -- string header
  let dat = if (globs == "")
              then ""
              else "segment .data\n"
  -- text header
  let text = unlines ["segment .text", "\tglobal main"]
  -- functions
  let funcs = unlines (map fst code)
  -- append everything and remove the last 2 extra newlines
  reverse $ drop 2 $ reverse $ decl ++ "\n" ++  dat ++ globs ++ "\n" ++ text ++ "\n" ++ funcs
  where

    -- initial state with only builtin
    sig0 = Map.fromList $ builtin ++ map sigEntry defs
    sigEntry def@(FnDef _ f@(Ident x) _ _) =
      (f, ) $ FunHead (Ident x) $ funType def
    







--------------------------------------- Types

type Sig = Map Ident FunHead -- functions
type Cxt = [Block]           -- variables
type Block = [(Ident, Value, LLVMType)]
type Output = [Code]         -- llvm code
type Compile = State St      -- state



----------------------- newtypes

newtype Label = L {theLabel :: Int}
  deriving (Eq, Enum, Show)

newtype Register = R {theRegister :: Int}
  deriving (Eq, Enum, Show)

newtype Param = P {theParameter :: Int}
  deriving (Eq, Enum, Show)

-- register for strings
newtype GlobalRegister = G Int
  deriving (Eq, Enum, Show)

-- index for getelementptr
newtype Index = I [Int]
  deriving (Show)

newtype Arguments = Args [(LLVMType, Value)]
  deriving(Show, Eq)


---------------------- data

data St = St
  { sig          :: Sig
  , cxt          :: Cxt
  , nextLabel    :: Label
  , nextReg      :: Register
  , nextPar      :: Param
  , output       :: Output
  , prevResult   :: [(Value, LLVMType)]
  , globalOut    :: Output
  , globals      :: Block
  , nextGlobal   :: GlobalRegister
  , params       :: [Param]
  , regSize      :: Map Int Integer -- todo still needed?
  , parSize      :: Map Int Integer
  }

-- initial state
initSt :: Sig -> St
initSt s = St { sig          = s
              , cxt          = [[]]
              , nextLabel    = L 0
              , nextReg      = R 0
              , nextPar      = P 0
              , output       = []
              , prevResult   = []
              , globalOut    = []
              , globals      = []
              , nextGlobal   = G 0
              , params       = []
              , regSize      = Map.empty
              , parSize      = Map.empty
              }

builtin :: [(Ident, FunHead)]
builtin =
  [ (Ident "printInt",     FunHead (Ident "printInt") $ FunType Void [Int])
  , (Ident "printDouble",  FunHead (Ident "printDouble") $ FunType Void [Doub])
  , (Ident "printString",  FunHead (Ident "printString") $ FunType Void [String])
  , (Ident "readInt"   ,   FunHead (Ident "readInt") $ FunType Int [])
  , (Ident "readDouble",   FunHead (Ident "readDouble") $ FunType Doub [])
  ]


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

-- value or register
data Value = LitInt Integer | LitDoub Double | LitBool Bool | LitString String | Reg Register | Glob GlobalRegister | X86 X86Reg
  deriving(Show, Eq)

-- binary operators
data Operator = Mo MulOp | Ao AddOp | Ro RelOp
  deriving(Show, Eq)

-- built in registers
data X86Reg
  = Stack Int | Param Int
  | RBP | RSP | RIP | RDI | RSI | RAX | RCX | RDX | RBX | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  | XMM0 | XMM1 | XMM2 | XMM3 | XMM4 | XMM5 | XMM6 | XMM7 | XMM8 | XMM9 | XMM10 | XMM11 | XMM12 | XMM13 | XMM15 
  deriving(Show, Eq)



data Code
  = 
    -- Store LLVMType Value Register -- rodo remove comments
  -- | Load Register LLVMType Register
  Return
  -- | ReturnVoid
  | Call LLVMType Ident
  -- | CallVoid LLVMType Ident Arguments
  | Label Label
  | Add AddOp LLVMType Value Value
  | Mul MulOp LLVMType Value Value
  | DivD Value Value
  | DivI Value
  | Cmp LLVMType Value Value
  | Branch Label
  | BranchCond RelOp Label
  | Global GlobalRegister LLVMType Value
  | Comment String
  | Mov LLVMType Value Value
  | MovF         Value Value -- double literal to e.g RAX
  | MovF2        Value Value -- RAX to e.g XMM2
  | Pop LLVMType Value
  | Push LLVMType Value
  | IncDec AddOp Value
  | CNeg Value
    deriving (Show, Eq)







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



-- load register (if it's variable, then load)
loadReg :: (Value, LLVMType) -> Compile Value ---------- todo, var = reg ptr, temp = reg lit
--loadReg (Reg r, Ptr (Lit t)) = do 
  --r' <- newRegister (Lit t)
  --emit $ Load r' (Lit t) r
  --return (Reg r')
loadReg (X86 x, _) = return $ X86 x
loadReg (Reg r, _) = return $ Reg r -- todo delete this? might do cmp [rsp + x] [rsp+y]? no cuz we do that check in emitbinarop
-- literals cant be the first operand of add/cmp ... so mov them to EAX
loadReg (v, Lit Doub) = do
  emit $ Mov (Lit Doub) v (X86 XMM0) -- todo needed?
  return (X86  XMM0)
loadReg (v, t) = do
  emit $ Mov t v (X86 RAX)
  return (X86 RAX)
--loadReg (r, Lit t) = return r


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
newRegister :: LLVMType -> Compile Register
newRegister t = do
  v <- gets nextReg
  modify $ \st -> st { nextReg = succ v }
  rs <- gets regSize
  modify $ \st -> st { regSize = Map.insert (theRegister v) (size t) rs }
  return v


-- create the next register name
newParam :: LLVMType -> Compile Param
newParam t = do
  (P v) <- gets nextPar
  modify $ \st -> st { nextPar = (P (v + fromIntegral (size t))) }
  rs <- gets parSize
  modify $ \st -> st { parSize = Map.insert v (size t) rs }
  return (P v)

-- calculate register size
size :: LLVMType -> Integer
size (Ptr _) = 8
size (Lit Doub) = 8
size (Lit _) = 8

-- add new variable to the state
newVar :: Ident -> Value -> LLVMType -> Compile ()
newVar x r t = modify $ \st@St { cxt = (b : bs) } -> st { cxt = ((x, r, t) : b) : bs }


-- get type and register for a variable
lookupVar :: Ident -> Compile (Value, LLVMType)
lookupVar id = do 
  c <- gets cxt
  return (fromJust $ cxtContains id c)
  where

    -- context contains var?
    cxtContains :: Ident -> [[(Ident, Value, LLVMType)]] -> Maybe (Value, LLVMType)
    cxtContains id []     = Nothing
    cxtContains id (b:bs) = do 
      let firstSearch = contains id b 
      if (isNothing firstSearch )
        then cxtContains id bs
        else firstSearch
      where

        -- block containts var?
        contains :: Ident -> [(Ident, Value, LLVMType)] -> Maybe (Value, LLVMType)
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

-- add proper prefix to mulop
prefixMulOp :: LLVMType -> MulOp -> String
prefixMulOp (Lit Int) Times  = "imul" -- todo add imul, stackoverflow else i guess
prefixMulOp (Lit Doub) Times = "mulsd"
prefixMulOp (Lit Doub) Div   = "fdiv"
prefixMulOp (Lit Int) Div    = "div"
prefixMulOp (Lit Int) Mod    = "srem" --


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
    LTH   -> "b"
    GTH   -> "a"
    LE    -> "be"
    GE    -> "ae"
    EQU   -> "e"
    NE    -> "ne"

instance ToLLVM Index where
  toLLVM (I (i:[])) = toLLVM (Lit Int) ++ " " ++ show i
  toLLVM (I (i:is)) = toLLVM (Lit Int) ++ " " ++ show i ++ ", " ++ toLLVM (I is)

instance ToLLVM FunHead where
  toLLVM (FunHead (Ident f) (FunType t ts)) = "define " ++ toLLVM t ++ " @" ++ f ++ "(" ++ ( reverse ( drop 2 ( reverse (((\t -> t ++ ", ") . toLLVM) =<< ts)))) ++ ")"

instance ToLLVM Label where
  toLLVM (L l) = "L" ++ show l

instance ToLLVM Value where
  toLLVM = \case
    LitInt i      -> show i
    LitDoub d     -> "__?float64?__(" ++ show d ++ ")"
    LitBool True  -> "1" 
    LitBool False -> "0" 
    LitString s   -> error $ "can only print adress to string, not string directly"
    Reg r         -> toLLVM r
    Glob g        -> toLLVM g 
    X86 (Stack 0) -> "[rbp]"
    X86 (Stack n) -> "[rbp - " ++ show n ++ "]"
    X86 (Param n) -> "[rbp + " ++ show n ++ "]"
    X86 reg       -> (map (\c -> toLower c) (show reg))
    -- todo arguments + 8 + 12 +16...
    
instance ToLLVM Register where
  toLLVM (R r) = "________RRRR" ++ show r

instance ToLLVM GlobalRegister where
  toLLVM (G g) = "str" ++ show g

instance ToLLVM Arguments where
  toLLVM (Args [])          = ""
  toLLVM (Args [(t, v)])    = toLLVM t ++ " " ++ toLLVM v
  toLLVM (Args ((t, v):as)) = toLLVM t ++ " " ++ toLLVM v ++ ", " ++ toLLVM (Args as)

-- print string in array form
stringType :: String -> String
stringType s = "[" ++ show ( (length s) + 1) ++ " x i8]"


instructionIncDec :: AddOp -> String
instructionIncDec Plus = "inc"
instructionIncDec Minus = "dec"

sizeKeyword :: LLVMType -> Value -> String
sizeKeyword t x = case x of
  (X86 (Stack n)) -> prefixT t ++  "word"
  (X86 (Param n)) -> prefixT t ++  "word"
  _               -> ""
  where
    prefixT (Lit Doub) = "d"
    prefixT _ = ""

instance ToLLVM Code where
  toLLVM = \case
    Return                           -> "ret"  -- ++ toLLVM t ++ " "  ++ toLLVM v
    Call t (Ident f)             -> "call  " ++ f
    Label l                                -> toLLVM l ++ ":"
    Cmp t v1 v2 | t == Lit Doub -> "comisd " ++ (sizeKeyword t v1) ++ "" ++ toLLVM v1 ++ ", " ++ toLLVM v2
                | otherwise     -> "cmp  " ++ (sizeKeyword t v1)  ++ " " ++ toLLVM v1 ++ ", " ++ toLLVM v2
    Add op t v1 v2 | t == Lit Int      -> toLLVM op         ++ "   " ++ toLLVM v1 ++ ", " ++ toLLVM v2
                   | t == Lit Doub     -> toLLVM op        ++ "sd " ++ toLLVM v1 ++ ", " ++ toLLVM v2
    Mul op t v1 v2                     -> (prefixMulOp t op)      ++ " " ++ toLLVM v1 ++ ", " ++ toLLVM v2
    -- Alloca adr t                           -> toLLVM adr ++ " = alloca " ++ toLLVM t
    Branch lb                               -> "jmp   " ++ toLLVM lb
    BranchCond op lb                 -> "j" ++ toLLVM op ++ "    " ++ toLLVM lb
    Global adr t (LitString s)             -> toLLVM adr ++ " db \"" ++ s ++ "\", 0"
   -- GetElementPointer r' s r i             -> toLLVM r' ++ " = getelementptr " ++ stringType s ++ ", " ++ stringType s ++ "* " ++ toLLVM r ++ ", " ++ toLLVM i
    Mov t v1 v2 | t == (Lit Doub)                      -> "movsd " ++ toLLVM v2 ++ ", " ++ toLLVM v1
                | otherwise          -> "mov   " ++ toLLVM v2 ++ ", " ++ toLLVM v1
    Pop t v           -> "pop     " ++ toLLVM v
    Push t v | t == Lit Doub ->     "push   "++ toLLVM v      -- "push dword " ++ toLLVM v __?float64?__(
             | t == Lit String -> "push word ["  ++ toLLVM v ++ "]"
             | otherwise -> "push   " ++ (sizeKeyword t v)  ++ " " ++ toLLVM v 
    IncDec op v -> instructionIncDec op ++ " " ++ toLLVM v
    CNeg v -> "neg  " ++ toLLVM v
    MovF  v1 v2 -> "mov   " ++ toLLVM v2 ++ ", " ++ toLLVM v1
    MovF2 v1 v2 -> "movq  " ++ toLLVM v2 ++ ", " ++ toLLVM v1
    DivD v1 v2 -> "divsd " ++ toLLVM v1 ++ ", " ++ toLLVM v2
    DivI v       -> "div   " ++ toLLVM v
    Comment ""                             -> ""
    Comment s                              -> "; " ++ s
    c -> show c
-- todo fix align








---------------------------------------------------------------------------------- emit code



-- add llvm code line to output
emit :: Code -> Compile ()
-- emit (Store (Lit Void) _ _) = return ()
-- emit (Load  _ (Lit Void) _) = return ()
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









----------------------------------------------------- helpers for bool-list operations

-- create bit array with length n
bitArray :: Int -> [Bool]
bitArray n = take n (repeat False)


replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs

-- bitwise and
bitAnd :: [Bool] -> [Bool] -> [Bool]
bitAnd a b = map (\(c,d) -> c && d) (zip a b)

-- bitwise or
bitOr :: [Bool] -> [Bool] -> [Bool]
bitOr a b = do
  if (False && length a /= length b)
    then error ("bitor " ++ show a ++ show b)
    else map (\(c,d) -> c || d) (zip a b)

-- bitwise xor
bitXor :: [Bool] -> [Bool] -> [Bool]
bitXor a b = map (\(c,d) -> (c && not d) || (d && not c)) (zip a b)

-- bitwise complement
bitMinus :: [Bool] -> [Bool] -> [Bool]
bitMinus a b = map (\(c,d) -> (c && not d)) (zip a b)

-- count nbr of True
boolSum :: [Bool] -> Int
boolSum ds = sum (map (\x -> if (x)
                              then 1
                              else 0) ds)


-- get element in list using index
getElem :: Int -> [a] -> a 
getElem i l = last $ take (i+1) l







---------------------------------------------------------------------------------- Register allocation optimization







-- "main" function for register allocation optimization
registerAlloc :: Compile ()
registerAlloc = do
  -- get def, use and succ sets
  (d, iu, du, s) <- defUseSucc
  --error (show $ map (\x -> map (\(b,i) -> if (b) then show i ++ show i else "") (zip x [0..])) s)
  --emit $ Comment (show iu)
  --error (show iu)
  --error (show $ map (\x -> map (\(b,i) -> if (b) then show i ++ " @@@" else "") (zip x [0..])) d)
  --error (show $ map (\x -> map (\(b,i) -> if (b) then show i else "") (zip x [0..])) iu)
  -- calculate liveness
  li             <- liveness (d, iu, s)
  ld             <- liveness (d, du, s)
  --error (show $ map (\x -> map (\(b,i) -> if (b) then show i ++ show i ++ show i else "") (zip x [0..])) li)
  --emit $ Comment (show ld)
  --emit $ Comment (show li)
  -- get interference graph
  let ii         = interference li
  --error (show $ map (\x -> map (\(b,i) -> if (b) then show i ++ " @@@" else "") (zip x [0..])) ii)

  --emit $ Comment (show ii)
  let id         = interference ld
  -- real registers
  let iRegs       = [RCX, RBX, RSI, R8, R9, R10, R11, R12, R13, R14, R15] -- rdi rax
  let dRegs       = [XMM2, XMM3, XMM4, XMM5, XMM6, XMM7, XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM15]
  -- nbr of real-regs
  let ki          = length iRegs
  let kd          = length dRegs
  -- k color to get real-reg and stack-var partitions + color for each real-reg
  (ri, si, ci)   <- kColor ii ki
  --emit $ Comment (show ri)
  (rd, sd, cd)   <- kColor id kd
  -- update code with real-regs
  registerOutput (Lit Int)  iRegs ri ci
  --o'' <- gets output
  --emit $ Comment (show $ map toLLVM o'')
  registerOutput (Lit Doub) dRegs rd cd
  -- space needed for spilled stack variables 
  let  intLocals  = 4*(length si)
  let doubLocals  = 8*(length sd)
  -- local var locations
  let iStack      = map (\(_, y) -> Stack (y*8)) (zip si [1..]) -- todo fix offset
  let dStack      = map (\(_, y) -> Stack (y*8 + intLocals)) (zip sd [1..]) -- todo fix offset
  -- which stack-location each spill register maps to
  let sli         = map (\(_, i) -> replaceNth i True (bitArray ki)) (zip si [0..])
  let sld         = map (\(_, i) -> replaceNth i True (bitArray kd)) (zip sd [0..])
  -- update code with stack registers for spilled vars
  registerOutput (Lit Int)  iStack si sli
  registerOutput (Lit Doub) dStack sd sld
  -- decrement RSP to make space for stack variables
  o             <- gets output
  let (o1, o2)   = splitAt 2 (reverse o)
  let localSize  = intLocals + doubLocals
  -- align stack if not 128 bit aligned
  (P p)         <- gets nextPar
  let pushRbp    = 8
  let returnAdr  = 8
  let aligned    = (localSize + (p-8) + pushRbp + returnAdr) `mod` 16 == 0
  --error (show aligned ++ show localSize ++ show p)
  let align      = if (aligned)     
                    then                             o2
                    else  (Push (Lit Int) (X86 RCX)):o2
  -- make space for local variables
  let prep       = if (localSize > 0)
                    then (Add Minus (Lit Int) (X86 RSP) (LitInt (toInteger $ localSize))):align
                    else                                                                  align
  let o' = reverse (o1++prep)
  -- pop RCX that was used to align before
  let o'' = if (aligned)
              then o'
              else alignBeforeRet o'
  modify $ \st -> st { output = o''}


-- Pop RCX before ret instructions to remove alignment
alignBeforeRet 
  :: [Code] -- remaining code to align
  -> [Code] -- aligned code
alignBeforeRet [] = []
alignBeforeRet (c:cs) = do
  let cs' = if (c == Return)
              then do 
                let (cs1, cs2) = splitAt 2 cs
                cs1 ++ ((Pop (Lit Int) (X86 RCX)):cs2)
              else cs
  c:(alignBeforeRet cs')


-- get a tuple with lists where element n is the def, use and succ set for the nth instruction
defUseSucc 
  :: Compile ([[Bool]], [[Bool]], [[Bool]], [[Bool]]) -- def set-, int use-, double use-, and successor set for each line of code
defUseSucc = do
  o                      <- gets output
  n_label                <- gets nextLabel
  n_reg                  <- gets nextReg
  defs'                  <- defs
  uses                   <- mapM (\(code, def') -> use code def') (zip (reverse o) defs')
  let (intUses, doubUses) = unzip uses
  succs                  <- mapM succSet (zip (reverse o ) [0..])
  --return zip3 defs' uses succs
  return (defs', intUses, doubUses, succs)
  where

    -- helper: call def set calculator for every line of code
    defs 
      :: Compile [[Bool]] -- def set for each line of code
    defs = do
      o          <- gets output
      (R n_reg)  <- gets nextReg
      -- set of already defined registers
      let deffed  = bitArray n_reg
      defsets    <- defSets deffed o
      return $ reverse defsets
      where


        -- get all def sets
        defSets 
          :: [Bool]           -- already defined registers
          -> [Code]           -- code to check
          -> Compile [[Bool]] -- def set for each line of code
        defSets deffed' []       = return []
        defSets deffed' (o':os') = do
          (d, deffed'')      <- def deffed' o' -- def sets should be list for each elem
          ds                 <- defSets deffed'' os'
          return (d:ds)
          where


            -- get def set for a line of code
            def 
              :: [Bool]                   -- previously defined variables
              -> Code                     -- the code
              -> Compile ([Bool], [Bool]) -- the def set and the already defined variables
            def deffed x = case x of
              (Mov _ _ r) -> defV r
              (MovF2 _ r) -> defV r
              -- only mov-instructions can introduce new variables
              v           -> do
                (R n_reg)         <- gets nextReg
                return ((bitArray n_reg), deffed)
              where


                -- get def set for a value (for register its the index, nothing for any other value)
                defV 
                  :: Value                    -- value to check
                  -> Compile ([Bool], [Bool]) -- def set and already defined registers
                defV (Reg r) = do 
                  (R n_reg)     <- gets nextReg
                  let thisReg    = replaceNth (theRegister r) True (bitArray n_reg)
                  let thisReg'   = map (\(a,b) -> ((not a) && b)) (zip deffed thisReg)
                  --error (show thisReg' ++ "--------------")
                  -- update 
                  let deffed'    = map (\(a,b) -> a || b)         (zip deffed thisReg')
                  return (thisReg', deffed')
                defV _       = do
                  (R n_reg)     <- gets nextReg
                  --error (show n_reg ++ "--------------")
                  return ((bitArray n_reg), deffed)

    
    -- get succ set for a line of code
    succSet 
      :: (Code, Int)    -- a line of code and the line number
      -> Compile [Bool] -- the succ set of the code
    succSet ((Branch l), _)           = succL l
    succSet ((BranchCond _ l), index) = do 
      lab <- succL l
      o   <- gets output
      let next = replaceNth (index+1) True (bitArray (length o))
      return $ bitOr lab next
    succSet (_, index)                    = do
      o   <- gets output
      let none = bitArray $ length o
      if (index + 1 < length o)
        then return $ replaceNth (index+1) True none
        else return none


    -- get successors of 1 label
    succL 
      :: Label          -- the label
      -> Compile [Bool] -- the succ set for the label
    succL l' = do 
      l'' <- labelLine l'
      o   <- gets output
      return $ replaceNth l'' True (bitArray (length o))
      where


        -- get the line that label is on
        labelLine 
          :: Label       -- the label
          -> Compile Int -- the line
        labelLine l'' = do
          o <- gets output
          return $ fromJust $ elemIndex (Label l'') (reverse o)


    -- get use set for a line of code
    use 
      :: Code                     -- get use set of this code
      -> [Bool]                   -- already defined registers
      -> Compile ([Bool], [Bool]) -- int use set and double use set for this code
    use x defines = case x of
      (Add _  t v1   v2) -> combine t v1 v2
      (Mul _  t v1   v2) -> combine t v1 v2
      (Mov    t v1   v2) -> combine t v1 v2
      (Cmp    t v1   v2) -> combine t v1 v2
      --(MovF     v1   v2) -> combine t v1 v2
      (DivD     v1   v2) -> combine (Lit Doub) v1 v2
      (DivI     v      ) -> combine (Lit Int)  v  (LitInt (-1)) -- -1 is dummy value
      (MovF2     _   v2) -> combine (Lit Doub) v2 (LitDoub (-1.0))
      (Pop    t       v) -> combine t          v  (LitInt (-1)) 
      (Push   t       v) -> combine t          v  (LitInt (-1)) 
      (IncDec _       v) -> combine (Lit Int)  v  (LitInt (-1)) 
      (CNeg           v) -> combine (Lit Int)  v  (LitInt (-1))
      _                  -> combine (Lit Int)     (LitInt (-1)) (LitInt (-1))
      where
      

      -- get the use set for 2 values
      combine 
        :: LLVMType                 -- type of the values
        -> Value                    -- value 1
        -> Value                    -- value 2
        -> Compile ([Bool], [Bool]) -- int use set and double use set of the values
      combine t v1 v2 = do
        v1' <- useV v1
        v2' <- useV v2
        return $ partitionType t (bitOr v1' v2')
        where


          -- get the use set of a value
          useV 
            :: Value          -- value to check
            -> Compile [Bool] -- the use set of the value
          useV (Reg r) = do 
            (R n) <- gets nextReg
            -- dont add to use set if its also defined here
            if (getElem (theRegister r) defines)
              then return (bitArray n)
              else return $ replaceNth (theRegister r) True (bitArray n)
          useV _       = do
            (R n) <- gets nextReg
            return $ bitArray n


          -- put result in the correct partition depending on type
          partitionType 
            :: LLVMType         -- type of this register
            -> [Bool]           -- use set
            -> ([Bool], [Bool]) -- use set of ints and doubles
          partitionType (Lit Doub) b =    (take (length b) (repeat False), b)
          partitionType _          b = (b, take (length b) (repeat False))





-- create a list with the liveness of each virtual register in the current function
-- element n corresponds to n:th instruction, each element is a list of live registers
-- todo make sure u do from backwards to top
liveness 
  :: ([[Bool]], [[Bool]], [[Bool]]) -- def, use and succ- sets
  -> Compile [[Bool]]               -- liveness for each line
liveness dus = do
  o           <- gets output
  let n_ins    = length o
  (R n_regs)  <- gets nextReg
  -- 1 list for each code line, each list has 1 bool for each reg
  let livein   = take n_ins (repeat (take (fromIntegral n_regs) (repeat False)))
  -- get livein sets
  let live     = iterate dus livein
  -- "add" def to livein because a variable is live when it is being defined
  let (def, _, _) = dus
  let def_live = foldl1 (zipWith $ zipWith (||)) ([live, def])
  return def_live
  where


    -- iterate until livein doesnt change
    iterate 
      :: ([[Bool]], [[Bool]], [[Bool]]) -- def, use and succ-sets 
      -> [[Bool]]                       -- current livein set
      -> [[Bool]]                       -- updated livein set
    iterate (defs, uses, succs) livein' = do -- (d, u, s):ds)
      --error (show defs)
      let livein'' = iterateLine 0 livein'
      --error (show livein'')
      --return uses
      -- repeat if not done
      if (livein' == livein'')
        then do 
          --error (show livein'')
          -- could be double
          --error (show uses ++ "\n\n" ++ show (length livein') ++ show (length livein'') ++  "\n" ++ show livein'' ++ "\n\n" ++ show livein')
          livein''
        else do 
          --error (show (length livein') ++ show (length livein'') ++  "\n" ++ show livein'' ++ "\n\n" ++ show livein')
          iterate (defs, uses, succs) livein''
      where


        --update 1 line according to algorithm
        iterateLine 
          :: Int      -- line
          -> [[Bool]] -- current livein set
          -> [[Bool]] -- updated livein set
        iterateLine i livein''' | i == length livein''' = livein'''
        iterateLine i livein''' = do
        -- out set for 1 line
        let out       = liveSucc (getElem i succs) livein'''
        -- successors "-" def set
        let s_minus_d = bitMinus out (getElem i defs)
        let line  = bitOr (getElem i uses) s_minus_d
        let livein'''' =  replaceNth i line livein'''
        iterateLine (i+1) livein''''
        where


          -- get livein of successors == out set
          liveSucc 
            :: [Bool]   -- successors for this line
            -> [[Bool]] -- current livein sets
            -> [Bool]   -- out set
          liveSucc []      _     = take (length (head succs)) (repeat False)
          liveSucc (s:ss) (l:ls) = do
            let l'  = if (s)
                        then do

                          --error (show l)
                          l 
                        else take (length l) (repeat False) --- here todo omegalul
            bitOr l' (liveSucc ss ls)


-- do something with output


-- create an interference tree from liveness list
interference 
  :: [[Bool]] -- livein set
  -> [[Bool]] -- interference graph
interference livein = do
  -- edges from each line
  let matrices = map lineInterference livein
  -- combine all edges
  let matrix = foldl1 (zipWith $ zipWith (||)) matrices
  matrix
  where

    -- calculate interference on one line
    lineInterference 
      :: [Bool]   -- the liveness of each register on the line
      -> [[Bool]] -- an adjacency matrix with edges between all live registers on this line
    lineInterference l' = do
      let n_regs = length l'
      -- if register e is live, then add edge to all the other live ones 
      -- make a list for each variable, an adjacency matrix
      map (\(e, i) -> if e
                        then (bitMinus l' ( replaceNth i True (bitArray n_regs)))  -- basically just remove the node itself : [1,1,1,1] -> [0,1,1,1]
                        else bitArray n_regs)
                      (zip l' [0..]) -- inlcude index 0..


-- k-color the registers
kColor 
  :: [[Bool]]                         -- register interference graph
  -> Int                              -- k colors
  -> Compile ([Int], [Int], [[Bool]]) -- "variables" to be allocated to registers / memory, and the color of each "register-variable"
kColor matrix k = do
  let n_regs            = length matrix
  let stack             = repeatAlg matrix n_regs (take n_regs (repeat False))
  --emit $ Comment (show stack)
  let (stack', matrix', spill) = removeSpill stack matrix
  -- an empty k-bit-map representation of color choice for each register
  let colors            = take n_regs (repeat (take k (repeat False)))
  let coloring          = color stack' matrix' colors
  return (stack', spill, coloring)
  where

    -- color nodes in stack
    color 
      :: [Int]    -- stack of nodes to color
      -> [[Bool]] -- interference graph
      -> [[Bool]] -- colors taken by all neighbours to a node
      -> [[Bool]] -- color of each node in the stack
    color []     matrix' colors' = colors'
    color (s:ss) matrix' colors' = do
      let neighbours = getElem s matrix'
      let nc         = zip neighbours colors'
      -- get the color of each neighbour
      let cs         = map (\(i, c) -> do 
                                        if (i)
                                          then c
                                          else take k (repeat False)) nc
      -- combine the color of each neighbour
      let taken_colors = foldl1 (zipWith (||)) cs
      -- take avialable color
      let this_color   = availableColor taken_colors
      let colors''     = replaceNth s this_color colors'
      -- choose colors for the rest of the stack
      color ss matrix' colors''
      where

        -- get the color thats not taken todo (should not be able to fail since we removed all spill)
        availableColor 
          :: [Bool] -- taken colors
          -> [Bool] -- available color
        availableColor (c':cc') = do
          -- if color taken then try the next one, else take this color
          if (c')
            then (False:(availableColor cc'))
            else (True:(take (length cc') (repeat False)))


    -- remove spill nodes and their edges, place the spill nodes in a new stack
    removeSpill 
      :: [(Int, Bool)]            -- varaibles + spill flag
      -> [[Bool]]                 -- interference graph
      -> ([Int], [[Bool]], [Int]) -- "register-variables", interference graph without spill, and spilled nodes
    removeSpill []              m = ([], m, [])
    removeSpill ((s, False):ss) m = do 
      -- keep non spill node in stack
      let       (ss', m', spill)  = removeSpill ss m
      ((s:ss', m', spill))
    removeSpill ((s, True) :ss) m = do 
      -- remove spill node from the first stack and put it in the spill stack
      let m'                      = deleteNode s m
      let    (ss', m'',   spill)  = removeSpill ss m'
      (ss', m'', s:spill)

      

    -- find node and delete it until k nodes left or fail
    repeatAlg 
      :: [[Bool]]      -- current interference graph (nodes are getting deleted)
      -> Int           -- nodes left
      -> [Bool]        -- deleted nodes
      -> [(Int, Bool)] -- nodes deleted in order according to, "simplify select", with a spill flag
    repeatAlg m r deleted = do
      if (r == 0) -- todo <= k
        then []
        else do
          -- find node n and delete edges to get new graph m'
          let (m', n, _) = findN m (-1) deleted
          let deleted' = replaceNth (fst n) True deleted
          -- number of nodes left -1
          let r' = r - 1 -- todo r redundant with deleted
          -- get the rest of the nodes
          let stack' = repeatAlg m' r' deleted'
          -- combine results
          (n:stack')

      
    -- find node n with <k egdges, also delete edges
    -- spill node with most interference
    -- int argument is the max degree discovered
    -- result: 
    -- graph with deleted edges, 
    -- the edge with a spill flag and 
    -- degree of node we need to spill
    findN 
      :: [[Bool]]                           -- current interference graph
      -> Int                                -- maximum degree found
      -> [Bool]                             -- deleted nodes
      -> ([[Bool]], (Int, Bool), Maybe Int) -- interference graph with n removed, n with spill flag, and maximum degree found if we need to spill
    -- look for node to spill
    findN []     degree _ = ([], (-1, False), Just degree)
    -- look for node with <k edges
    findN (b:bs) degree deleted''= do
      let degree' = boolSum b
      let index   = (length matrix) - (length bs) - 1
      let del = isDeleted bs deleted''
      -- if degree' < k, delete this node, otherwise check the next node
      if (degree' < k && not del)
        then do
          let matrix'' = deleteNode index matrix
          (matrix'', (index, False), Nothing)
        else do 
          -- update max degree (for spill prioritization)
          let degree'' = if (degree' >= degree)
                          then degree'
                          else degree
          -- try delete other node
          let (matrix''', (b', flag), degree''') = findN bs degree'' deleted''
          -- if we deleted, then return, else see if we should spill the current node
          if (isNothing degree''') -- todo maybe entire result use when?
            then (matrix''', (b', flag), degree''')
            else do
              -- if this node had the highest degree, then spill this, otherwise ask previous node to spill
              if (fromJust degree''' == degree')
                then do
                  let matrix'''' = deleteNode index matrix
                  (matrix'''', (index, True), Nothing)
                -- ask previous node to spill
                -- dont actually care about any value other than degree''' in this case
                else (matrix''', (b', flag), degree''')

          
    -- delete node at given index
    deleteNode 
      :: Int      -- index
      -> [[Bool]] -- current interference graph
      -> [[Bool]] -- interference graph with n deleted
    deleteNode index' m = do
      -- remove edges to n
      let m'  = map (\x -> replaceNth index' False x) m
      -- remove edges from n
      let m'' = replaceNth index' (take (length m') (repeat False)) m'
      m''

    
    -- see if node (specified by the remaining nodes rest) has been deleted
    isDeleted 
      :: [a]    -- the rest of the nodes
      -> [Bool] -- deleted nodes
      -> Bool   -- has this node been deleted?
    isDeleted rest deleted''' = do
      let node = (length matrix) - (length rest) - 1
      getElem node deleted'''

    



-- update "output" with real registers / memory
registerOutput 
  :: LLVMType  -- int or double register
  -> [X86Reg]  -- available real registers
  -> [Int]     -- temp registers to be allocated
  -> [[Bool]]  -- which color each temp register was given
  -> Compile ()
registerOutput typ realRegs tempRegs colorMap = do
  --for each elem in stack
  traverseStack tempRegs
  -- for each code
  -- if stack elem == reg, replace
  where

    -- for each register, see if it needs to be updated somewhere
    traverseStack 
      :: [Int]     -- temp registers to update
      -> Compile ()
    traverseStack []     = return ()
    traverseStack (s:ss) = do
      o     <- gets output
      let o' = traverseCode (s, o)
      modify $ \st -> st { output = o'}
      traverseStack ss
      where

        -- see if register needs to be updated in any line of code
        traverseCode 
          :: (Int, [Code]) -- temp register and current generated code
          -> [Code]        -- code with the temp register allocated its real register / memory
        traverseCode x = case x of
          (s, []) -> []
          (s, (c:cc)) -> do
            let c'  = updateCode c (R s)
            let cc' = traverseCode (s, cc)
            (c':cc')
            where

              -- update 1 line of code
              updateCode 
                :: Code     -- code to be updated
                -> Register -- register to update
                -> Code     -- updated code
              updateCode x r = case x of
                (Mov    t v1 v2) -> (Mov    t  (swap t          v1 r) (swap t v2 r))
                (Add op t v1 v2) -> (Add op t  (swap t          v1 r) (swap t v2 r))
                (Mul op t v1 v2) -> (Mul op t  (swap t          v1 r) (swap t v2 r))
                (Cmp    t v1 v2) -> (Cmp    t  (swap t          v1 r) (swap t v2 r))
                (MovF2    v1 v2) -> (MovF2     v1                      (swap (Lit Doub) v2 r))
                (DivD     v1 v2) -> (DivD      (swap (Lit Doub) v1 r) (swap (Lit Doub) v2 r))
                (DivI     v    ) -> (DivI      (swap (Lit Int)  v  r)) 
                (Pop    t     v) -> (Pop    t  (swap t          v  r))
                (Push   t     v) -> (Push   t  (swap t          v  r))
                (IncDec op    v) -> (IncDec op (swap (Lit Int)  v  r))
                (CNeg         v) -> (CNeg      (swap (Lit Int)  v  r))
                c -> c
                where

                  -- swap temp-reg with real reg if its the correct one
                  swap 
                    :: LLVMType -- register type 
                    -> Value    -- value to update
                    -> Register -- register to update
                    -> Value    -- updated value
                  swap t v r = do
                    -- only update if its the correct type
                    if ((typ == Lit Doub && t == Lit Doub) || (typ == Lit Int && not (t == Lit Doub) ))
                      then if (v == Reg r)
                            then (X86 (color2Reg r))
                            else v
                      else v
                    where

                      -- get the real reg for a temp-reg, given a color mapping
                      color2Reg 
                        :: Register -- register to update
                        -> X86Reg   -- the corresponding real register / memory
                      color2Reg (R reg) = do
                        -- the color of this reg
                        let color = getElem reg colorMap
                        --the real reg corresponding to the color
                        let index = (elemIndex True color)
                        if (isNothing index)
                          then error $ show (s:ss)
                          else do
                            let rr = getElem (fromJust index) realRegs
                            --if (rr == X)
                            --emit $ Comment (show rr ++ ----------)
                            rr
                        -- more regs than colors since we removed spill, eaxctly k colors tho




















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
  --let args' = Args $ zip (map (\(Argument t id) -> (Lit t)) args) (map (\x -> Reg x) (params st')) -- kinda ugly to have have params here, just do it in compilefun todo
  -- print the function header
  let func  = intercalate "" [f, ":\n", unlines $ map (indent . toLLVM) $ reverse $ (output st'), "\n"]
  -- print the string constants
  let glob  = unlines $ map toLLVM $ reverse (globalOut st')
  (func, glob, st')
  -- compile the function
  where st' = execState (compileFun (Ident f) t args ss) st


-- compile function helper
compileFun :: Ident -> Type -> [Arg] -> [Stmt] -> Compile ()
compileFun (Ident f) t0 args ss = do
  -- reset code output and register -------------------- todo reset block too? dont care about var from other function
  modify $ \st -> st { output = []}
  modify $ \st -> st { globalOut = []}
  modify $ \st -> st { nextReg = R 0}
  modify $ \st -> st { nextPar = P 8}
  -- make parameter registers
  --regs <- mapM (\(Argument t' x) -> newRegister (Lit t')) args ---------------------- todo prob dont need both param and alloc
  --let arg_reg = zip args regs
  -- make a new variable and alloc memory for each parameter:
  regs <- mapM (\(Argument t' x) -> do
                                        --r' <- newRegister (Lit t') --- todo maybe ptr lit t' cuz variable, not reg ish
                                        (P r') <- newParam (Lit t')
                                        newVar x (X86 (Param r')) (Ptr (Lit t'))
                                        -- emit $ Alloca r' (Lit t')
                                        -- emit $ Store (Lit t') (Reg r) r'
                                        return (P r')
                                      ) args
  -- store current parameters
  modify $ \st -> st { params = regs} -- todo not needed?
  -- push registers
  emit $ Push (Lit Int) (X86 RBP)-- todo says dword on lecture
  emit $ Mov (Lit Int) (X86 RSP) (X86 RBP) -- todo what type?
  -- todo sub esp localbytes , do in register alloc stage
  compileStms ss
  -- add "ret void" if no return statement at the end
  if (t0 == Void)
    then do 
      prevStm <- gets output
      if (length prevStm /= 0 && (head $ words $ toLLVM $ head prevStm) == "ret")
        then return ()
        else do
          -- pop registers
          emit $ Mov (Lit Int) (X86 RBP) (X86 RSP) -- todo this will be after ret :\
          emit $ Pop (Lit Int) (X86 RBP) -- rly int? dword in lecvture
          emit $ Return
    else return ()
  -- optimize
  registerAlloc











---------------------------------------------------------------------------------- compile statements



-- help function for compiling variable declaration
compileDecl :: Type -> Item -> Compile ()
compileDecl t (Init id (ETyped e _)) = do
  -- compile expression and make new variable
  compileExp (ETyped e t) False
  r <- newRegister (Lit t)
  newVar id (Reg r) (Ptr (Lit t))
  --emit $ Alloca r (Lit t) ---- todo create stack or 
  (p, t')  <- getPrevResult
  -- p' <- loadReg p
  --emit $ Store (Lit t) p' r
  emit $ Mov (Lit t) p (Reg r)

compileDecl t (NoInit id) = do
  -- just create new variable
  r <- newRegister (Lit t)
  newVar id (Reg r) (Ptr (Lit t))
  default0 r t
  --emit $ Alloca r (Lit t)
  where

    -- default value for a given type
    default0 :: Register -> Type -> Compile ()
    default0 r Doub = do
      emit $ MovF  (LitDoub 0.0) (X86 RAX)
      emit $ MovF2 (X86 RAX) (Reg r)
      --setPrevVal (Reg r, Lit Doub)  b
    default0 r Int = emit $ Mov (Lit t) (LitInt 0) (Reg r)  
    default0 r Bool = emit $ Mov (Lit t) (LitBool False) (Reg r)  

-- compile list of statements 
-- Bool: do these statements guarantee a return statement?
compileStms :: [Stmt] -> Compile Bool
compileStms []        = return False
compileStms (s : ss') = do
  returns <- compileStm s
  -- stop if you found a statement that guaranteed a return statement
  if (returns)
    then return True
    else compileStms ss'

-- move result to correct register (RAX or XMM0 if not there already) 
fixReturnReg :: Type -> Value -> Compile ()
fixReturnReg Doub (X86 XMM0) = return ()
fixReturnReg Doub v = emit $ Mov (Lit Doub) v (X86 XMM0)
fixReturnReg t (X86 RAX) = return ()
fixReturnReg t v = emit $ Mov (Lit t) v (X86 RAX)

-- compile statement
-- Bool: does this statement guarantee a return statement?
compileStm :: Stmt -> Compile Bool
compileStm (Retting s0 ret) = do
  case s0 of


    Ret e@(ETyped _ t) -> do
      compileExp e False
      (r, t0)  <- getPrevResult
      -- r' <- loadReg r
      --emit $ Return (Lit t) r'
      fixReturnReg t r   -- todo ?
      -- pop registers
      emit $ Mov (Lit Int) (X86 RBP) (X86 RSP) -- todo this will be after ret :\
      emit $ Pop (Lit Int) (X86 RBP) -- todo ig weere not using ptr anymore ever? omega
      emit $ Return
      return True


    VRet -> do
      -- pop registers
      emit $ Mov (Lit Int) (X86 RBP) (X86 RSP) -- todo this will be after ret :\
      emit $ Pop (Lit Int) (X86 RBP)
      emit $ Return
      emit $ Return
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
          (r, typ')     <- getPrevResult
          r'    <- loadReg (r, typ')
          emit $ Cmp (Lit typ) r' (LitBool True) -- todo fixreturnreg
          emit $ BranchCond EQU t
          emit $ Branch f
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
            (r, typ)   <- getPrevResult
            r'    <- loadReg (r, typ)
            emit $ Cmp (Lit Bool) r' (LitBool True)
            emit $ BranchCond EQU t
            emit $ Branch f
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
          (r, typ')  <- getPrevResult
          r'    <- loadReg (r, typ')
          emit $ Cmp (Lit typ) r' (LitBool True)
          emit $ BranchCond EQU t
          emit $ Branch f
          -- compile statement
          emit $ Label t
          inNewBlock $ compileStm s
          emit $ Branch f
          -- end
          emit $ Label f
          return False


    Ass x e@(ETyped _ typ) -> do
      compileExp e False
      (a, t)  <- lookupVar x
      (r, tr) <- getPrevResult
      -- r'     <- loadReg r -- todo fixreturnreg
      emit $ Mov (Lit typ) r a
      --emit $ Store (Lit typ) r' a
      return False


    Incr i -> incDecr i Plus
    Decr i -> incDecr i Minus


    Empty -> return False


    s -> error $ "not implemented compileStm " ++ printTree s



-- helper for incrementing / decrementing statement
incDecr :: Ident -> AddOp -> Compile Bool
incDecr i op = do
  (adr, t) <- lookupVar i
  -- adr'''   <- loadReg (Reg adr, t) -- todo fixreturnreg
  if (t == (Lit Int) || t == (Ptr (Lit Int)))
    then do 
      emit $ IncDec op adr
      return False
    else do
      emit $ Add op (Lit Doub) (LitDoub 1.0) adr
      return False
     

-- get inner type of pointer
unwrap :: LLVMType -> LLVMType
unwrap (Ptr t) = t
unwrap (Lit t) = (Lit t)




---------------------------------------------------------------------------------- compile expression


-- default register for different types
defaultReg :: Int -> Type -> Value
defaultReg 0 Doub = X86 XMM0
defaultReg 1 Doub = X86 XMM1
defaultReg 0 _    = X86 RAX
defaultReg 1 _    = X86 RBX



-- helper: emit add / mul / rel expression.
emitBinaryOp :: Type -> Operator -> Expr -> Expr -> Bool -> Compile ()
emitBinaryOp t op' e1 e2 b = do
  -- compile arguments
  compileExp e1 True
  (arg1, t1) <- getPrevResult  -- ____rx = lookup
  --r1' <- newRegister
  compileExp e2 True
  (arg2, t2) <- getPrevResult  -- RAX
  --allArgs         <- gets prevResult
  -- args'   <- mapM (\x -> loadReg x) $ take 2 allArgs -- todo fixreturnreg
  --let [(arg1, t1), (arg2, _)] = take 2 allArgs
  --r'    <- loadReg (arg1, t1)
  -- mov the second operand away from RAX / XMM0
  arg2' <- if (arg2 ==  defaultReg 0 t)
            then do
              if (op' == Ro EQU)
                then error (show arg2)
                else return ()
              r2 <- newRegister (Lit t)
              emit $ Mov (Lit t) arg2 (Reg r2) -- rbx
              return (Reg r2)
            else return arg2
  -- mov first operand to RAX / XMM0
  if (arg1 == (defaultReg 0 t))
    then return ()
    else emit $ Mov (Lit t) arg1 (defaultReg 0 t)  -- rax
  -- create result register
  --r               <- newRegister (Lit t)
  -- compile and remove arguments
  case op' of
    Ao op -> emit $ Add  op (Lit t) (defaultReg 0 t)  arg2'
    --Mo Div -> divide
    Mo Mod -> modulo arg2'
    Mo Div -> divide t arg2'
    Mo op  -> emit $ Mul  op (Lit t) (defaultReg 0 t) arg2'
    Ro op  -> compare     op (Lit t) (defaultReg 0 t) arg2'
  removeArgs 2
  -- compare will always return bool
  case op' of 
    Ro _   -> setPrevVal ((defaultReg 0 Bool), (Lit t)) b
    Mo Mod -> setPrevVal ((X86 RDX),         (Lit Int)) b
    _      -> setPrevVal ((defaultReg 0 t   ), (Lit t)) b

  where

    divide :: Type -> Value -> Compile ()
    divide Doub arg2 = emit $ DivD (defaultReg 0 Doub) arg2
    divide Int  arg2 = emit $ DivI arg2

    -- modulo operation
    modulo :: Value -> Compile ()
    modulo arg2 = do
      emit $ Mov (Lit Int) (LitInt 0) (X86 RDX)
      emit $ DivI arg2


    -- emit instructions for comparison
    compare ::  RelOp -> LLVMType -> Value -> Value -> Compile ()
    compare  op typ arg1 arg2 = do -- todo dont need to use arg2
      --r'    <- loadReg (arg2, typ)
      emit $ Cmp typ arg1 arg2
      t <- newLabel
      f <- newLabel
      e <- newLabel
      emit $ BranchCond op t
      emit $ Branch f
      -- true
      emit $ Label t
      emit $ Mov (Lit Bool) (LitBool True) (X86 RAX)
      emit $ Branch e
      -- false
      emit $ Label f
      emit $ Mov (Lit Bool) (LitBool False) (X86 RAX)
      emit $ Branch e
      -- end
      emit $ Label e
      -- todo fix return? / guarantee return



-- compile expression
-- Bool: this expr is an argument to a function, 
-- the bool is needed for knowing whether or not to save or override the previous result in the state 
compileExp :: Expr -> Bool -> Compile ()
compileExp e0 b = case e0 of


  ELitInt i  -> do 
    r <- newRegister (Lit Int)
    emit $ Mov (Lit Int) (LitInt i) (Reg r)
    setPrevVal (Reg r, Lit Int)  b
  ELitDoub d -> do
    --r <- newRegister (Lit Int)
    r2 <- newRegister (Lit Doub)
    emit $ MovF  (LitDoub d) (X86 RAX)
    emit $ MovF2 (X86 RAX) (Reg r2)
    setPrevVal (Reg r2, Lit Doub)  b
  ELitTrue   -> do
    r <- newRegister (Lit Bool)
    emit $ Mov (Lit Bool) (LitBool True) (Reg r)
    setPrevVal (Reg r, Lit Bool)  b
    setPrevVal (LitBool True , Lit Bool) b
  ELitFalse   -> do
    r <- newRegister (Lit Bool)
    emit $ Mov (Lit Bool) (LitBool False) (Reg r)
    setPrevVal (Reg r, Lit Bool)  b
    setPrevVal (LitBool False , Lit Bool) b


  EString s  -> do -- todo
    adr  <- newGlobalRegister
    emitGlobal $ Global adr (Lit String) (LitString s)
    --adr' <- newRegister (Lit String)
    --emit $ GetElementPointer adr' s adr $ I [0, 0]
    setPrevVal (Glob adr, Lit String) b -- todo  should be ok, we need to push but should be handled elsewhere? maybe loadReg should be used: do nothing if int / double, mov / otherwise
    -- actually not the same, push has nothing to do with it, we always to push manually for arguments


  EVar x -> do
    (a, t) <- lookupVar x
    setPrevVal (a, t) b


  EApp x@(Ident _) es -> do
    -- get function header
    FunHead id (FunType t ts) <- gets ((fromMaybe (error "undefined") . Map.lookup x) . sig)
    let n_args = length ts
    -- se if we are calling an extern function or not
    let externNames = ["printInt", "printDouble", "printString", "readInt", "readDouble"]
    let isExtern = or (map (\n -> (Ident n) == id) externNames) 
    -- if not extern, align stack if needed
    let bytes   = sum (map (\t -> size (Lit t)) ts)
    let aligned = bytes `mod` 16 == 0
    if ((not isExtern) && (not aligned))
      then emit $ Push (Lit Int) (X86 RCX)
      else return ()

    -- compile arguments and and make sure they dont override each other
    mapM_ (\e -> do
                  compileExp e False -- todo used to be true
                  (prev, typ) <- getPrevResult
                  -- prev' <- loadReg -- todo fixreturnreg
                  
                  if (isExtern)
                    then emit $ Mov typ prev (X86 RDI) -- use same calling convention as runtime
                    else if (typ == Lit Doub)
                      then do -- "push" doub manually
                        emit $ Mov typ prev (X86 (Stack 0))
                        emit $ Add Minus (Lit Int) (X86 RSP) (LitInt 8)
                      else emit $ Push typ prev) es
    --allArgs <- gets prevResult
    --args    <- mapM (\x -> loadReg x) (reverse $ take n_args allArgs)
    -- fix types
    -- save registers
    if (isExtern)
      then do
        --emit $ Push (Lit Int) (X86 RDI)
        emit $ Push (Lit Int) (X86 RBX)
        emit $ Push (Lit Int) (X86 RSI)
        emit $ Push (Lit Int) (X86 RDX)
        emit $ Push (Lit Int) (X86 RCX)
        emit $ Push (Lit Int) (X86 R8)
        emit $ Push (Lit Int) (X86 R9)
        emit $ Push (Lit Int) (X86 R10)
        emit $ Push (Lit Int) (X86 R11)
        emit $ Push (Lit Int) (X86 R11) -- extra to align
      else return ()
    --let args' = zip ts' args
    -- if void function, then no need to save the result

    emit $ Call (Lit t) id
    -- remove arguments from stack, if using push
    if (isExtern)
      then do
        emit $ Pop (Lit Int) (X86 R11) -- extra to align
        emit $ Pop (Lit Int) (X86 R11)
        emit $ Pop (Lit Int) (X86 R10)
        emit $ Pop (Lit Int) (X86 R9)
        emit $ Pop (Lit Int) (X86 R8)
        emit $ Pop (Lit Int) (X86 RCX)
        emit $ Pop (Lit Int) (X86 RDX)
        emit $ Pop (Lit Int) (X86 RSI)
        emit $ Pop (Lit Int) (X86 RBX)
        --emit $ Pop (Lit Int) (X86 RDI)
        return ()
      else do 
        -- remove arguments + alignment bytes
        emit $ Add Plus (Lit Int) (X86 RSP) (LitInt $ sum $ map (\x -> size (Lit x)) ts)
        if (not aligned)
          then emit $ Pop (Lit Int) (X86 RCX)
          else return ()
    if (t == Doub)
      then setPrevVal ((X86 XMM0), (Lit t)) b
      else setPrevVal ((X86 RAX ), (Lit t)) b
{-
    if (t == Void)
      then do
        emit $ CallVoid (Lit t) id (Args args')
        removeArgs n_args
      else do
        r <- newRegister (Lit t)
        emit $ Call r (Lit t) id (Args args')
        --removeArgs n_args
        setPrevVal (Reg r, (Lit t)) b
        -}


  ETyped (EAdd e1 op e2) t              -> emitBinaryOp t (Ao op) e1 e2 b
  ETyped (EMul e1 op e2) t              -> emitBinaryOp t (Mo op) e1 e2 b
  ETyped (ERel e1@(ETyped _ t) op e2) _ -> emitBinaryOp t (Ro op) e1 e2 b


  EAnd e1 e2 -> do
    -- e1 true?
    compileExp e1 True
    (e1_result, t1) <- getPrevResult
    -- e1_result <- loadReg r1 -- todo fixreturnreg
    t         <- newLabel
    f         <- newLabel
    -- create result variable
    --result    <- newRegister (Lit Bool)
    --emit $ Alloca result (Lit Bool)
    -- if e1 true, then compile e2, otherwise skip (lazy eval)
    r'    <- loadReg (e1_result, t1)
    emit $ Cmp (Lit Bool) r' (LitBool True)
    emit $ BranchCond EQU t
    emit $ Branch f

    -- e2 true?
    emit $ Label t 
    compileExp e2 False -- it is ok to overwrite e1_result
    (e2_result, typ2) <- getPrevResult
    --e2_result <- loadReg r2 -- todo fixreturnreg
    t2        <- newLabel
    -- if e2 true, emit true, otherwise false
    e2_result'    <- loadReg (e2_result, typ2)
    emit $ Cmp (Lit Bool) e2_result' (LitBool True)
    emit $ BranchCond EQU t2
    emit $ Branch f

    -- emit true
    emit $ Label t2
    --emit $ Store (Lit Bool) (LitBool True) result
    emit $ Mov (Lit Bool) (LitBool True) (X86 RAX)
    end <- newLabel
    emit $ Branch end

    -- emit false
    emit $ Label f
    --emit $ Store (Lit Bool) (LitBool False) result
    emit $ Mov (Lit Bool) (LitBool False) (X86 RAX)
    emit $ Branch end

    -- end
    emit $ Label end
    removeArgs 1
    setPrevVal (X86 RAX, Ptr (Lit Bool)) b -- todo not ptr?


  EOr e1 e2 -> do
    -- e1 true?
    compileExp e1 True
    (e1_result, t1) <- getPrevResult
    -- e1_result <- loadReg r1 -- todo fixreturnreg
    t         <- newLabel
    f         <- newLabel
    --result    <- newRegister (Lit Bool)
    -- create result variable
    --emit $ Alloca result (Lit Bool)
    -- if e1 true, then emit true, otherwise check e2
    e1_result'    <- loadReg (e1_result, t1)
    emit $ Cmp (Lit Bool) e1_result' (LitBool True)
    emit $ BranchCond EQU t
    emit $ Branch f

    -- e2 true?
    emit $ Label f 
    compileExp e2 False -- ok to overwrite e1_result
    (e2_result, t2) <- getPrevResult
    -- e2_result <- loadReg r2 -- todo fixreturnreg
    f2        <- newLabel
    -- if e2 true, then emit true, otherwise emit false
    e2_result'    <- loadReg (e2_result, t2)
    emit $ Cmp (Lit Bool) e2_result' (LitBool True)
    emit $ BranchCond EQU t
    emit $ Branch f2 -- todo fix all of these

    -- both were false
    emit $ Label f2
    --emit $ Store (Lit Bool) (LitBool False) result
    emit $ Mov (Lit Bool) (LitBool False) (X86 RAX)
    end <- newLabel
    emit $ Branch end

    -- something was true
    emit $ Label t
    --emit $ Store (Lit Bool) (LitBool True) result
    emit $ Mov (Lit Bool) (LitBool True) (X86 RAX)
    emit $ Branch end

    -- end
    emit $ Label end
    removeArgs 1
    setPrevVal (X86 RAX, Ptr (Lit Bool)) b


  Neg (ETyped e t) -> do
    if (t == Int)
      then do 
        compileExp (ETyped e t) b
        (i0, t') <- getPrevResult
        emit $ Mul Times (Lit Int) i0 (LitInt (-1))
        --let i = case i0 of
        --          LitInt i0' -> i0'
        --          _          -> error "lol" -- not possible
        -- r' <- loadReg -- todo fixreturnreg
        --setPrevVal (LitInt ((-1)*i), Lit Int ) b
      else
        case e of
          (ELitDoub d) -> do
            compileExp (ETyped e t) b
            r' <- getPrevResult
            -- r' <- loadReg -- todo fixreturnreg
            setPrevVal (LitDoub ((-1.0)*d), Lit Doub) b
          _            -> compileExp (ETyped (EMul e Times (ELitDoub (-1.0)) ) t) b
    

  Not (ETyped e Bool) -> do
    case e of
      (ELitTrue)  -> setPrevVal (LitBool False, Lit Bool) b -- idk if b or false
      (ELitFalse) -> setPrevVal (LitBool True, Lit Bool) b
      _           -> do
        compileExp e True -- todo why not false?
        (r, t) <- getPrevResult
        -- mov value to RAX, because you cant to neg on [RBP + 8] for example
        -- we dont know which type of variable it is until after register allocation
        emit $ Mov (Lit Bool) r (X86 RAX)
        emit $ CNeg (X86 RAX)
        emit $ Mov (Lit Bool) (X86 RAX) r


  ETyped e _ -> compileExp e b


  e          -> error $ "not implemented compileexp " ++ show e