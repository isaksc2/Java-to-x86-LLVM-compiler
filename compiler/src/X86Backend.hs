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
type Block = [(Ident, Value, Type)]
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

newtype Arguments = Args [(Type, Value)]
  deriving(Show, Eq)


---------------------- data

data St = St
  { sig          :: Sig
  , cxt          :: Cxt
  , nextLabel    :: Label
  , nextReg      :: Register
  , nextPar      :: Param
  , output       :: Output
  , prevResult   :: [(Value, Type)]
  , globalOut    :: Output
  , globals      :: Block
  , nextGlobal   :: GlobalRegister
  }

-- initial state
initSt :: Sig -> St
initSt s = St { sig          = s
              , cxt          = [[]]
              , nextLabel    = L 0
              , nextReg      = R 0
              , nextPar      = P 16
              , output       = []
              , prevResult   = []
              , globalOut    = []
              , globals      = []
              , nextGlobal   = G 0
              }

builtin :: [(Ident, FunHead)]
builtin =
  [ (Ident "printInt",     FunHead (Ident "printInt") $ FunType Void [Int])
  , (Ident "printDouble",  FunHead (Ident "printDouble") $ FunType Void [Doub])
  , (Ident "printString",  FunHead (Ident "printString") $ FunType Void [String])
  , (Ident "readInt"   ,   FunHead (Ident "readInt") $ FunType Int [])
  , (Ident "readDouble",   FunHead (Ident "readDouble") $ FunType Doub [])
  ]


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
  = Locals Int | Param Int | Stack Int
  | RBP | RSP | RIP | RDI | RSI | RAX | RCX | RDX | RBX | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  | XMM0 | XMM1 | XMM2 | XMM3 | XMM4 | XMM5 | XMM6 | XMM7 | XMM8 | XMM9 | XMM10 | XMM11 | XMM12 | XMM13 | XMM14 | XMM15 
  deriving(Show, Eq)



data Code
  = 
    -- Store Type Value Register -- rodo remove comments
  -- | Load Register Type Register
  Return
  -- | ReturnVoid
  | Call Type Ident
  -- | CallVoid Type Ident Arguments
  | Label Label
  | Add AddOp Type Value Value
  | Mul MulOp Type Value Value
  | DivD Value Value
  | DivI Value
  | Cmp Type Value Value
  | Branch Label
  | BranchCond RelOp Label
  | Global GlobalRegister Type Value
  | Comment String
  | Mov Type Value Value
  | MovF         Value Value -- double literal to e.g RAX
  | MovF2        Value Value -- RAX to e.g XMM2
  | MovF3        Value Value -- XMM to e.g RDI for printDouble
  | Pop Type Value
  | Push Type Value
  | IncDec AddOp Value
  | CNeg Value
    deriving (Show, Eq)







----------------------------------------------------------------------- interract with state

-- update or add argument from previous calculation
setPrevVal :: (Value, Type) -> Bool -> Compile ()
setPrevVal v False = do 
  vs <- gets prevResult
  if (length vs == 0)
    then modify $ \st@St { prevResult = vs } -> st { prevResult = [v] }
    else modify $ \st@St { prevResult = vs } -> st { prevResult = v : (tail vs) }
setPrevVal v True  = modify $ \st@St { prevResult = vs } -> st { prevResult = v : vs }


-- get value or register from previous statement
getPrevResult :: Compile (Value, Type)
getPrevResult = do
  allArgs <- gets prevResult
  return $ head allArgs



-- load register (if it's variable, then load)
loadReg :: (Value, Type) -> Compile Value ---------- todo, var = reg ptr, temp = reg lit
--loadReg (Reg r, Ptr ( t)) = do 
  --r' <- newRegister ( t)
  --emit $ Load r' ( t) r
  --return (Reg r')
loadReg (X86 x, _) = return $ X86 x
loadReg (Reg r, _) = return $ Reg r -- todo delete this? might do cmp [rsp + x] [rsp+y]? no cuz we do that check in emitbinarop
-- literals cant be the first operand of add/cmp ... so mov them to EAX
loadReg (v,  Doub) = do
  emit $ Mov ( Doub) v (X86 XMM0) -- todo needed?
  return (X86  XMM0)
loadReg (v, t) = do
  emit $ Mov t v (X86 RAX)
  return (X86 RAX)
--loadReg (r,  t) = return r


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
newRegister :: Type -> Compile Register
newRegister t = do
  v <- gets nextReg
  modify $ \st -> st { nextReg = succ v }
  return v


-- create the next register name
newParam :: Type -> Compile Param
newParam t = do
  (P v) <- gets nextPar
  modify $ \st -> st { nextPar = (P (v + fromIntegral (size t))) }
  return (P v)

-- calculate register size
size :: Type -> Integer
size ( Doub) = 8
size ( _) = 8

-- add new variable to the state
newVar :: Ident -> Value -> Type -> Compile ()
newVar x r t = modify $ \st@St { cxt = (b : bs) } -> st { cxt = ((x, r, t) : b) : bs }


-- get type and register for a variable
lookupVar :: Ident -> Compile (Value, Type)
lookupVar id = do 
  c <- gets cxt
  return (fromJust $ cxtContains id c)
  where

    -- context contains var?
    cxtContains :: Ident -> [[(Ident, Value, Type)]] -> Maybe (Value, Type)
    cxtContains id []     = Nothing
    cxtContains id (b:bs) = do 
      let firstSearch = contains id b 
      if (isNothing firstSearch )
        then cxtContains id bs
        else firstSearch
      where

        -- block containts var?
        contains :: Ident -> [(Ident, Value, Type)] -> Maybe (Value, Type)
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
impossible a = error $ "impossible code " ++ toX86 a



-------------------------------------------------- convert code


class ToX86 a where
    toX86 :: a -> String


instance ToX86 Type where
  toX86 t = case t of
    Int    -> "i32"
    Void   -> "void"
    Bool   -> "i1"
    Doub   -> "double"
    String -> "i8*"

-- add proper prefix to mulop
prefixMulOp :: Type -> MulOp -> String
prefixMulOp ( Int) Times  = "imul"
prefixMulOp ( Doub) Times = "mulsd"


instance ToX86 AddOp where
  toX86 op = case op of
    Plus  -> "add"
    Minus -> "sub"

-- add proper prefix to relop
prefixRelOp :: Type -> RelOp -> String
prefixRelOp ( Doub) op                         = "o" ++ toX86 op
prefixRelOp ( Int)  op | op == EQU || op == NE =        toX86 op
prefixRelOp ( Int)  op                         = "s" ++ toX86 op
prefixRelOp ( Bool) op                         = prefixRelOp ( Int) op

instance ToX86 RelOp where
  toX86 op = case op of
    LTH   -> "l"
    GTH   -> "g"
    LE    -> "le"
    GE    -> "ge"
    EQU   -> "e"
    NE    -> "ne"

instance ToX86 Index where
  toX86 (I (i:[])) = toX86 ( Int) ++ " " ++ show i
  toX86 (I (i:is)) = toX86 ( Int) ++ " " ++ show i ++ ", " ++ toX86 (I is)

instance ToX86 FunHead where
  toX86 (FunHead (Ident f) (FunType t ts)) = "define " ++ toX86 t ++ " @" ++ f ++ "(" ++ ( reverse ( drop 2 ( reverse (((\t -> t ++ ", ") . toX86) =<< ts)))) ++ ")"

instance ToX86 Label where
  toX86 (L l) = "L" ++ show l

instance ToX86 Value where
  toX86 = \case
    LitInt i      -> show i
    LitDoub d     -> "__?float64?__(" ++ show d ++ ")"
    LitBool True  -> "1" 
    LitBool False -> "0" 
    LitString s   -> error $ "can only print adress to string, not string directly"
    Reg r         -> toX86 r
    Glob g        -> toX86 g 
    X86 (Locals 0) -> "[rbp]"
    X86 (Locals n) -> "[rbp - " ++ show n ++ "]"
    X86 (Stack  0) -> "[rsp]"
    X86 (Stack  n) -> "[rsp - " ++ show n ++ "]"
    X86 (Param  n) -> "[rbp + " ++ show n ++ "]"
    X86 reg       -> (map (\c -> toLower c) (show reg))
    
instance ToX86 Register where
  toX86 (R r) = "________RRRR" ++ show r

instance ToX86 GlobalRegister where
  toX86 (G g) = "str" ++ show g

instance ToX86 Arguments where
  toX86 (Args [])          = ""
  toX86 (Args [(t, v)])    = toX86 t ++ " " ++ toX86 v
  toX86 (Args ((t, v):as)) = toX86 t ++ " " ++ toX86 v ++ ", " ++ toX86 (Args as)

-- print string in array form
stringType :: String -> String
stringType s = "[" ++ show ( (length s) + 1) ++ " x i8]"


instructionIncDec :: AddOp -> String
instructionIncDec Plus = "inc"
instructionIncDec Minus = "dec"

sizeKeyword :: Type -> Value -> String
sizeKeyword t x = case x of
  (X86 (Locals n)) -> prefixT t ++  "word"
  (X86 (Param n)) -> prefixT t ++  "word"
  (X86 (Stack n)) -> prefixT t ++  "word"
  _               -> ""
  where
    prefixT ( Doub) = "q"
    prefixT _ = "q"

instance ToX86 Code where
  toX86 = \case
    Return                         -> "ret"
    Call t (Ident f)               -> "call  " ++ f
    Global adr t (LitString s)     -> toX86 adr ++ " db \"" ++ s ++ "\", 0"
    Label l                        -> toX86 l ++ ":"
    Branch lb                      -> "jmp   " ++ toX86 lb
    BranchCond op lb               -> "j" ++ toX86 op ++ "    " ++ toX86 lb
    Cmp t v1 v2 | t ==  Doub       -> "comisd " ++ (sizeKeyword t v1) ++ ""    ++ toX86 v1 ++ ", " ++ toX86 v2
                | otherwise        -> "cmp  " ++   (sizeKeyword t v1) ++ " "   ++ toX86 v1 ++ ", " ++ toX86 v2
    Add op t v1 v2 | t ==  Int     -> toX86 op                        ++ "   " ++ toX86 v1 ++ ", " ++ toX86 v2
                   | t ==  Doub    -> toX86 op                        ++ "sd " ++ toX86 v1 ++ ", " ++ toX86 v2
    Mul op t v1 v2                 -> (prefixMulOp t op)              ++ " "   ++ toX86 v1 ++ ", " ++ toX86 v2
    DivD v1 v2                     -> "divsd " ++ toX86 v1 ++ ", " ++ toX86 v2
    DivI v                         -> "div   " ++ toX86 v
    Mov t v1 v2 | t == ( Doub)     -> "movsd " ++ toX86 v2 ++ ", " ++ toX86 v1
                | otherwise        -> "mov   " ++ toX86 v2 ++ ", " ++ toX86 v1
    -- mov __?float62?__(x.y) to RAX
    MovF  v1 v2                    -> "mov   " ++ toX86 v2 ++ ", " ++ toX86 v1
    -- mov RAX to xmm
    MovF2 (X86 (Locals v1)) v2     -> "mov   " ++ toX86 v2 ++ ", " ++ toX86 (X86 (Locals v1))
    MovF2 (X86 (Param  v1)) v2     -> "mov   " ++ toX86 v2 ++ ", " ++ toX86 (X86 (Param v1))
    MovF2 v1 v2                    -> "movq  " ++ toX86 v2 ++ ", " ++ toX86 v1
    -- mov from xmm to e.g RDI
    MovF3 v1 v2                    -> "movq  "   ++ toX86 v2 ++ ", " ++ toX86 v1
    Pop t v                        -> "pop     " ++ toX86 v
    Push t v | t ==  Doub          -> "push   "  ++ toX86 v -- todo not in use
             | t ==  String        -> "push qword ["  ++ toX86 v ++ "]" -- todo used to be word
             | otherwise           -> "push   " ++ (sizeKeyword t v)  ++ " " ++ toX86 v 
    IncDec op v                    -> instructionIncDec op ++ " " ++ toX86 v
    CNeg v                         -> "neg  " ++ toX86 v
    Comment ""                     -> ""
    Comment s                      -> "; " ++ s
    c                              -> show c








---------------------------------------------------------------------------------- emit code



-- add llvm code line to output
emit :: Code -> Compile ()
emit c = modify $ \st@St { output = cs } -> st { output = c : cs }

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

-- replace nth element in array
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

-- count nbr of True elements
boolSum :: [Bool] -> Int
boolSum ds = sum (map (\x -> if (x)
                              then 1
                              else 0) ds)


-- get element in list at given index
getElem :: Int -> [a] -> a 
getElem i l = last $ take (i+1) l







---------------------------------------------------------------------------------- Register allocation optimization







-- "main" function for register allocation optimization
registerAlloc :: Compile ()
registerAlloc = do
  --------------------------------- perform register allocation ------------------------------
  -- get def, use and succ sets
  (d, iu, du, s) <- defUseSucc
  --error (show $ map (\x -> map (\(b,i) -> if (b) then show i ++ show i else "") (zip x [0..])) du)
  -- calculate liveness
  li             <- liveness (d, iu, s) False
  ld             <- liveness (d, du, s) True
  -- todo : only the second liveness is slow
  -- get interference graph
  let ii         = interference li
  let id         = interference ld
  -- real registers
  let iRegs       = [RSI, R8, R9, R10, R11, R12, R13, R14, R15] -- rdi rax rbx rcx
  let dRegs       = [XMM2, XMM3, XMM4, XMM5, XMM6, XMM7, XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15]
  -- nbr of real-regs
  let ki          = length iRegs
  let kd          = length dRegs
  -- k color to get real-reg and stack-var partitions + color for each real-reg
  (ri, si, ci)   <- kColor ii ki
  (rd, sd, cd)   <- kColor id kd
  -- update code with real-regs
  registerOutput ( Int)  iRegs ri ci
  registerOutput ( Doub) dRegs rd cd
  -- space needed for spilled stack variables 
  let  intLocals  = 8*(length si)
  let doubLocals  = 8*(length sd)
  -- local var locations
  let iStack      = map (\(_, y) -> Locals (y*8)) (zip si [1..]) -- todo fix offset
  let dStack      = map (\(_, y) -> Locals (y*8 + intLocals)) (zip sd [1..]) -- todo fix offset
  -- make map that says which memory location each spill register maps to
  (R n_regs)     <- gets nextReg
  let sli         = makeSpillColorMap (length si) 0 n_regs si (take n_regs (repeat (bitArray (length si))))
  let sld         = makeSpillColorMap (length sd) 0 n_regs sd (take n_regs (repeat (bitArray (length sd))))
  -- update code with stack registers for spilled vars
  registerOutput ( Int)  iStack si sli
  registerOutput ( Doub) dStack sd sld





  --------------------- align stack if we created stack variables --------------------------
  o             <- gets output
  let (o1, o2)   = splitAt 2 (reverse o)
  let localSize  = intLocals + doubLocals
  -- align stack if not 128 bit aligned
  (P p)         <- gets nextPar
  let pushRbp    = 8
  let returnAdr  = 8
  let aligned    = (localSize + p + pushRbp + returnAdr) `mod` 16 == 0
  let align      = if (aligned)     
                    then                             o2
                    else  (Push ( Int) (X86 RCX)):o2





  ------------------- decrement RSP to make space for stack variables ------------------
  -- make space for local variables
  let prep       = if (localSize > 0)
                    then (Add Minus ( Int) (X86 RSP) (LitInt (toInteger $ localSize))):align
                    else                                                                  align
  let o' = reverse (o1++prep)
  -- pop RCX (if we had to align)
  let o'' = if (aligned)
              then o'
              else alignBeforeRet o'
  modify $ \st -> st { output = o''}









-- map each spill register to a memory location
makeSpillColorMap 
  :: Int      -- n memory locations needed
  -> Int      -- current index
  -> Int      -- total nbr of regs
  -> [Int]    -- registers to be mapped
  -> [[Bool]] -- current Color map
  -> [[Bool]] -- updated Color map
makeSpillColorMap _ _ _ []         colorMap = colorMap
makeSpillColorMap n i r (s:ss) colorMap = do
  -- there are n possible colors (== nbr of spill vars)
  let none      = take n (repeat False)
  -- take the next color (memory location)
  let color     = replaceNth i True none
  -- give spill register s this color
  let colorMap' = replaceNth s color colorMap
  -- give colors to the rest of the pill registers
  makeSpillColorMap n (i+1) r ss colorMap'



-- Pop RCX before ret instructions to remove alignment
alignBeforeRet 
  :: [Code] -- remaining code to align
  -> [Code] -- aligned code
alignBeforeRet [] = []
alignBeforeRet (c:cs) = do
  let cs' = if (c == Return)
              then do 
                let (cs1, cs2) = splitAt 2 cs
                cs1 ++ ((Pop ( Int) (X86 RCX)):cs2)
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
      (DivD     v1   v2) -> combine ( Doub) v1 v2
      (DivI     v      ) -> combine ( Int)  v  (LitInt (-1)) -- -1 is dummy value
      (MovF2     v1   v2) -> combine ( Doub) v2 v1 --(LitDoub (-1.0))
      (MovF3    v1    _) -> combine ( Doub) v1 (LitDoub (-1.0))
      (Pop    t       v) -> combine t          v  (LitInt (-1)) 
      (Push   t       v) -> combine t          v  (LitInt (-1)) 
      (IncDec _       v) -> combine ( Int)  v  (LitInt (-1)) 
      (CNeg           v) -> combine ( Int)  v  (LitInt (-1))
      _                  -> combine ( Int)     (LitInt (-1)) (LitInt (-1))
      where
      

      -- get the use set for 2 values
      combine 
        :: Type                 -- type of the values
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
            :: Type         -- type of this register
            -> [Bool]           -- use set
            -> ([Bool], [Bool]) -- use set of ints and doubles
          partitionType ( Doub) b =    (take (length b) (repeat False), b)
          partitionType _          b = (b, take (length b) (repeat False))





-- create a list with the liveness of each virtual register in the current function
-- element n corresponds to n:th instruction, each element is a list of live registers
-- todo make sure u do from backwards to top
liveness 
  :: ([[Bool]], [[Bool]], [[Bool]]) -- def, use and succ- sets
  -> Bool
  -> Compile [[Bool]]               -- liveness for each line
liveness dus b = do
  o           <- gets output
  --if(head o == Comment "") then error (show "asdad") else return ()
  let n_ins    = length o
  (R n_regs)  <- gets nextReg
  --let n_regs = toInteger 14
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
  let (stack', matrix', spill) = removeSpill stack matrix
  -- an empty k-bit-map representation of color choice for each register
  let colors            = take n_regs (repeat (take k (repeat False)))
  let coloring          = color stack' matrix' colors
  --if (k == 9) then error (show stack) else return ()
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
      if (r == 0)
        then []
        else do
          -- find node n and delete edges to get new graph m'
          let (m', n, _) = findN m (-1) deleted m
          let deleted' = replaceNth (fst n) True deleted
          -- number of nodes left -1
          let r' = r - 1 -- todo r redundant with deleted
          -- get the rest of the nodes
          let stack' = repeatAlg m' r' deleted'
          -- combine results
          if (False && fst n == 1 && snd n && or ( map (\s -> (fst s == 1  && snd s)) stack'))
            then error (show stack')
            else return ()
          (n:stack')

      
    -- find node n with <k egdges, also delete edges
    -- spill node with most interference
    -- int argument is the max degree discovered
    -- result: 
    -- graph with deleted edges, 
    -- the edge with a spill flag and 
    -- degree of node we need to spill
    findN 
      :: [[Bool]]                           -- remaining interference graph to explore
      -> Int                                -- maximum degree found
      -> [Bool]                             -- deleted nodes
      -> [[Bool]]                           -- entire interference graph
      -> ([[Bool]], (Int, Bool), Maybe Int) -- interference graph with n removed, n with spill flag, and maximum degree found if we need to spill
    -- look for node to spill
    findN []     degree _         m0 = (m0, (-1, False), Just degree)
    -- look for node with <k edges
    findN (b:bs) degree deleted'' m0 = do
      let degree' = boolSum b
      let index   = (length matrix) - (length bs) - 1
      let del = isDeleted bs deleted''
      -- if degree' < k, delete this node, otherwise check the next node
      if (degree' < k && not del)
        then do
          let matrix'' = deleteNode index m0--matrix
          (matrix'', (index, False), Nothing)
        else do 
          -- update max degree (for spill prioritization)
          let degree'' = if (degree' >= degree)
                          then degree'
                          else degree
          -- try delete other node
          let (matrix''', (b', flag), degree''') = findN bs degree'' deleted'' m0
          -- if we deleted, then return, else see if we should spill the current node
          if (isNothing degree''') -- todo maybe entire result use when?
            then (matrix''', (b', flag), degree''')
            else do
              -- if this node had the highest degree, then spill this, otherwise ask previous node to spill
              if (fromJust degree''' == degree')
                then do
                  let matrix'''' = deleteNode index m0--matrix -- aaaaaaaaaaa shouldnt just be mtrix?
                  --error (show matrix'''')
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
  :: Type  -- int or double register
  -> [X86Reg]  -- available real registers/memory
  -> [Int]     -- temp registers to be allocated
  -> [[Bool]]  -- which color each temp register was given
  -> Compile ()
registerOutput typ realRegs tempRegs colorMap = do
  --for each register in stack, replace it in the generated code
  traverseStack tempRegs
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
          :: (Int, [Code]) -- temp register and current code to update
          -> [Code]        -- code with the temp register allocated its real register / memory
        traverseCode x = case x of
          (s, []) -> []
          (s, (c:cc)) -> do
            let c'  = updateCode c (R s)
            let cc' = traverseCode (s, cc)
            --if (head realRegs == Locals 8 && c' == Return)
              --then error (show s ++ show cc')
              --else return ()
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
                (MovF2    v1 v2) -> (MovF2     (swap ( Doub) v1 r)                    (swap ( Doub) v2 r)) -- v1    swap v2
                (MovF3    v1 v2) -> (MovF3     (swap ( Doub) v1 r) v2)
                (DivD     v1 v2) -> (DivD      (swap ( Doub) v1 r) (swap ( Doub) v2 r))
                (DivI     v    ) -> (DivI      (swap ( Int)  v  r)) 
                (Pop    t     v) -> (Pop    t  (swap t          v  r))
                (Push   t     v) -> (Push   t  (swap t          v  r))
                (IncDec op    v) -> (IncDec op (swap ( Int)  v  r))
                (CNeg         v) -> (CNeg      (swap ( Int)  v  r))
                c -> c
                where

                  -- swap temp-reg with real reg if its the correct one
                  swap 
                    :: Type     -- register type 
                    -> Value    -- value to update
                    -> Register -- register to update
                    -> Value    -- updated value
                  swap t v r = do
                    -- only update if its the correct type
                    if ((typ ==  Doub && t ==  Doub) || (typ ==  Int && not (t ==  Doub) ))
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
                          else getElem (fromJust index) realRegs




















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
  -- print the function header
  let func  = intercalate "" [f, ":\n", unlines $ map (indent . toX86) $ reverse $ (output st'), "\n"]
  -- print the string constants
  let glob  = unlines $ map toX86 $ reverse (globalOut st')
  (func, glob, st')
  -- compile the function
  where st' = execState (compileFun (Ident f) t args ss) st


-- compile function helper
compileFun :: Ident -> Type -> [Arg] -> [Stmt] -> Compile ()
compileFun (Ident f) t0 args ss = do
  -- reset code output and register -- todo reset block too? dont care about var from other function
  modify $ \st -> st { output = []}
  modify $ \st -> st { globalOut = []}
  modify $ \st -> st { nextReg = R 0}
  modify $ \st -> st { nextPar = P 16}
  -- make a new variable and alloc memory for each parameter:
  regs <- mapM (\(Argument t' x) -> do
                                        (P r') <- newParam ( t')
                                        newVar x (X86 (Param r'))  ( t')
                                        return (P r')
                                      ) (reverse args)
  -- push registers
  emit $ Push ( Int) (X86 RBP)
  emit $ Mov ( Int) (X86 RSP) (X86 RBP)
  compileStms ss
  -- add "ret void" if no return statement at the end
  if (t0 == Void)
    then do 
      prevStm <- gets output
      if (length prevStm /= 0 && (head $ words $ toX86 $ head prevStm) == "ret")
        then return ()
        else do
          -- pop registers
          emit $ Mov ( Int) (X86 RBP) (X86 RSP)
          emit $ Pop ( Int) (X86 RBP)
          emit $ Return
    else return ()
  -- optimize using register allocation
  registerAlloc











---------------------------------------------------------------------------------- compile statements



-- help function for compiling variable declaration
compileDecl :: Type -> Item -> Compile ()
compileDecl t (Init id (ETyped e _)) = do
  -- compile expression and make new variable
  compileExp (ETyped e t) False
  r <- newRegister ( t)
  newVar id (Reg r) ( t)
  (p, t')  <- getPrevResult
  emit $ Mov ( t) p (Reg r)

compileDecl t (NoInit id) = do
  -- create new variable and use default value
  r <- newRegister ( t)
  newVar id (Reg r) ( t)
  default0 r t
  where


    -- assign default value to register for a given type
    default0 :: Register -> Type -> Compile ()
    default0 r Doub = do
      emit $ MovF  (LitDoub 0.0) (X86 RAX)
      emit $ MovF2 (X86 RAX) (Reg r)
    default0 r Int = emit $ Mov ( t) (LitInt 0) (Reg r)  
    default0 r Bool = emit $ Mov ( t) (LitBool False) (Reg r)  




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
fixReturnReg Doub v = emit $ Mov ( Doub) v (X86 XMM0)
fixReturnReg t (X86 RAX) = return ()
fixReturnReg t v = emit $ Mov ( t) v (X86 RAX)

-- compile statement
-- Bool: does this statement guarantee a return statement?
compileStm :: Stmt -> Compile Bool
compileStm (Retting s0 ret) = do
  case s0 of


    Ret e@(ETyped _ t) -> do
      compileExp e False
      (r, t0)  <- getPrevResult
      fixReturnReg t r
      -- pop registers
      emit $ Mov ( Int) (X86 RBP) (X86 RSP) 
      emit $ Pop ( Int) (X86 RBP)
      emit $ Return
      return True


    VRet -> do
      -- pop registers
      emit $ Mov ( Int) (X86 RBP) (X86 RSP)
      emit $ Pop ( Int) (X86 RBP)
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
          emit $ Cmp ( typ) r' (LitBool True)
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
            emit $ Cmp ( Bool) r' (LitBool True)
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
          emit $ Cmp ( typ) r' (LitBool True)
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
      -- r'     <- loadReg r
      emit $ Mov ( typ) r a
      --emit $ Store ( typ) r' a
      return False


    Incr i -> incDecr i Plus
    Decr i -> incDecr i Minus


    Empty -> return False


    s -> error $ "not implemented compileStm " ++ printTree s



-- helper for incrementing / decrementing statement
incDecr :: Ident -> AddOp -> Compile Bool
incDecr i op = do
  (adr, t) <- lookupVar i
  -- only int has direct inc / dec instruction
  if (t == ( Int) )
    then do 
      emit $ IncDec op adr
      return False
    else do
      emit $ Add op ( Doub) (LitDoub 1.0) adr
      return False
     




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
  -- compile arg 1
  compileExp e1 True
  (arg1Temp, t1) <- getPrevResult
  arg1 <- newRegister t1
  emit $ Mov t1 arg1Temp (Reg arg1)
  -- compile arg2
  compileExp e2 True
  (arg2, t2) <- getPrevResult 
  -- mov the second operand away from RAX / XMM0
  arg2' <- if (arg2 ==  defaultReg 0 t)
            then do
              r2 <- newRegister ( t)  -- todo too late, we have already overriden arg1
              emit $ Mov ( t) arg2 (Reg r2) -- rbx
              return (Reg r2)
            else return arg2
  -- mov first operand to RAX / XMM0
  emit $ Mov ( t) (Reg arg1) (defaultReg 0 t)  -- rax
  -- create result register
  --r               <- newRegister ( t)
  -- compile and remove arguments
  case op' of
    Ao op -> emit $ Add  op ( t) (defaultReg 0 t)  arg2'
    Mo Mod -> modulo arg2'
    Mo Div -> divide t arg2'
    Mo op  -> emit $ Mul  op ( t) (defaultReg 0 t) arg2'
    Ro op  -> compare     op ( t) (defaultReg 0 t) arg2'
  removeArgs 2
  -- compare will always return bool
  case op' of 
    Ro _   -> setPrevVal ((defaultReg 0 Bool), ( Bool)) b
    Mo Mod -> setPrevVal ((X86 RDX),         ( Int)) b
    _      -> setPrevVal ((defaultReg 0 t   ), ( t)) b

  where


    -- divide
    divide :: Type -> Value -> Compile ()
    divide Doub arg2 = emit $ DivD (defaultReg 0 Doub) arg2
    divide Int  arg2 = emit $ DivI arg2

    -- modulo operation
    modulo :: Value -> Compile ()
    modulo arg2 = do
      emit $ Mov ( Int) (LitInt 0) (X86 RDX)
      emit $ DivI arg2

    -- emit instructions for comparison
    compare ::  RelOp -> Type -> Value -> Value -> Compile ()
    compare  op typ arg1 arg2 = do
      emit $ Cmp typ arg1 arg2
      t <- newLabel
      f <- newLabel
      e <- newLabel
      emit $ BranchCond op t
      emit $ Branch f
      -- true
      emit $ Label t
      emit $ Mov ( Bool) (LitBool True) (X86 RAX)
      emit $ Branch e
      -- false
      emit $ Label f
      emit $ Mov ( Bool) (LitBool False) (X86 RAX)
      emit $ Branch e
      -- end
      emit $ Label e



-- compile expression
-- Bool: this expr is an argument to a function, 
-- the bool is needed for knowing whether or not to save or override the previous result in the state 
compileExp :: Expr -> Bool -> Compile ()
compileExp e0 b = case e0 of


  ELitInt i  -> do 
    r <- newRegister ( Int)
    emit $ Mov ( Int) (LitInt i) (Reg r)
    setPrevVal (Reg r,  Int)  b
  ELitDoub d -> do
    r2 <- newRegister ( Doub)
    emit $ MovF  (LitDoub d) (X86 RAX)
    emit $ MovF2 (X86 RAX) (Reg r2)
    setPrevVal (Reg r2,  Doub)  b
  ELitTrue   -> do
    r <- newRegister ( Bool)
    emit $ Mov ( Bool) (LitBool True) (Reg r)
    setPrevVal (Reg r,  Bool)  b
  ELitFalse   -> do
    r <- newRegister ( Bool)
    emit $ Mov ( Bool) (LitBool False) (Reg r)
    setPrevVal (Reg r,  Bool)  b


  EString s  -> do
    adr  <- newGlobalRegister
    emitGlobal $ Global adr ( String) (LitString s)
    setPrevVal (Glob adr,  String) b


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
    let bytes   = sum (map (\t -> size ( t)) ts)
    let aligned = bytes `mod` 16 == 0
    if ((not isExtern) && (not aligned))
      then emit $ Push ( Int) (X86 RCX)
      else return ()

    -- save registers
    if (True || isExtern)
      then do
        emit $ Push ( Int) (X86 RBX) -- push 1 extra to 128 bit align
        emit $ Push ( Int) (X86 RBX)
        emit $ Push ( Int) (X86 RDI)
        emit $ Push ( Int) (X86 RSI)
        emit $ Push ( Int) (X86 RDX)
        emit $ Push ( Int) (X86 RCX)
        emit $ Push ( Int) (X86 R8)
        emit $ Push ( Int) (X86 R9)
        emit $ Push ( Int) (X86 R10)
        emit $ Push ( Int) (X86 R11) -- [XMM2, XMM3, XMM4, XMM5, XMM6, XMM7, XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15]

        emit $ Add Minus (Int) (X86 RSP) (LitInt 8) 
        emit $ Mov Doub (X86 XMM2) (X86 (Stack 0))

        emit $ Add Minus (Int) (X86 RSP) (LitInt 8) 
        emit $ Mov Doub (X86 XMM3) (X86 (Stack 0))

        emit $ Add Minus (Int) (X86 RSP) (LitInt 8) 
        emit $ Mov Doub (X86 XMM4) (X86 (Stack 0))


        emit $ Add Minus (Int) (X86 RSP) (LitInt 8) 
        emit $ Mov Doub (X86 XMM5) (X86 (Stack 0))
        {-

        emit $ Add Minus (Int) (X86 RSP) (LitInt 8) 
        emit $ Mov Doub (X86 XMM6) (X86 (Stack 0))

        emit $ Add Minus (Int) (X86 RSP) (LitInt 8) 
        emit $ Mov Doub (X86 XMM7) (X86 (Stack 0))

        emit $ Add Minus (Int) (X86 RSP) (LitInt 8) 
        emit $ Mov Doub (X86 XMM8) (X86 (Stack 0))

        emit $ Add Minus (Int) (X86 RSP) (LitInt 8) 
        emit $ Mov Doub (X86 XMM9) (X86 (Stack 0))

        emit $ Add Minus (Int) (X86 RSP) (LitInt 8) 
        emit $ Mov Doub (X86 XMM10) (X86 (Stack 0))

        emit $ Add Minus (Int) (X86 RSP) (LitInt 8) 
        emit $ Mov Doub (X86 XMM11) (X86 (Stack 0))

        emit $ Add Minus (Int) (X86 RSP) (LitInt 8) 
        emit $ Mov Doub (X86 XMM12) (X86 (Stack 0))

        emit $ Add Minus (Int) (X86 RSP) (LitInt 8) 
        emit $ Mov Doub (X86 XMM13) (X86 (Stack 0))

        emit $ Add Minus (Int) (X86 RSP) (LitInt 8) 
        emit $ Mov Doub (X86 XMM14) (X86 (Stack 0))

        emit $ Add Minus (Int) (X86 RSP) (LitInt 8) 
        emit $ Mov Doub (X86 XMM15) (X86 (Stack 0))
        -}
      else return ()
    -- compile arguments and and make sure they dont override each other
    mapM_ (\e -> do
                  compileExp e False -- todo used to be true
                  (prev, typ) <- getPrevResult
                  -- move argument to rdi
                  if (isExtern)
                    then if (id == Ident "printDouble")
                          then emit $ Mov     Doub prev (X86 XMM0)
                          else emit $ Mov (Int) prev (X86 RDI) -- use same calling convention as runtime
                    
                    else if (typ == Doub)
                      then do -- "push" doub manually
                        emit $ Add Minus ( Int) (X86 RSP) (LitInt 8) 
                        emit $ Mov typ prev (X86 (Stack 0))
                      else do
                        emit $ Push typ prev) es
    -- call function
    emit $ Call ( t) id
    -- remove arguments
    if (not isExtern)
      then emit $ Add Plus ( Int) (X86 RSP) (LitInt $ sum $ map (\x -> size ( x)) ts)
      else return ()
    -- restore registers

    {-

    emit $ Mov (Doub) (X86 (Stack 0)) (X86 XMM15)
    emit $ Add Plus (Int) (X86 RSP) (LitInt 8)

    emit $ Mov (Doub) (X86 (Stack 0)) (X86 XMM14)
    emit $ Add Plus (Int) (X86 RSP) (LitInt 8)

    emit $ Mov (Doub) (X86 (Stack 0)) (X86 XMM13)
    emit $ Add Plus (Int) (X86 RSP) (LitInt 8)

    emit $ Mov (Doub) (X86 (Stack 0)) (X86 XMM12)
    emit $ Add Plus (Int) (X86 RSP) (LitInt 8)

    emit $ Mov (Doub) (X86 (Stack 0)) (X86 XMM11)
    emit $ Add Plus (Int) (X86 RSP) (LitInt 8)

    emit $ Mov (Doub) (X86 (Stack 0)) (X86 XMM10)
    emit $ Add Plus (Int) (X86 RSP) (LitInt 8)

    emit $ Mov (Doub) (X86 (Stack 0)) (X86 XMM9)
    emit $ Add Plus (Int) (X86 RSP) (LitInt 8)

    emit $ Mov (Doub) (X86 (Stack 0)) (X86 XMM8)
    emit $ Add Plus (Int) (X86 RSP) (LitInt 8)

    emit $ Mov (Doub) (X86 (Stack 0)) (X86 XMM7)
    emit $ Add Plus (Int) (X86 RSP) (LitInt 8)

    emit $ Mov (Doub) (X86 (Stack 0)) (X86 XMM6)
    emit $ Add Plus (Int) (X86 RSP) (LitInt 8)
    -}

    emit $ Mov (Doub) (X86 (Stack 0)) (X86 XMM5)
    emit $ Add Plus (Int) (X86 RSP) (LitInt 8)

    emit $ Mov (Doub) (X86 (Stack 0)) (X86 XMM4)
    emit $ Add Plus (Int) (X86 RSP) (LitInt 8)

    emit $ Mov (Doub) (X86 (Stack 0)) (X86 XMM3)
    emit $ Add Plus (Int) (X86 RSP) (LitInt 8)

    emit $ Mov (Doub) (X86 (Stack 0)) (X86 XMM2)
    emit $ Add Plus (Int) (X86 RSP) (LitInt 8)

    emit $ Pop ( Int) (X86 R11)
    emit $ Pop ( Int) (X86 R10)
    emit $ Pop ( Int) (X86 R9)
    emit $ Pop ( Int) (X86 R8)
    emit $ Pop ( Int) (X86 RCX)
    emit $ Pop ( Int) (X86 RDX)
    emit $ Pop ( Int) (X86 RSI)
    emit $ Pop ( Int) (X86 RDI)
    emit $ Pop ( Int) (X86 RBX)
    emit $ Pop ( Int) (X86 RBX)
    -- remove alignment
    if ((not isExtern) && not aligned)
      then emit $ Pop ( Int) (X86 RCX)
      else return ()
    -- set return value
    if (t == Doub)
      then setPrevVal ((X86 XMM0), ( t)) b
      else setPrevVal ((X86 RAX ), ( t)) b


  ETyped (EAdd e1 op e2) t              -> emitBinaryOp t (Ao op) e1 e2 b
  ETyped (EMul e1 op e2) t              -> emitBinaryOp t (Mo op) e1 e2 b
  ETyped (ERel e1@(ETyped _ t) op e2) _ -> emitBinaryOp t (Ro op) e1 e2 b


  EAnd e1 e2 -> do
    -- e1 true?
    compileExp e1 True
    (e1_result, t1) <- getPrevResult
    t               <- newLabel
    f               <- newLabel
    -- if e1 true, then compile e2, otherwise skip (lazy eval)
    r'    <- loadReg (e1_result, t1)
    emit $ Cmp ( Bool) r' (LitBool True)
    emit $ BranchCond EQU t
    emit $ Branch f

    -- e2 true?
    emit $ Label t 
    compileExp e2 False -- it is ok to overwrite e1_result
    (e2_result, typ2) <- getPrevResult
    t2        <- newLabel
    -- if e2 true, emit true, otherwise false
    e2_result'    <- loadReg (e2_result, typ2)
    emit $ Cmp ( Bool) e2_result' (LitBool True)
    emit $ BranchCond EQU t2
    emit $ Branch f

    -- emit true
    emit $ Label t2
    emit $ Mov ( Bool) (LitBool True) (X86 RAX)
    end <- newLabel
    emit $ Branch end

    -- emit false
    emit $ Label f
    emit $ Mov ( Bool) (LitBool False) (X86 RAX)
    emit $ Branch end

    -- end
    emit $ Label end
    removeArgs 1
    setPrevVal (X86 RAX, ( Bool)) b


  EOr e1 e2 -> do
    -- e1 true?
    compileExp e1 True
    (e1_result, t1) <- getPrevResult
    t         <- newLabel
    f         <- newLabel
    -- if e1 true, then emit true, otherwise check e2
    e1_result'    <- loadReg (e1_result, t1)
    emit $ Cmp ( Bool) e1_result' (LitBool True)
    emit $ BranchCond EQU t
    emit $ Branch f

    -- e2 true?
    emit $ Label f 
    compileExp e2 False -- ok to overwrite e1_result
    (e2_result, t2) <- getPrevResult
    f2        <- newLabel
    -- if e2 true, then emit true, otherwise emit false
    e2_result'    <- loadReg (e2_result, t2)
    emit $ Cmp ( Bool) e2_result' (LitBool True)
    emit $ BranchCond EQU t
    emit $ Branch f2

    -- both were false
    emit $ Label f2
    emit $ Mov ( Bool) (LitBool False) (X86 RAX)
    end <- newLabel
    emit $ Branch end

    -- something was true
    emit $ Label t
    emit $ Mov ( Bool) (LitBool True) (X86 RAX)
    emit $ Branch end

    -- end
    emit $ Label end
    removeArgs 1
    setPrevVal (X86 RAX,  Bool) b


  Neg (ETyped e t) -> do
    if (t == Int)
      then do 
        compileExp (ETyped e t) b
        (i0, t') <- getPrevResult
        emit $ Mul Times ( Int) i0 (LitInt (-1))
      else
        case e of
          (ELitDoub d) -> do
            compileExp (ETyped e t) b
            (r', _)  <- getPrevResult
            compileExp (ELitDoub (-1.0)) b -- todo same b as setprevval rly?
            (r2', _) <- getPrevResult -- todo should be safe since the literal ends up in a temp reg
            emit $ Mul Times ( Doub) r'  r2'
            setPrevVal (r',  Doub) b
          _            -> compileExp (ETyped (EMul e Times (ELitDoub (-1.0)) ) t) b
    

  Not (ETyped e Bool) -> do
    case e of
      (ELitTrue)  -> setPrevVal (LitBool False,  Bool) b
      (ELitFalse) -> setPrevVal (LitBool True,  Bool) b
      _           -> do
        compileExp e True -- todo why not false? probably doesnt matter
        (r, t) <- getPrevResult
        -- mov value to RAX, because you cant to neg on [RBP + 8] for example
        -- we dont know which type of variable it is until after register allocation
        emit $ Mov ( Bool) r (X86 RAX)
        emit $ CNeg (X86 RAX)
        emit $ Mov ( Bool) (X86 RAX) r


  ETyped e _ -> compileExp e b


  e          -> error $ "not implemented compileexp " ++ show e