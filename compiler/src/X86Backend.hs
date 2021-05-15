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
  deriving(Show)

-- built in registers
data X86Reg
  = Stack Int | Param Int
  | RBP | RSP | RIP | RDI | RSI | RAX | RCX | RDX | RBX | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  | XMM0 | XMM1 | XMM2 | XMM3 | XMM4 | XMM5 | XMM6 | XMM7 | XMM8 | XMM9 | XMM10 | XMM11 | XMM12 | XMM13 | XMM15 
  deriving(Show, Eq)

-- jump flags
data JumpOp = EE | NEE | ZZ | GG | GEE | LL | LEE -- todo dont need ig, just delete
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
  -- | Compare Register RelOp LLVMType Value Value
  | Cmp LLVMType Value Value
  -- | Alloca Register LLVMType
  | Branch Label
  | BranchCond RelOp Label
  | Global GlobalRegister LLVMType Value
  -- | GetElementPointer Register String GlobalRegister Index
  | Comment String
  | Mov LLVMType Value Value -- todo RAX | ESP + 8         intermediate: kan vara (X86 RAX) eller (R 5)
  | Pop LLVMType Value
  | Push LLVMType Value
  | IncDec AddOp Value -- todo can only be reg ig
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


{-
-- load register (if it's variable, then load)
loadReg :: (Value, LLVMType) -> Compile Value ---------- todo, var = reg ptr, temp = reg lit
loadReg (Reg r, Ptr (Lit t)) = do 
  r' <- newRegister (Lit t)
  emit $ Load r' (Lit t) r
  return (Reg r')
loadReg (r, Lit t) = return r
-}

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
size (Ptr _) = 4
size (Lit Doub) = 8
size (Lit _) = 4

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
  toLLVM (I (i:[])) = toLLVM (Lit Int) ++ " " ++ show i
  toLLVM (I (i:is)) = toLLVM (Lit Int) ++ " " ++ show i ++ ", " ++ toLLVM (I is)

instance ToLLVM FunHead where
  toLLVM (FunHead (Ident f) (FunType t ts)) = "define " ++ toLLVM t ++ " @" ++ f ++ "(" ++ ( reverse ( drop 2 ( reverse (((\t -> t ++ ", ") . toLLVM) =<< ts)))) ++ ")"

instance ToLLVM Label where
  toLLVM (L l) = "L" ++ show l

instance ToLLVM Value where
  toLLVM = \case
    LitInt i      -> show i
    LitDoub d     -> show d
    LitBool True  -> "1" 
    LitBool False -> "0" 
    LitString s   -> error $ "can only print adress to string, not string directly"
    Reg r         -> toLLVM r
    Glob g        -> toLLVM g 
    X86 reg       -> (map (\c -> toLower c) (show reg))
    X86 (Stack n) -> "[rbp -" ++ show n ++ "]"
    -- todo arguments + 8 + 12 +16...
    
instance ToLLVM Register where
  toLLVM (R r) = "________r" ++ show r

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

instance ToLLVM Code where
  toLLVM = \case
    Return                           -> "ret"  -- ++ toLLVM t ++ " "  ++ toLLVM v
    Call t (Ident f)             -> "call " ++ f
    Label l                                -> toLLVM l ++ ":"
    Cmp t v1 v2 | t == Lit Doub -> "cmpsd"  ++ toLLVM v1 ++ ", " ++ toLLVM v2
                | otherwise     -> "cmp  "  ++ toLLVM v1 ++ ", " ++ toLLVM v2
    Add op t v1 v2 | t == Lit Int      -> toLLVM op         ++ " " ++ toLLVM v1 ++ ", " ++ toLLVM v2
                   | t == Lit Doub     -> toLLVM op        ++ "sd " ++ toLLVM v1 ++ ", " ++ toLLVM v2
    Mul op t v1 v2                     -> (prefixMulOp t op)      ++ " " ++ toLLVM v1 ++ ", " ++ toLLVM v2
    -- Alloca adr t                           -> toLLVM adr ++ " = alloca " ++ toLLVM t
    Branch lb                               -> "jmp " ++ toLLVM lb
    BranchCond op lb                 -> "j" ++ toLLVM op ++ " " ++ toLLVM lb
    Global adr t (LitString s)             -> toLLVM adr ++ " db \"" ++ s ++ "\""
   -- GetElementPointer r' s r i             -> toLLVM r' ++ " = getelementptr " ++ stringType s ++ ", " ++ stringType s ++ "* " ++ toLLVM r ++ ", " ++ toLLVM i
    Mov t v1 v2                       -> "mov " ++ toLLVM v2 ++ ", " ++ toLLVM v1
    Pop t v           -> "pop " ++ toLLVM v
    Push t v -> "push " ++ toLLVM v 
    IncDec op v -> instructionIncDec op ++ " " ++ toLLVM v
    CNeg v -> "neg " ++ toLLVM v
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
bitOr a b = map (\(c,d) -> c || d) (zip a b)

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


-- get element in list
getElem :: Int -> [a] -> a 
getElem i l = last $ take (i+1) l







---------------------------------------------------------------------------------- Register allocation optimization







-- "main" function for register allocation optimization
registerAlloc :: Compile ()
registerAlloc = do
  -- get def, use and succ sets
  (d, iu, du, s) <- defUseSucc
  --error (show d)
  --error ("_____" ++ (show d ))
  -- calculate liveness
  li             <- liveness (d, iu, s)
  ld             <- liveness (d, du, s)
  emit $ Comment (show li)
  -- get interference graph
  let ii         = interference li
  let id         = interference ld
  -- real registers
  let iRegs       = [RCX, RDX, RBX, RDI, RSI, R8, R9, R10, R11, R12, R13, R14, R15]
  let dRegs       = [XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7, XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM15]
  -- nbr of real-regs
  let ki          = length iRegs
  let kd          = length dRegs
  -- k color to get real-reg and stack-var partitions + color for each real-reg
  (ri, si, ci)   <- kColor ii ki
  (rd, sd, cd)   <- kColor id kd
  -- update code with real-regs
  registerOutput iRegs ri ci
  registerOutput dRegs rd cd
  -- space needed for spilled stack variables 
  let  intLocals  = 4*(length si)
  let doubLocals  = 8*(length sd)
  -- local var locations
  let iStack      = map (\(_, y) -> Stack (y*4)) (zip si [1..]) -- todo fix offset
  let dStack      = map (\(_, y) -> Stack (y*8 + intLocals)) (zip sd [1..]) -- todo fix offset
  -- which stack-location each spill register maps to
  let sli         = map (\(_, i) -> replaceNth i True (bitArray ki)) (zip si [0..])
  let sld         = map (\(_, i) -> replaceNth i True (bitArray kd)) (zip sd [0..])
  -- update code with stack registers for spilled vars
  registerOutput iStack si sli
  registerOutput dStack sd sld
  -- decrement RSP to make space for stack variables
  o             <- gets output
  let (o1, o2)   = splitAt 2 (reverse o)
  let localSize  = intLocals + doubLocals
  let prep       = if (localSize > 0)
                    then (Add Minus (Lit Int) (X86 RSP) (LitInt (toInteger $ localSize))):o2
                    else                                                                  o2
  modify $ \st -> st { output = reverse (o1++prep)}





-- get a tuple with lists where element n is the def, use and succ set for the nth instruction
defUseSucc :: Compile ([[Bool]], [[Bool]], [[Bool]], [[Bool]])
defUseSucc = do
  o                      <- gets output
  n_label                <- gets nextLabel
  n_reg                  <- gets nextReg
  defs'                  <- defs
  uses                   <- mapM use  (reverse o)
  let (intUses, doubUses) = unzip uses
  succs                  <- mapM succSet (reverse o)
  --return zip3 defs' uses succs
  return (defs', intUses, doubUses, succs)
  where

    -- helper: call def set calculator for every line of code
    defs :: Compile [[Bool]]
    defs = do
      o          <- gets output
      (R n_reg)  <- gets nextReg
      -- set of already defined registers
      let deffed  = bitArray n_reg
      defsets    <- defSets deffed o
      return $ reverse defsets
      where

        -- get all def sets
        defSets :: [Bool] -> [Code] -> Compile [[Bool]]
        defSets deffed' []       = return []
        defSets deffed' (o':os') = do
          (d, deffed'')      <- def deffed' o' -- def sets should be list for each elem
          ds                 <- defSets deffed'' os'
          return (d:ds)
          where

            -- get def set for a line of code
            def :: [Bool] -> Code -> Compile ([Bool], [Bool])
            def deffed (Mov _ _ r) = defV r
            -------------------------------------------------------------
              where

                -- get def set for a value (for register its the index, nothing for any other value)
                defV :: Value -> Compile ([Bool], [Bool])
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
            ---------------------------------------------------------------
            -- only mov-instructions can introduce new variables
            def deffed v     = do
              (R n_reg)         <- gets nextReg
              return ((bitArray n_reg), deffed)

    
    -- get succ set for a line of code
    succSet :: Code -> Compile [Bool]
    succSet (Branch l)           = succL l -- todo first label was f:, and is defined outside this scope, do i need to care about he first one? u cant jump to it
    succSet (BranchCond _ l) = succL l
    succSet _                    = do
      o   <- gets output
      return $ bitArray $ length o -- todo: need to add 1 extra elem for f:? no cause its not in output ig?

    -- get successors of 1 label
    succL :: Label -> Compile [Bool]
    succL l' = do 
      l'' <- labelLine l'
      o   <- gets output
      return $ replaceNth l'' True (bitArray (length o))
      where

        -- get the line that label is on
        labelLine :: Label -> Compile Int
        labelLine l'' = do
          o <- gets output
          return $ fromJust $ elemIndex (Label l'') o

    -- get use set for a line of code, first set is ints, the second is doubles
    use :: Code -> Compile ([Bool], [Bool])
    use x = case x of
      (Add _  t v1   v2) -> combine t v1 v2
      (Mul _  t v1   v2) -> combine t v1 v2
      (Mov    t v1   v2) -> combine t v1 v2
      (Cmp    t v1   v2) -> combine t v1 v2
      (Pop    t       v) -> combine t         v (LitInt (-1)) -- -1 is dummy value
      (Push   t       v) -> combine t         v (LitInt (-1)) 
      (IncDec _       v) -> combine (Lit Int) v (LitInt (-1)) 
      (CNeg           v) -> combine (Lit Int) v (LitInt (-1))
      _                  -> combine (Lit Int)   (LitInt (-1)) (LitInt (-1))
      where
      
      -- get the use set for 2 values
      combine :: LLVMType -> Value -> Value -> Compile ([Bool], [Bool])
      combine t v1 v2 = do
        v1' <- useV v1
        v2' <- useV v2
        return $ partitionType t (bitOr v1' v2')
        where

          -- get the use set of a value
          useV :: Value -> Compile [Bool]
          useV (Reg r) = do 
            (R n) <- gets nextReg
            return $ replaceNth (theRegister r) True (bitArray n)
          useV _       = do
            (R n) <- gets nextReg
            return $ bitArray n

          -- put result in the correct partition depending on type
          partitionType :: LLVMType -> [Bool] -> ([Bool], [Bool])
          partitionType (Lit Doub) b =    (take (length b) (repeat False), b)
          partitionType _          b = (b, take (length b) (repeat False))





-- create a list with the liveness of each virtual register in the current function
-- element n corresponds to n:th instruction, each element is a list of live registers
-- todo make sure u do from backwards to top
liveness :: ([[Bool]], [[Bool]], [[Bool]]) -> Compile [[Bool]]
liveness dus = do
  o        <- gets output
  let n_ins = length o
  (R n_regs)   <- gets nextReg
  -- 1 list for each code line, each list has 1 bool for each reg
  let livein = take n_ins (repeat (take (fromIntegral n_regs) (repeat False)))
  return $ iterate dus livein
  where -- todo should take [[Bool]]

    -- iterate until livein doesnt change
    iterate :: ([[Bool]], [[Bool]], [[Bool]]) -> [[Bool]] -> [[Bool]]
    iterate (defs, uses, succs) livein' = do -- (d, u, s):ds)
      -- out set for every line
      let out       = map (\s' -> liveSucc s' livein') succs
      -- successors "-" def set
      let s_minus_d = map (\(o, d) -> bitMinus o d) (zip out defs)
      -- livein for this iteration
      --error (show defs)
      let livein''  = map (\(u, smd) -> bitOr u smd) (zip uses s_minus_d)
      --return uses
      -- repeat if not done
      if (livein' == livein'')
        then livein''
        else iterate (defs, uses, succs) livein''
      where

        -- get livein of successors
        liveSucc :: [Bool] -> [[Bool]] -> [Bool]
        liveSucc []      _     = []
        liveSucc (s:ss) (l:ls) = do
          let l'  = if (s)
                      then l 
                      else take (length l) (repeat False)
          bitOr l' (liveSucc ss ls)


-- do something with output


-- create an interference tree from liveness list
interference :: [[Bool]] -> [[Bool]]
interference livein = do
  -- edges from each line
  let matrices = map lineInterference livein
  -- combine all edges
  let matrix = foldl1 (zipWith $ zipWith (||)) matrices
  matrix
  where

    -- calculate interference on one line
    lineInterference :: [Bool] -> [[Bool]]
    lineInterference l' = do
      let n_regs = length l'
      -- if register e is live, then add edge to all the other live ones 
      -- make a list for each variable, an adjacency matrix
      map (\(e, i) -> if e
                        then (bitMinus l' ( replaceNth i True (bitArray n_regs)))  -- basically just remove the node itself : [1,1,1,1] -> [0,1,1,1]
                        else bitArray n_regs)
                      (zip l' [0..]) -- inlcude index 0..


-- k-color the registers,
-- returns registers to be allocates, spills and the coloring
kColor :: [[Bool]] -> Int -> Compile ([Int], [Int], [[Bool]])
kColor matrix k = do
  let n_regs            = length matrix
  let stack             = repeatAlg matrix n_regs
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
        availableColor :: [Bool] -> [Bool]
        availableColor (c':cc') = do
          -- if color taken then try the next one, else take this color
          if (c')
            then (False:(availableColor cc'))
            else (True:(take (length cc') (repeat False)))


    -- remove spill nodes and their edges, place the spill nodes in a new stack
    removeSpill :: [(Int, Bool)] -> [[Bool]] -> ([Int], [[Bool]], [Int])
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

      

    -- find node and delete it until k nodes left or fal
    -- arguemtns are the graph, nbr of remaining nodes and the node stack
    repeatAlg :: [[Bool]] -> Int -> [(Int, Bool)]
    repeatAlg m r = do
      if (r == 0) -- todo <= k
        then []
        else do
          let (m', n, _) = findN m (-1)
          let r' = r - 1
          let stack' = repeatAlg m' r'
          (n:stack')
    -- find node n with <k egdges, also delete edges
    -- spill node with most interference
    -- int argument is the max degree discovered
    -- result: 
    -- graph with deleted edges, 
    -- the edge with a spill flag and 
    -- degree of node we need to spill
    findN :: [[Bool]] -> Int -> ([[Bool]], (Int, Bool), Maybe Int)
    findN []     degree = ([], (-1, False), Just degree)
    findN (b:bs) degree = do
      let degree' = boolSum b
      let index   = (length matrix) - (length bs) - 1
      -- if degree' < k, delete this node, otherwise check the next node
      if (degree' < k)
        then do
          let matrix'' = deleteNode index matrix
          (matrix'', (index, False), Nothing)
        else do 
          -- update max degree (for spill prioritization)
          let degree'' = if (degree' >= degree)
                          then degree'
                          else degree
          -- try delete other node
          let (matrix''', (b', flag), degree''') = findN bs degree''
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
                -- dont actually care about any value other than degre''' in this case
                else (matrix''', (b', flag), degree''')
    -- delete node at given index
    deleteNode :: Int -> [[Bool]] -> [[Bool]]
    deleteNode index' m = do
      -- remove edges to n
      let m'  = map (\x -> replaceNth index' False x) m
      -- remove edges from n
      let m'' = replaceNth index' (take (length m') (repeat False)) m'
      m''
    



-- update "output" with new registers
registerOutput :: [X86Reg] -> [Int] -> [[Bool]] -> Compile ()
registerOutput realRegs tempRegs colorMap = do
  --for each elem in stack
  traverseStack tempRegs
  -- for each code
  -- if stack elem == reg, replace
  where

    -- for each register, see if it needs to be updated somewhere
    traverseStack :: [Int] -> Compile ()
    traverseStack []     = return ()
    traverseStack (s:ss) = do
      o     <- gets output
      let o' = traverseCode (s, o)
      modify $ \st -> st { output = o'}
      traverseStack ss
      where

        -- see if register needs to be updated in any line of code
        traverseCode :: (Int, [Code]) -> [Code]
        traverseCode x = case x of
          (s, []) -> []
          (s, (c:cc)) -> do
            let c'  = updateCode c (R s)
            let cc' = traverseCode (s, cc)
            (c':cc')
            where

              -- update 1 line of code
              updateCode :: Code -> Register -> Code
              updateCode x r = case x of
                (Mov t v1 v2) -> (Mov t (swap v1 r) (swap v2 r))
                (Pop t     v) -> (Pop t (swap v  r))
                c -> c
              -- todo implement the rest
                where

                  -- swap temp-reg with real reg if its the correct one
                  swap :: Value -> Register -> Value
                  swap v r = do
                    if (v == Reg r)
                      then (X86 (color2Reg r))
                      else v
                    where

                      -- get the real reg for a temp-reg, given a color mapping
                      color2Reg :: Register -> X86Reg
                      color2Reg (R reg) = do
                        -- the color of this reg
                        let color = getElem reg colorMap
                        --the real reg corresponding to the color 
                        getElem (fromJust (elemIndex True color)) realRegs
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
  --emit $ Alloca r (Lit t)



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
          -- r'    <- loadReg r
          emit $ Cmp (Lit typ) r (LitBool True) -- todo fixreturnreg
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
            -- r'  <- loadReg r -- todo maybe dont need loadreg -- todo fixreturnreg
            emit $ Cmp (Lit Bool) r (LitBool True)
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
          -- r' <- loadReg r -- todo fixreturnreg
          emit $ Cmp (Lit typ) r (LitBool True)
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



-- helper: emit add / mul / rel expression.
emitBinaryOp :: Type -> Operator -> Expr -> Expr -> Bool -> Compile ()
emitBinaryOp t op' e1 e2 b = do
  -- compile arguments
  compileExp e1 True
  compileExp e2 True
  allArgs         <- gets prevResult
  -- args'   <- mapM (\x -> loadReg x) $ take 2 allArgs -- todo fixreturnreg
  let [(arg1, _), (arg2, _)] = take 2 allArgs
  -- create result register
  r               <- newRegister (Lit t)
  -- compile and remove arguments
  case op' of
    Ao op -> emit $ Add op (Lit t) arg2 arg1
    Mo op -> emit $ Mul op (Lit t) arg2 arg1
    Ro op -> compare    op (Lit t) arg2 arg1
  removeArgs 2
  setPrevVal (Reg r, (Lit t)) b

  where
    -- emit instructions for comparison
    compare :: RelOp -> LLVMType -> Value -> Value -> Compile ()
    compare op typ arg2 arg1 = do
      emit $ Cmp typ arg2 arg1
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


  ELitInt i  -> setPrevVal (LitInt i     , Lit Int)  b
  ELitDoub d -> setPrevVal (LitDoub d    , Lit Doub) b
  ELitTrue   -> setPrevVal (LitBool True , Lit Bool) b
  ELitFalse  -> setPrevVal (LitBool False, Lit Bool) b


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
    -- compile arguments and and make sure they dont override each other
    mapM_ (\e -> do
                  compileExp e False -- todo used to be true
                  (prev, typ) <- getPrevResult
                  -- prev' <- loadReg -- todo fixreturnreg
                  emit $ Push typ prev) es
    --allArgs <- gets prevResult
    --args    <- mapM (\x -> loadReg x) (reverse $ take n_args allArgs)
    -- fix types
    
    --let args' = zip ts' args
    -- if void function, then no need to save the result

    emit $ Call (Lit t) id
    -- remove arguments from stack
    emit $ Add Plus (Lit Int) (LitInt $ sum $ map (\x -> size (Lit x)) ts) (X86 RSP)
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
    emit $ Cmp t1 e1_result (LitBool True)
    emit $ BranchCond EQU t
    emit $ Branch f

    -- e2 true?
    emit $ Label t 
    compileExp e2 False -- it is ok to overwrite e1_result
    (e2_result, typ2) <- getPrevResult
    --e2_result <- loadReg r2 -- todo fixreturnreg
    t2        <- newLabel
    -- if e2 true, emit true, otherwise false
    emit $ Cmp typ2 e2_result (LitBool True)
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
    emit $ Cmp t1 e1_result (LitBool True)
    emit $ BranchCond EQU t
    emit $ Branch f

    -- e2 true?
    emit $ Label f 
    compileExp e2 False -- ok to overwrite e1_result
    (e2_result, t2) <- getPrevResult
    -- e2_result <- loadReg r2 -- todo fixreturnreg
    f2        <- newLabel
    -- if e2 true, then emit true, otherwise emit false
    emit $ Cmp t2 e2_result (LitBool True)
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
        let i = case i0 of
                  LitInt i0' -> i0'
                  _          -> (-1) -- not possible
        -- r' <- loadReg -- todo fixreturnreg
        setPrevVal (LitInt ((-1)*i), Lit Int ) b
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
        emit $ CNeg r 


  ETyped e _ -> compileExp e b


  e          -> error $ "not implemented compileexp " ++ show e