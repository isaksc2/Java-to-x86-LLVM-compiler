module Interpreter where


import Data.Maybe
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map

import CMM.Abs
import CMM.Print
import CMM.ErrM



interpret :: Program -> IO ()
interpret (PDefs defs) = do
  liftIO (runExceptT (runStateT (runMain defs) (emptyEnv)))
  return ()

-- built in functions
std = [(DFun Type_void ( Id "printInt") ([ADecl Type_int ( Id "i")]) ([]))]++
      [(DFun Type_void ( Id "printDouble") ([ADecl Type_double ( Id "d")]) ([]))]++
      [(DFun Type_int ( Id "readInt") ([]) ([]))]++
      [(DFun Type_double ( Id "readDouble") ([]) ([]))]

runMain :: [Def] -> Exec Value
runMain defs = do
  addFuns (std ++ defs)
  (eval ((EApp (Id "main") []) ))

reportResult :: (Sig -> ()) -> IO ()
reportResult = undefined

data Value
  = VInt Integer
  | VDouble Double
  | VBool Bool
  | VVoid
  deriving (Eq, Show)

type Exec = (StateT Env (ExceptT Value IO))

-- initial values / execute / run, takes monad and initial state
type Env = (Sig,[Context]) -- functions and context stack
type Sig = Map Id ([Arg],[Stm],Type) -- function type signature
type Context = Map Id Value -- variables with their types

lookupVar :: Id -> Exec Value
lookupVar id = do
  (sig, (cons)) <- get
  if ((length cons) == 0)
    then
      liftIO (fail $ "variable " ++ (show id) ++ " doesnt exist")
    else do
      let c = (head cons)
      let val = Map.lookup id c
      if (isJust val)
        then do
          let v = fromJust $ Map.lookup id c
          put (sig, cons)
          return v
        else do
          put (sig, (tail cons))
          v <- lookupVar id
          put (sig, cons)
          return v

lookupFun :: Id -> Exec ([Arg], [Stm], Type)
lookupFun id = do
  (sig, cons) <- get
  return $ fromJust (Map.lookup id sig)


updateVar :: (Id , Value) -> Exec ()
updateVar (id, VVoid) = liftIO (fail $ "cant assign void / null to variable " ++ (show id))
updateVar (id, val) = do
  (sig, c:cons) <- get
  let b = Map.member id c
  if(b)
    then do
      let m = Map.insert id val c
      put (sig, m:cons)
      return ()
    else do
      put (sig, cons)
      updateVar (id,val)
      (sig', cons') <- get
      put (sig' , (c:cons'))
      return ()

updateVars :: [(Id, Value)] -> Exec ()
updateVars ((id, v):ivs) = do
  updateVar (id, v)
  updateVars ivs
  return ()



makeEnv :: [Arg] -> [Value] -> Exec ()
makeEnv args values = do
  declareArgs args values
  return ()

declareVar :: Id -> Value -> Exec ()
declareVar id val = do
  (sig, cons) <- get
  if(null cons)
    then do
      let c = Map.empty
      let c' = Map.insert id val c
      put (sig, c':cons)
    else do
      let c = head cons
      let c' = Map.insert id val c
      put (sig, c':(tail cons))


declareVars :: [Id] -> [Value] -> Exec ()
declareVars [] [] = return ()
declareVars (id:ids) (v:values) = do
   declareVar id v
   declareVars ids values

declareArgs :: [Arg] -> [Value] -> Exec ()
declareArgs [] _ = return ()
declareArgs ((ADecl typ id):args) (v:values) = do
  if (typ == Type_double)
    then
      declareVar id (VDouble (toDouble v))
    else
      declareVar id v
  declareArgs args values

addFun :: Id -> ([Arg],[Stm],Type) -> Exec()
addFun id (args, ss, typ) = do
  (sig, cons) <- get
  let sig' = Map.insert id (args, ss, typ) sig
  put  (sig', cons)


addFuns ::  [Def] -> Exec ()
addFuns [] = return ()
addFuns ((DFun typ id arg ss):defs) = do
  (addFun id (arg ,ss, typ))
  addFuns defs

--create empty environment
emptyEnv :: Env
emptyEnv = (Map.empty,[])

getType :: Value -> Type
getType (VInt _ )= Type_int
getType (VDouble _ )= Type_double
getType (VBool _ )= Type_bool
getType (VVoid)= Type_void




eval :: Exp -> Exec (Value)
eval exp = case exp of
  EApp (Id "printInt") es-> do
    vs <- evals es
    liftIO $ putStr ( (show (getIntValue (head vs))) ++ "\n")
    return VVoid

  EApp (Id "printDouble") es-> do
    vs <- evals es
    liftIO $ putStr ((show (toDouble (head vs))) ++ "\n")
    return VVoid

  EApp (Id "readInt") es -> do
     s <- liftIO (getLine)
     let v = read (s)::Integer
     return (VInt v)

  EApp (Id "readDouble") es -> do
    s <- liftIO (getLine)
    let v = read s::Double
    return (VDouble v)

  EApp f es -> do
    vs <- evals es
    (params,ss,typ) <- lookupFun f
    env <- get
    addLayer
    makeEnv params vs
    r <- ((evalStms ss) `catchError` (\v -> do
      if (typ == Type_double)
        then
          return (VDouble (toDouble v))
        else
          return v))
    removeLayer
    return r

  EId id -> do
    v <- lookupVar id
    if(v == VVoid)
      then
        liftIO (fail ("uninitialized variable " ++ show id ))
      else
        return v

  EAss x e -> do
    v <- eval e
    updateVar (x, v)

    return v
  EBool bool ->
    return (VBool ( fromBoolLit bool))

  (EInt n) -> return (VInt n)

  EDouble n -> return (VDouble n)

  (EPost id op) -> do
    v <- lookupVar id
    if(op == OInc)
      then
        updateVar (id , ( add' v (VInt 1)))
      else
        updateVar (id , (sub' v (VInt 1)))
    return v

  (EPre op id) -> do
    v <- lookupVar id
    if(op == OInc)
      then do
          let v' = add' v (VInt 1)
          updateVar (id, v')
          return v'
      else do
          let v' = sub' v  (VInt 1)
          updateVar (id, v')
          return v'

  (EMul e1 op e2) -> do
    v1 <- eval e1
    v2 <- eval e2
    if(op == OTimes)
      then
        return  (mul' v1 v2)
      else
        return  (div' v1 v2)

  (EAdd e1 op e2) -> do
    v1 <- eval e1
    v2 <- eval e2
    if(op == OPlus)
      then
        return  (add' v1 v2)
      else
        return (sub' v1 v2)
  (ECmp e1 op e2) -> do
    v1 <- eval e1
    v2 <- eval e2
    let result = (cmp' v1 v2 op)

    return result
  EAnd e1 e2 -> do
    v1 <- eval e1
    if (getBoolValue v1)
      then do
        v2 <- eval e2
        return  (VBool ( (getBoolValue v1) && (getBoolValue v2)))
      else
        return (VBool False)

  EOr e1 e2 -> do
    v1 <- eval e1
    if (getBoolValue v1)
      then
        return (VBool True)
      else do
        v2 <- eval e2
        return  (VBool ( (getBoolValue v1) || (getBoolValue v2)))




toDouble :: Value -> Double
toDouble (VInt i) = (fromIntegral i)
toDouble (VDouble i) =  i
toDouble v = error  (show v)

getIntValue :: Value -> Integer
getIntValue (VInt v) = v

getBoolValue :: Value -> Bool
getBoolValue (VBool v) = v

fromBoolLit :: BoolLit -> Bool
fromBoolLit LTrue = True
fromBoolLit LFalse = False


cmp' :: Value -> Value -> CmpOp -> Value
cmp' (VBool v1) (VBool v2) OEq = VBool $ v1 == v2
cmp' (VBool v1) (VBool v2) ONEq = VBool $ v1 /= v2
cmp' v1 v2 OEq = VBool $ (toDouble v1) == (toDouble v2)
cmp' v1 v2 ONEq = VBool $ (toDouble v1) /= (toDouble v2)
cmp' v1 v2 OLt = VBool $ (toDouble v1) < (toDouble v2)
cmp' v1 v2 OGt = VBool $ (toDouble v1) > (toDouble v2)
cmp' v1 v2 OLtEq = VBool $ (toDouble v1) <= (toDouble v2)
cmp' v1 v2 OGtEq = VBool $ (toDouble v1) >= (toDouble v2)

div' :: Value -> Value -> Value
div' (VInt v1) (VInt v2) = VInt $ div v1 v2
div' v1 v2 = VDouble $ (toDouble v1) / (toDouble v2)

mul' :: Value -> Value -> Value
mul' (VInt v1) (VInt v2) = VInt $ v1 * v2
mul' v1 v2 = VDouble $ (toDouble v1) * (toDouble v2)

add' :: Value -> Value -> Value
add' (VInt v1) (VInt v2) = VInt $ v1 + v2
add' v1 v2 = VDouble $ (toDouble v1) + (toDouble v2)

sub' :: Value -> Value -> Value
sub' (VInt v1) (VInt v2) = VInt $ v1 - v2
sub' v1 v2 = VDouble $ (toDouble v1) - (toDouble v2)




evalStm :: Stm -> Exec Value
evalStm (SReturn e) = do
  v <- (eval e)
  throwError v

evalStm (SExp e) = do
   (eval e)

evalStm (SDecls _ ids) = do
   (declareVars ids ( replicate (length ids) VVoid))
   return VVoid

evalStm (SInit typ id e) = do
    v <- (eval e)
    if(typ == Type_double)
      then do
         let v' = (VDouble $ toDouble v)
         (declareVar id v')
         return v'
      else do
        (declareVar id v)
        return v

evalStm (SWhile e stm) = do
      v <- (eval e)
      if(getBoolValue v)
        then do
          addLayer
          evalStm stm
          removeLayer
          evalStm (SWhile e stm)
        else do
          return VVoid

evalStm (SBlock ss) = do
    addLayer
    v <- (evalStms ss)
    removeLayer
    return VVoid

evalStm (SIfElse exp s1 s2) = do
    b <-( eval exp)
    addLayer
    if(getBoolValue b)
      then do
        v <- (evalStm s1)
        removeLayer
        return VVoid
      else do
        v <- (evalStm s2)
        removeLayer
        return VVoid




evals :: [Exp] -> Exec [Value]
evals = mapM eval

evalStms :: [Stm] -> Exec Value
evalStms ss =  do
  mapM_ evalStm ss
  return VVoid


-- add "block" to environment
addLayer :: Exec ()
addLayer = do
  (sig, cons) <- get
  put (sig, Map.empty:cons)

-- remove innermost "block" from environment
removeLayer :: Exec ()
removeLayer = do
  (sig, c:cons) <- get
  put (sig, cons)
