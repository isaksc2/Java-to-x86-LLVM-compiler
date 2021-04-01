module TypeChecker where

import           Control.Monad

import           Data.List
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.Typeable

import           Javalette.Abs
import           Javalette.ErrM
import           Javalette.Print

--built in functions
std =
  [FnDef Void (Ident "printInt") [Argument Int (Ident "i")] (Block [])]
    ++ [FnDef Void (Ident "printDouble") [Argument Doub (Ident "d")] (Block [])]
    ++ [FnDef Void (Ident "printString") [Argument String (Ident "s")] (Block [])]
    ++ [FnDef Int (Ident "readInt") [] (Block [])]
    ++ [FnDef Doub (Ident "readDouble") [] (Block [])]

-- types
type Rank = [Type]
type Env = (Sig, [Context]) -- functions and context stack
type Sig = Map Ident ([Type], Type) -- function type signature
type Context = Map Ident Type -- variables with their types


-- type check program and return as type annotated
typecheck :: Prog -> Err Prog
typecheck (Program defs) = do
  -- creates annotated tree, but dont use it until lab3 i guess?
  --(Program annotatedTree) <- checkDefs emptyEnv (std ++ defs)
  e'   <- addFuns emptyEnv (std ++ defs)
  ds' <- checkFuns e' defs
  return $ Program $ removebuiltIns ds'
  --return ()

-- remove built in functions before returning annotated syntax tree
removebuiltIns :: [TopDef] -> [TopDef]
removebuiltIns [] = []
removebuiltIns (def@(FnDef _ (Ident x) _ _) : defs)
  | isbuiltIn x = removebuiltIns defs
  | otherwise   = def : removebuiltIns defs
 where
  isbuiltIn :: String -> Bool
  isbuiltIn "printInt"    = True
  isbuiltIn "printDouble" = True
  isbuiltIn "printString" = True
  isbuiltIn "readInt"     = True
  isbuiltIn "readDouble"  = True
  isbuiltIn _             = False

-- create new block for loops and such ------------------------------------------------------- not in use
newBlock :: Env -> Env
newBlock (sig, cons) = (sig, Map.empty : cons)

-- create initial empty environment
emptyEnv :: Env
emptyEnv = (Map.empty, [])

-- extract the type from an annotated expression
unwrap :: Expr -> Err (Expr, Type)
unwrap (ETyped e t) = return (ETyped e t, t)
unwrap _            = Bad "can only unwrap annotated expressions"

-- check if functions are type correct
--checkDefs :: Env -> [TopDef] -> Err Prog
--checkDefs e ds = do
--  e'   <- addFuns e ds
--  defs <- checkFuns e' ds
--  return (PDefs defs)

-- type check function and return it with type annotated statements
checkFuns :: Env -> [TopDef] -> Err [TopDef]
checkFuns _   []       = return []
checkFuns env (d : ds) = do
  (_, d') <- checkFun env d
  ds'     <- checkFuns env ds
  return (d' : ds')

--checkFun :: Env -> TopDef -> Err (Env, TopDef)

-- return type of var if exists
lookupVar :: Env -> Ident -> Err Type
lookupVar (_  , []      ) id = Bad $ "variable doesnt exist: " ++ show id
lookupVar (sig, c : cons) id = do
  let present = Map.lookup id c
  if (isNothing present)
    then do
      lookupVar (sig, cons) id
    else do
      return (fromJust present)

-- return head type of function
lookupFun :: Env -> Ident -> Err ([Type], Type) -------------------------------------------------- use internal type instead?
lookupFun (sig, _) id = case Map.lookup id sig of
  Nothing               -> Bad $ "function doesnt exist " ++ show id
  (Just (args, result)) -> return (args, result)

-- add var to environment
addVar :: Env -> Ident -> Type -> Err Env
addVar _ id Void =
  Bad $ "can't have void as type for varaible: " ++ show id
addVar _ id String =
  Bad $ "can't have string as type for varaible: " ++ show id
addVar (sig, []) id typ = do
  let m  = Map.empty
  let m' = Map.insert id typ m
  return (sig, [m'])
addVar (sig, c : cons) id typ = do
  if (Map.member id (head (c : cons)))
    then Bad $ "variable exists already" ++ show id
    else return (sig, Map.insert id typ c : cons)

-- add one function to environment
addFun :: Env -> Ident -> ([Type], Type) -> Err Env
addFun (sig, cons) id (args, result) = do
  if (Map.member id sig)
    then Bad $ "function exists already" ++ show id
    else return (Map.insert id (args, result) sig, cons)


-- type check and annotate expression
inferExp :: Env -> Expr -> Err Expr
inferExp env x = case x of
  --EBool   b              -> return (ETyped x Bool)
  ELitTrue  -> return (ETyped x Bool)
  ELitFalse -> return (ETyped x Bool)

  ELitInt n -> return (ETyped x Int)

  ELitDoub n -> return (ETyped x Doub)

  EString s -> return (ETyped x String)

  --(ETyped e Doub) -> return x ------------------------ remove?

  EVar id -> do
    ETyped x <$> lookupVar env id

  EApp id args1 -> do
    (args2, result) <- lookupFun env id
    argsT <- mapM (inferExp env) args1

    if (length argsT /= length args2)
      then Bad $ "incorrect number of arguments for function " ++ show id
      else do
        let pairs  = zip argsT args2
        if (checkTypes argsT args2)
          then do
            return (ETyped (EApp id argsT) result)
          else Bad $ "arguments have incorrect type for function " ++ show id

  EMul exp1 op exp2 -> do
    (exp3, exp4, typ) <- inferBin [Int, Doub] env exp1 exp2
    return (ETyped (EMul exp3 op exp4) typ)

  EAdd exp1 op exp2 -> do
    (exp3, exp4, typ) <- inferBin [Int, Doub] env exp1 exp2
    return (ETyped (EAdd exp3 op exp4) typ)

  ERel exp1 op exp2 -> do
    (e1, typ1)         <- inferExp env exp1 >>= unwrap
    (e2, typ2)         <- inferExp env exp2 >>= unwrap
    if (typ1 /= typ2)
      then Bad ("cant compare different types " ++ printTree exp1 ++ " and " ++ printTree exp2)
      else do
        if ((op /= EQU && op /= NE ) && typ1 == Bool)
          then
            Bad ( "can't compare bools: "++ printTree exp1++ " and " ++ printTree exp2 ++ " with operators other than == and !=" )
          else return (ETyped (ERel e1 op e2) Bool)

  EAnd exp1 exp2 -> do
    (exp3, exp4, typ) <- inferBin [Bool] env exp1 exp2
    return (ETyped (EAnd exp3 exp4) typ)

  EOr exp1 exp2 -> do
    (exp3, exp4, typ) <- inferBin [Bool] env exp1 exp2
    return (ETyped (EOr exp3 exp4) typ)

  Neg exp -> do
    (exp', t) <- inferExp env exp >>= unwrap
    if (t == Int || t == Doub)
      then return (ETyped (Neg exp') t)
      else Bad ( "can only do negative on numbers, type found: " ++ printTree t )

  Not exp -> do
    checkExp env Bool exp
    exp' <- inferExp env exp 
    return (ETyped (Not exp') Bool)
    
  ETyped e t -> inferExp env e


-- check if a list of expressions have the correct given types
checkTypes :: [Expr] -> [Type] ->  Bool
checkTypes [] [] = True
checkTypes ((ETyped _ t1) : es) (t2 : ts) = t1 == t2 && checkTypes es ts

-- type annotate operands of a binary operation, the first argument is the allowed types for a given operator
inferBin :: [Type] -> Env -> Expr -> Expr -> Err (Expr, Expr, Type)
inferBin types env exp1 exp2 = do
  (exp1, typ1) <- inferExp env exp1 >>= unwrap
  (exp2, typ2) <- inferExp env exp2 >>= unwrap

  if (elem typ1 types && typ1 == typ2)
    then
      return (exp1, exp2, typ1)
    else
      Bad
      $  "type of "
      ++ printTree exp1
      ++ " type of "
      ++ printTree exp2
      ++ " expected types from"
      ++ show types
      ++ " but found "
      ++ printTree typ1
      ++ " and "
      ++ printTree typ2

-- type check expression
checkExp :: Env -> Type -> Expr -> Err Type
checkExp env typ exp = do
  (e, typ2) <- inferExp env exp >>= unwrap
  if (typ2 == typ)
    then return typ
    else
      Bad
      $  "type of "
      ++ printTree exp
      ++ " expected "
      ++ printTree typ
      ++ " but found "
      ++ printTree typ2


-- helper for checkStm, htype check and annotate list of declarations
checkDecls :: Env -> Type -> [Item] -> Err (Env, [Item])
checkDecls e t (NoInit id:items) = do
  e' <- addVar e id t
  (e'', items') <- checkDecls e' t items
  return (e'', NoInit id:items')
checkDecls e t ((Init id exp):items) = do
  checkExp e t exp
  exp' <- inferExp e exp
  e' <- addVar e id t
  (e'', items') <- checkDecls e' t items
  return (e'', (Init id exp'):items')
checkDecls e t [] = return (e, [])


-- type check and annotate statement
-- Bool: flag for indicating that this is the last statement (void functions dont need to have a last return statememt)
checkStm :: Env -> Type -> Stmt -> Bool -> Err (Env, Stmt, Bool)
-- valid last statements
checkStm env val (CondElse exp s1 s2) True = do 
  checkExp env Bool exp
  let env' = (fst env, Map.empty : snd env)
  (_, s1') <- checkStm env' val s1 True
  (_, s2') <- checkStm env' val s2 True
  exp'     <- inferExp env exp
  return (env, CondElse exp' s1' s2')
checkStm env val (Ret exp) True = do
  checkExp env val exp
  exp' <- inferExp env exp
  return (env, Ret exp')
checkStm env val (VRet) True = do
  if (val == Void)
    then return (env, VRet)
    else Bad ( "returns void but expected " ++ printTree val )
--checkStm env val (Empty) True = return (env, Empty)
checkStm _ _ _ True = Bad "the last statement has to be a return statement"

checkStm env val x last = case x of
  SExp exp -> do
    exp' <- inferExp env exp
    return (env, SExp exp', False)

  Decl typ items -> do
    if (typ == String)
      then Bad "variable cant have type string"
      else do
        (env', items') <- checkDecls env typ items
        return (env', Decl typ items', False)

  Ret exp -> do
    checkExp env val exp
    exp' <- inferExp env exp
    return (env, Ret exp', True)

  VRet -> do
    if (val == Void)
      then return (env, VRet, True)
      else Bad ( "returns void but expected " ++ printTree val )

  While exp stm -> do
    checkExp env Bool exp
    let env' = (fst env, Map.empty : snd env) ------------------------------------------ empty env
    (_, stm', stm_returns) <- checkStm env' val stm last
    exp'      <- inferExp env exp
    if (exp == ELitTrue)
      then let returns = stm_returns
      else let returns = False
    return (env, While exp' stm', returns)

  BStmt (Block ss) -> do
    let env' = (fst env, Map.empty : snd env)
    (_, ss', returns) <- checkStms env' val ss last
    return (env, BStmt (Block ss'), returns)

  Cond exp s -> do
    checkExp env Bool exp
    let env' = (fst env, Map.empty : snd env) -----------------------newblock
    (_, s', s_returns) <- checkStm env' val s last
    exp'    <- inferExp env exp
    if (exp == ELitTrue)
      then let returns = s_returns
      else let returns = False 
    return (env, Cond exp' s', returns)

  CondElse exp s1 s2 -> do
    checkExp env Bool exp
    let env' = (fst env, Map.empty : snd env)
    (_, s1', s1_returns) <- checkStm env' val s1 last
    (_, s2', s2_returns) <- checkStm env' val s2 last
    exp'     <- inferExp env exp
    if (exp == ELitTrue)
      then let returns = s1_returns
      else let returns = False
    if (exp == ELitFalse)
      then let returns = s2_returns
      else let returns = False  
    return (env, CondElse exp' s1' s2', returns)

  Ass id exp -> do
    typ <- lookupVar env id
    (e, typ2) <- inferExp env exp >>= unwrap
    if (typ == typ2) -------------------------------------------------- use checkexp?
      then return (env , Ass id e, False )
      else Bad ( "variable " ++ show id ++ " has type " ++ printTree typ ++ ", cant assign expression of different type " ++ printTree typ2 )
    
  Incr id -> do
    typ <- lookupVar env id
    if (typ == Int || typ == Doub)
      then return (env, x, False)
      else
        Bad ( "type " ++ printTree typ ++ "of variable " ++ show id ++ " cant be incremented" )

  Decr id -> do
    typ <- lookupVar env id
    if (typ == Int || typ == Doub)
      then return (env, x, False)
      else  Bad ( "type " ++ printTree typ ++ "of variable " ++ show id ++ " cant be decremented" )

  SExp exp -> do
    exp' <- inferExp env exp
    return (env, SExp exp', False)
  
  Empty -> return (env, x, False)




-- type check and annotate statements
-- Bool last: the last statement in this block has to be a return statement
-- Bool returns: do these statements fuarantee a return?
checkStms :: Env -> Type -> [Stmt] -> Bool -> Err (Env, [Stmt], Bool)
checkStms e _ [] _ = return (e, [])
checkStms e Void (s:[]) _ = do
  (e', s') <- checkStm e Void s False
  return (e', s':[])
-- if(true), we must have return statement if not void
checkStms env t ((Cond ELitTrue s) :ss) True = do
  (env', s') <- checkStm env typ s ?
  (env'', ss') <- checkStms env' typ ss last -- the doesnt need to be another return statement
  return (env'', s' : ss')
  -- if(true) s1 else s2
checkStms env t ((CondElse ELitTrue s1 s2) :ss) last = do
  (env', s') <- checkStm env typ s True
  (env'', ss') <- checkStms env' typ ss False -- the doesnt need to be another return statement
  return (env'', s' : ss')


--checkStms e t ((Ret exp):[]) True = do -------------------- return
  --(e', s') <- checkStm e t (Ret exp) True
  --return (e', s':[])
--checkStms e t ((Condelse s1 exp s2):[]) True = do -------------------- return
  --(e', s') <- checkStm e t (Condelse s1 exp s2) True
  --return (e', s':[])
--checkStms e t (s:[]) True = do
  --Bad ( "last statement has to be return for function with return type " ++ printTree t )
checkStms env typ (s : []) = do
  (env', s', returns) <- checkStm env typ s
  return (env', s':[], returns)

checkStms env typ (s : ss) = do
  (env' , s', s_returns) <- checkStm env typ s
  (env'', ss', ss_returns) <- checkStms env' typ ss
  return (env'', s' : ss', s_returns || ss_returns)


-- type check dunction and return it with a type annotated body
checkFun :: Env -> TopDef -> Err (Env, TopDef)
checkFun env (FnDef result id args (Block ss)) = do
  env'     <- addArgs env args
  (_, ss') <- checkStms env' result ss True
  return (env, FnDef result id args (Block ss'))

-- add the given functions to the environment
addFuns :: Env -> [TopDef] -> Err Env
addFuns e [] = do
  lookupFun e (Ident "main")
  return e
addFuns env ((FnDef result (Ident "main") args ss) : ds) = do
  if (result /= Int)
    then Bad "main must have type int"
    else do
      if (args /= [])
        then Bad "main must have no arguments"
        else do
          env' <- addFun env (Ident "main") (argsToType args, result)
          addFuns env' ds
addFuns env ((FnDef result id args ss) : ds) = do
  env' <- addFun env id (argsToType args, result)
  addFuns env' ds


-- get the types of given arguments
argsToType :: [Arg] -> [Type]
argsToType []                  = []
argsToType ((Argument t id) : as) = t : argsToType as

-- add variables to environment
addVars :: Env -> [Ident] -> Type -> Err Env
addVars env ids typ = foldM (\env id -> addVar env id typ) env ids


-- add arguments to environment
addArgs :: Env -> [Arg] -> Err Env
addArgs env []                      = return env
addArgs env ((Argument typ id) : args) = do
  env' <- addVar env id typ
  addArgs env' args
