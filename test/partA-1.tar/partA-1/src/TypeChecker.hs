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
type Env = (Sig, [Context]) -- functions and context stack
type Sig = Map Ident ([Type], Type) -- function type signature
type Context = Map Ident Type


-- "main method", type check program and return as type annotated
typecheck :: Prog -> Err Prog
typecheck (Program defs) = do
  e' <- addFuns emptyEnv (std ++ defs)
  ds' <- checkFuns e' defs
  return $ Program $ removebuiltIns ds'







-------------------------------------------miscellaneous helper functions ------------------------------------------------

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


-- create new block for loops etc
newBlock :: Env -> Env
newBlock (sig, cons) = (sig, Map.empty : cons)


-- create initial empty environment
emptyEnv :: Env
emptyEnv = (Map.empty, [])


-- extract the type from an annotated expression
unwrap :: Expr -> Err (Expr, Type)
unwrap (ETyped e t) = return (ETyped e t, t)
unwrap _            = Bad "can only unwrap annotated expressions"


------------------------------------------- lookup functions ------------------------------------------------

-- return head type of function
lookupFun :: Env -> Ident -> Err ([Type], Type)
lookupFun (sig, _) id = case Map.lookup id sig of
  Nothing               -> Bad $ "function doesnt exist " ++ show id
  (Just (args, result)) -> return (args, result)


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


------------------------------------------- functions for updating environment ------------------------------------------------


--------------------------- Functions


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


-- add one function to environment
addFun :: Env -> Ident -> ([Type], Type) -> Err Env
addFun (sig, cons) id (args, result) = do
  if (Map.member id sig)
    then Bad $ "function exists already" ++ show id
    else return (Map.insert id (args, result) sig, cons)


--------------------------- Arguments


-- get the types of given arguments
argsToType :: [Arg] -> [Type]
argsToType []                  = []
argsToType ((Argument t id) : as) = t : argsToType as


-- add arguments to environment
addArgs :: Env -> [Arg] -> Err Env
addArgs env []                      = return env
addArgs env ((Argument typ id) : args) = do
  env' <- addVar env id typ
  addArgs env' args


--------------------------- Variables


-- add variables to environment
addVars :: Env -> [Ident] -> Type -> Err Env
addVars env ids typ = foldM (\env id -> addVar env id typ) env ids


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


------------------------------------------- type check + annotation functions ------------------------------------------------




--------------------------- Functions


-- type check function and return it with type annotated statements
checkFuns :: Env -> [TopDef] -> Err [TopDef]
checkFuns _   []       = return []
checkFuns env (d : ds) = do
  (_, d') <- checkFun env d
  ds' <- checkFuns env ds
  return (d' : ds')

-- type check dunction and return it with a type annotated body
checkFun :: Env -> TopDef -> Err (Env, TopDef)
checkFun env (FnDef result id args (Block ss)) = do
  env' <- addArgs env args
  (_, ss', returns) <- checkStms env' result ss (id == (Ident "main"))
  if (returns)
    then return (env, FnDef result id args (Block ss'))
    else Bad ("the function " ++ show id ++ " doesnt guarantee a return statement")



-------------------------- Statements


-- type check and annotate statements
-- Bool argument, main: true if we are checking statements for the main function
-- Bool return value: do these statements guarantee a return statement?
checkStms :: Env -> Type -> [Stmt] -> Bool -> Err (Env, [Stmt], Bool)
-- base case void function, dont care about return statement existence
checkStms e Void [] False = return (e, [], True)
-- base case
checkStms e _ [] main = return (e, [], False)
-- general case void function, dont care about return statement existance
checkStms e Void (s:ss) False = do
  (e', s', _) <- checkStm e Void s False
  (e'', ss', _) <- checkStms e' Void ss False
  return (e'', s':ss', True)
-- general case
checkStms env typ (s : ss) main = do
  (env' , s', s_returns) <- checkStm env typ s main
  (env'', ss', ss_returns) <- checkStms env' typ ss main
  return (env'', s' : ss', s_returns || ss_returns)


-- helper for checkStm, type check and annotate list of declarations
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
-- Bool argument: main, is this the main function?
-- Bool return value: does this statement guarantee a return statement?
checkStm :: Env -> Type -> Stmt -> Bool -> Err (Env, Stmt, Bool)
checkStm env val x main = case x of
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
    if (main && exp /= (ELitInt 0))
      then Bad "main has a return statement which doesnt return the integer literal 0"
      else do
        checkExp env val exp
        exp' <- inferExp env exp
        return (env, Ret exp', True)

  VRet -> do
    if (val == Void)
      then return (env, VRet, True)
      else Bad ( "returns void but expected " ++ printTree val )

  While exp stm -> do
    checkExp env Bool exp
    let env' = newBlock env
    (_, stm', stm_returns) <- checkStm env' val stm main
    exp'      <- inferExp env exp
    if (exp == ELitTrue)
      then return (env, While exp' stm', stm_returns)
      else return (env, While exp' stm', False)
    

  BStmt (Block ss) -> do
    let env' = newBlock env
    (_, ss', returns) <- checkStms env' val ss main
    return (env, BStmt (Block ss'), returns)

  Cond exp s -> do
    checkExp env Bool exp
    let env' = newBlock env
    (_, s', s_returns) <- checkStm env' val s main
    exp'    <- inferExp env exp
    if (exp == ELitTrue)
      then return (env, Cond exp' s', s_returns) 
      else return (env, Cond exp' s', False)

  CondElse exp s1 s2 -> do
    checkExp env Bool exp
    let env' = newBlock env
    (_, s1', s1_returns) <- checkStm env' val s1 main
    (_, s2', s2_returns) <- checkStm env' val s2 main
    exp'     <- inferExp env exp
    case exp of
      ELitTrue -> return (env, CondElse exp' s1' s2', s1_returns)
      ELitFalse -> return (env, CondElse exp' s1' s2', s2_returns)
      _ -> return (env, CondElse exp' s1' s2', s1_returns && s2_returns)

  Ass id exp -> do
    typ <- lookupVar env id
    (e, typ2) <- inferExp env exp >>= unwrap
    if (typ == typ2)
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
  
  Empty -> return (env, x, False)





--------------------------------- Expressions



-- type check expression
checkExp :: Env -> Type -> Expr -> Err Type
checkExp env typ exp = do
  (e, typ2) <- inferExp env exp >>= unwrap
  if (typ2 == typ)
    then return typ
    else
      Bad ( "type of " ++ printTree exp ++ " expected " ++ printTree typ ++ " but found " ++ printTree typ2 )



-- check if a list of expressions have the correct given types
checkTypes :: [Expr] -> [Type] ->  Bool
checkTypes [] [] = True
checkTypes ((ETyped _ t1) : es) (t2 : ts) = t1 == t2 && checkTypes es ts


-- type check and annotate expression
inferExp :: Env -> Expr -> Err Expr
inferExp env x = case x of
  ELitTrue  -> return (ETyped x Bool)

  ELitFalse -> return (ETyped x Bool)

  ELitInt n -> return (ETyped x Int)

  ELitDoub n -> return (ETyped x Doub)

  EString s -> return (ETyped x String)

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
    if (typ == Doub && op == Mod)
      then Bad ("% operator obly works on integers, but was applied on doubles")
      else return (ETyped (EMul exp3 op exp4) typ)

  EAdd exp1 op exp2 -> do
    (exp3, exp4, typ) <- inferBin [Int, Doub] env exp1 exp2
    return (ETyped (EAdd exp3 op exp4) typ)

  ERel exp1 op exp2 -> do
    (e1, typ1) <- inferExp env exp1 >>= unwrap
    (e2, typ2) <- inferExp env exp2 >>= unwrap
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



-- type annotate operands of a binary expression, 
-- the first argument is the allowed types for a given operator
inferBin :: [Type] -> Env -> Expr -> Expr -> Err (Expr, Expr, Type)
inferBin types env exp1 exp2 = do
  (exp1, typ1) <- inferExp env exp1 >>= unwrap
  (exp2, typ2) <- inferExp env exp2 >>= unwrap

  if (elem typ1 types && typ1 == typ2)
    then
      return (exp1, exp2, typ1)
    else
      Bad ( "type of " ++ printTree exp1 ++ " type of " ++ printTree exp2 ++ " expected 1 type from" ++ show types ++ " but found " ++ printTree typ1 ++ " and " ++ printTree typ2 )

