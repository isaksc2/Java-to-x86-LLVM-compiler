module TypeChecker where

import           Control.Monad

import           Data.List
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.Typeable

import           CMM.Abs
import           CMM.ErrM
import           CMM.Print

--built in functions
std =
  [FnDef Void (Id "printInt") [Argument Int (Id "i")] []]
    ++ [FnDef Void (Id "printDouble") [Argument Doub (Id "d")] []]
    ++ [FnDef Void (Id "printString") [Argument String (Id "s")] []]
    ++ [FnDef Int (Id "readInt") [] []]
    ++ [FnDef Doub (Id "readDouble") [] []]

-- types
type Rank = [Type]
type Env = (Sig, [Context]) -- functions and context stack
type Sig = Map Id ([Type], Type) -- function type signature
type Context = Map Id Type -- variables with their types


-- type check program and return as type annotated
typecheck :: Prog -> Err Prog
typecheck (Program defs) = do
  -- creates annotated tree, but dont use it until lab3 i guess?
  --(Program annotatedTree) <- checkDefs emptyEnv (std ++ defs)
  let env = emptyEnv (std ++ defs)
  e'   <- addFuns env defs
  ds' <- checkFuns e' defs
  return $ Program $ removebuiltIns ds'
  --return ()

-- remove built in functions before returning annotated syntax tree
removebuiltIns :: [TopDef] -> [TopDef]
removebuiltIns [] = []
removebuiltIns (def@(FnDef _ (Id x) _ _) : defs)
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
unwrap :: Exp -> Err (Exp, Type)
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
lookupVar :: Env -> Id -> Err Type
lookupVar (_  , []      ) id = Bad $ "variable doesnt exist: " ++ show id
lookupVar (sig, c : cons) id = do
  let present = Map.lookup id c
  if (isNothing present)
    then do
      lookupVar (sig, cons) id
    else do
      return (fromJust present)

-- return head type of function
lookupFun :: Env -> Id -> Err ([Type], Type) -------------------------------------------------- use internal type instead?
lookupFun (sig, _) id = case Map.lookup id sig of
  Nothing               -> Bad $ "function doesnt exist " ++ show id
  (Just (args, result)) -> return (args, result)

-- add var to environment
addVar :: Env -> Id -> Type -> Err Env
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
addFun :: Env -> Id -> ([Type], Type) -> Err Env
addFun (sig, cons) id (args, result) = do
  if (Map.member id sig)
    then Bad $ "function exists already" ++ show id
    else return (Map.insert id (args, result) sig, cons)


-- type check and annotate expression
inferExp :: Env -> Exp -> Err Exp
inferExp env x = case x of
  EBool   b              -> return (ETyped x Bool)

  EInt    n              -> return (ETyped x Int)

  EDouble n              -> return (ETyped x Doub)

  EString s              -> return (Etyped x String)

  (ETyped e Doub) -> return x ------------------------ remove?

  EId id                 -> do
    ETyped x <$> lookupVar env id

  EApp id args1 -> do
    (args2, result) <- lookupFun env id
    argsT           <- mapM (inferExp env) args1

    if (length argsT /= length args2)
      then Bad $ "incorrect amount of arguments for function " ++ show id
      else do
        let pairs  = zip argsT args2
        let argsT' = coerceTargets pairs
        if (checkTypes argsT' args2)
          then do
            let argsT'' = map fst argsT'
            return (ETyped (EApp id argsT'') result)
          else Bad $ "arguments have incorrect type for function " ++ show id

  EPost id op -> do
    typ <- lookupVar env id
    if (typ == Int || typ == Doub)
      then return (ETyped x typ)
      else
        Bad
        $  "type "
        ++ printTree typ
        ++ "of variable "
        ++ show id
        ++ " incompatible with operator "
        ++ show op

  EPre op id -> do
    typ <- lookupVar env id
    if (typ == Int || typ == Doub)
      then return (ETyped x typ)
      else
        Bad
        $  "type "
        ++ printTree typ
        ++ "of variable "
        ++ show id
        ++ " incompatible with operator "
        ++ show op

  EMul exp1 op exp2 -> do
    (exp3, exp4, typ) <- inferBin [Int, Doub] env exp1 exp2
    return (ETyped (EMul exp3 op exp4) typ)

  EAdd exp1 op exp2 -> do
    (exp3, exp4, typ) <- inferBin [Int, Doub] env exp1 exp2
    return (ETyped (EAdd exp3 op exp4) typ)

  ECmp exp1 op exp2 -> do
    (e1, typ1)         <- inferExp env exp1 >>= unwrap
    (e2, typ2)         <- inferExp env exp2 >>= unwrap
    (exp3, exp4, typ3) <- coerce (e1, typ1) (e2, typ2)
    if ((op /= OEq && op /= ONEq) && typ3 == Bool)
      then
        Bad
        $  "can't compare bools: "
        ++ printTree exp1
        ++ " and "
        ++ printTree exp2
      else return (ETyped (ECmp exp3 op exp4) Bool)

  EAnd exp1 exp2 -> do
    (exp3, exp4, typ) <- inferBin [Bool] env exp1 exp2
    return (ETyped (EAnd exp3 exp4) typ)

  EOr exp1 exp2 -> do
    (exp3, exp4, typ) <- inferBin [Bool] env exp1 exp2
    return (ETyped (EOr exp3 exp4) typ)

  EAss id exp -> do
    typ      <- lookupVar env id
    if (typ == String)
        then Bad ("variable " ++ show id ++ " can't have type string")
        else do
          (e, inf) <- inferExp env exp >>= unwrap
          let (e', typ') = coerceTarget e inf typ
          if (typ == typ')
            then return (ETyped (EAss id e') typ)
            else
              Bad
              $  "variable "
              ++ show id
              ++ " has type "
              ++ printTree typ
              ++ ", cant assign expression of type "
              ++ printTree typ'
  ETyped e t -> inferExp env e

-- check if a list of expressions have the correct given types
checkTypes :: [(Exp, Type)] -> [Type] -> Bool
checkTypes []             []        = True
checkTypes ((e, t) : ets) (t2 : ts) = t == t2 && checkTypes ets ts

-- type annotate operands of a binary operation, the first argument is the allowed types for a given operator
inferBin :: [Type] -> Env -> Exp -> Exp -> Err (Exp, Exp, Type)
inferBin types env exp1 exp2 = do
  (exp1, typ1) <- inferExp env exp1 >>= unwrap
  (exp2, typ2) <- inferExp env exp2 >>= unwrap

  if (elem typ1 types && typ1 == typ2)
    then
      (exp1, exp2, typ1)
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
checkExp :: Env -> Type -> Exp -> Err Type
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




-- type check and annotate statement
checkStm :: Env -> Type -> Stm -> Err (Env, Stm)
checkStm env val x = case x of
  SExp exp -> do
    exp' <- inferExp env exp
    return (env, SExp exp')

  SDecls typ ids -> do
    env' <- addVars env ids typ
    return (env', x)

  SInit typ id exp -> do
    env'       <- addVar env id typ
    (exp', te) <- inferExp env' exp >>= unwrap
    let (exp'', typ') = coerceTarget exp' te typ
    checkExp env' typ exp''
    return (env', SInit typ id exp'')

  SReturn exp -> do
    (exp', te) <- inferExp env exp >>= unwrap
    let (exp'', typ') = coerceTarget exp' te val
    checkExp env val exp''
    return (env, SReturn exp'')

  SWhile exp stm -> do
    checkExp env Bool exp
    let env' = (fst env, Map.empty : snd env)
    (_, stm') <- checkStm env' val stm
    exp'      <- inferExp env exp
    return (env, SWhile exp' stm')

  SBlock ss -> do
    let env' = (fst env, Map.empty : snd env)
    (_, ss') <- checkStms env' val ss
    return (env, SBlock ss')

  SIfElse exp s1 s2 -> do
    checkExp env Bool exp
    let env' = (fst env, Map.empty : snd env)
    (_, s1') <- checkStm env' val s1
    (_, s2') <- checkStm env' val s2
    exp'     <- inferExp env exp
    return (env, SIfElse exp' s1' s2')




-- type check and annotate statements
checkStms :: Env -> Type -> [Stm] -> Err (Env, [Stm])
checkStms e   _   []       = return (e, [])
checkStms env typ (s : ss) = do
  (env' , s' ) <- checkStm env typ s
  (env'', ss') <- checkStms env' typ ss
  return (env'', s' : ss')


-- type check dunction and return it with a type annotated body
checkFun :: Env -> TopDef -> Err (Env, TopDef)
checkFun env (FnDef result id args ss) = do
  env'     <- addArgs env args
  (_, ss') <- checkStms env' result ss
  return (env, FnDef result id args ss')

-- add the given functions to the environment
addFuns :: Env -> [TopDef] -> Err Env
addFuns e [] = do
  lookupFun e (Id "main")
  return e
addFuns env ((FnDef result (Id "main") args ss) : ds) = do
  if (result /= Int)
    then Bad "main must have type int"
    else do
      if (args /= [])
        then Bad "main must have no arguments"
        else do
          env' <- addFun env (Id "main") (argsToType args, result)
          addFuns env' ds
addFuns env ((FnDef result id args ss) : ds) = do
  env' <- addFun env id (argsToType args, result)
  addFuns env' ds


-- get the types of given arguments
argsToType :: [Arg] -> [Type]
argsToType []                  = []
argsToType ((Argument t id) : as) = t : argsToType as

-- add variables to environment
addVars :: Env -> [Id] -> Type -> Err Env
addVars env ids typ = foldM (\env id -> addVar env id typ) env ids

-- add arguments to environment
addArgs :: Env -> [Arg] -> Err Env
addArgs env []                      = return env
addArgs env ((Argument typ id) : args) = do
  env' <- addVar env id typ
  addArgs env' args
