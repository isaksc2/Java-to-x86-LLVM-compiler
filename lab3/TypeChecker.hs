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
  [DFun Type_void (Id "printInt") [ADecl Type_int (Id "i")] []]
    ++ [DFun Type_void (Id "printDouble") [ADecl Type_double (Id "d")] []]
    ++ [DFun Type_int (Id "readInt") [] []]
    ++ [DFun Type_double (Id "readDouble") [] []]



typecheck :: Program -> Err Program
typecheck (PDefs defs) = do
  -- creates annotated tree, but dont use it until lab3 i guess?
  (PDefs annotatedTree) <- checkDefs emptyEnv (std ++ defs)
  return $ PDefs $ removeBuildins annotatedTree
  --return ()

removeBuildins :: [Def] -> [Def]
removeBuildins [] = []
removeBuildins (def@(DFun _ (Id x) _ _) : defs)
  | isBuildIn x = removeBuildins defs
  | otherwise   = def : removeBuildins defs
 where
  isBuildIn :: String -> Bool
  isBuildIn "printInt"    = True
  isBuildIn "printDouble" = True
  isBuildIn "readInt"     = True
  isBuildIn "readDouble"  = True
  isBuildIn _             = False

checkDefs :: Env -> [Def] -> Err Program
checkDefs e ds = do
  e'   <- addSigs e ds
  defs <- concatDefs e' ds
  return (PDefs defs)

concatDefs :: Env -> [Def] -> Err [Def]
concatDefs _   []       = return []
concatDefs env (d : ds) = do
  (_, d') <- checkDef env d
  ds'     <- concatDefs env ds
  return (d' : ds')

--checkDef :: Env -> Def -> Err (Env, Def)


type Rank = [Type]
type Env = (Sig, [Context]) -- functions and context stack
type Sig = Map Id ([Type], Type) -- function type signature
type Context = Map Id Type -- variables with their types

lookupVar :: Env -> Id -> Err Type
lookupVar (_  , []      ) id = Bad $ "variable doesnt exist: " ++ show id
lookupVar (sig, c : cons) id = do
  let present = Map.lookup id c
  if (isNothing present)
    then do
      lookupVar (sig, cons) id
    else do
      return (fromJust present)

lookupFun :: Env -> Id -> Err ([Type], Type)
lookupFun (sig, _) id = case Map.lookup id sig of
  Nothing               -> Bad $ "function doesnt exist " ++ show id
  (Just (args, result)) -> return (args, result)

updateVar :: Env -> Id -> Type -> Err Env
updateVar _ id Type_void =
  Bad $ "can't have void as type for varaible: " ++ show id
updateVar (sig, []) id typ = do
  let m  = Map.empty
  let m' = Map.insert id typ m
  return (sig, [m'])
updateVar (sig, c : cons) id typ = do
  if (Map.member id (head (c : cons)))
    then Bad $ "variable exists already" ++ show id
    else return (sig, Map.insert id typ c : cons)

updateFun :: Env -> Id -> ([Type], Type) -> Err Env
updateFun (sig, cons) id (args, result) = do
  if (Map.member id sig)
    then Bad $ "function exists already" ++ show id
    else return (Map.insert id (args, result) sig, cons)

newBlock :: Env -> Env
newBlock (sig, cons) = (sig, Map.empty : cons)

emptyEnv :: Env
emptyEnv = (Map.empty, [])

unwrap :: Exp -> Err (Exp, Type)
unwrap (ETyped e t) = return (ETyped e t, t)
unwrap _            = Bad "can only unwrap annotated expressions"


inferExp :: Env -> Exp -> Err Exp
inferExp env x = case x of
  EBool   b              -> return (ETyped x Type_bool)

  EInt    n              -> return (ETyped x Type_int)

  EDouble n              -> return (ETyped x Type_double)
  (ETyped e Type_double) -> return x

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
    if (typ == Type_int || typ == Type_double)
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
    if (typ == Type_int || typ == Type_double)
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
    (exp3, exp4, typ) <- inferBin [Type_int, Type_double] env exp1 exp2
    return (ETyped (EMul exp3 op exp4) typ)

  EAdd exp1 op exp2 -> do
    (exp3, exp4, typ) <- inferBin [Type_int, Type_double] env exp1 exp2
    return (ETyped (EAdd exp3 op exp4) typ)

  ECmp exp1 op exp2 -> do
    (e1, typ1)         <- inferExp env exp1 >>= unwrap
    (e2, typ2)         <- inferExp env exp2 >>= unwrap
    (exp3, exp4, typ3) <- coerce (e1, typ1) (e2, typ2)
    if ((op /= OEq && op /= ONEq) && typ3 == Type_bool)
      then
        Bad
        $  "can't compare bools: "
        ++ printTree exp1
        ++ " and "
        ++ printTree exp2
      else return (ETyped (ECmp exp3 op exp4) Type_bool)

  EAnd exp1 exp2 -> do
    (exp3, exp4, typ) <- inferBin [Type_bool] env exp1 exp2
    return (ETyped (EAnd exp3 exp4) typ)

  EOr exp1 exp2 -> do
    (exp3, exp4, typ) <- inferBin [Type_bool] env exp1 exp2
    return (ETyped (EOr exp3 exp4) typ)

  EAss id exp -> do
    typ      <- lookupVar env id
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

checkTypes :: [(Exp, Type)] -> [Type] -> Bool
checkTypes []             []        = True
checkTypes ((e, t) : ets) (t2 : ts) = t == t2 && checkTypes ets ts

coerceTargets :: [(Exp, Type)] -> [(Exp, Type)]
coerceTargets [] = []
coerceTargets ((e@(ETyped exp typ), target) : ts) =
  coerceTarget e typ target : coerceTargets ts

coerceTarget :: Exp -> Type -> Type -> (Exp, Type)
coerceTarget e Type_int Type_double = (ETyped e Type_double, Type_double)
coerceTarget (ETyped e _) t _ = (ETyped e t, t)
coerceTarget e t _ = (ETyped e t, t)

coerce :: (Exp, Type) -> (Exp, Type) -> Err (Exp, Exp, Type)
coerce (e1, Type_int) (e2, Type_double) =
  return (ETyped e1 Type_double, e2, Type_double)
coerce (e1, Type_double) (e2, Type_int) =
  return (e1, ETyped e2 Type_double, Type_double)
coerce (e1, t1) (e2, t2)
  | t1 == t2
  = return (e1, e2, t1)
  | otherwise
  = Bad
    $  "failed when casting "
    ++ printTree e1
    ++ " and "
    ++ printTree e2
    ++ ",can't cast "
    ++ printTree t1
    ++ " to "
    ++ printTree t2
    ++ " or vice versa"


inferBin :: [Type] -> Env -> Exp -> Exp -> Err (Exp, Exp, Type)
inferBin types env exp1 exp2 = do
  (exp1, typ1) <- inferExp env exp1 >>= unwrap
  (exp2, typ2) <- inferExp env exp2 >>= unwrap

  if (elem typ1 types && elem typ2 types)
    then do
      coerce (exp1, typ1) (exp2, typ2)
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





checkStm :: Env -> Type -> Stm -> Err (Env, Stm)
checkStm env val x = case x of
  SExp exp -> do
    exp' <- inferExp env exp
    return (env, SExp exp')

  SDecls typ ids -> do
    env' <- updateVars env ids typ
    return (env', x)

  SInit typ id exp -> do
    env'       <- updateVar env id typ
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
    checkExp env Type_bool exp
    let env' = (fst env, Map.empty : snd env)
    (_, stm') <- checkStm env' val stm
    exp'      <- inferExp env exp
    return (env, SWhile exp' stm')

  SBlock ss -> do
    let env' = (fst env, Map.empty : snd env)
    (_, ss') <- checkStms env' val ss
    return (env, SBlock ss')

  SIfElse exp s1 s2 -> do
    checkExp env Type_bool exp
    let env' = (fst env, Map.empty : snd env)
    (_, s1') <- checkStm env' val s1
    (_, s2') <- checkStm env' val s2
    exp'     <- inferExp env exp
    return (env, SIfElse exp' s1' s2')





checkStms :: Env -> Type -> [Stm] -> Err (Env, [Stm])
checkStms e   _   []       = return (e, [])
checkStms env typ (s : ss) = do
  (env' , s' ) <- checkStm env typ s
  (env'', ss') <- checkStms env' typ ss
  return (env'', s' : ss')



checkDef :: Env -> Def -> Err (Env, Def)
checkDef env (DFun result id args ss) = do
  env'     <- updateArgs env args
  (_, ss') <- checkStms env' result ss
  return (env, DFun result id args ss')


addSigs :: Env -> [Def] -> Err Env
addSigs e [] = do
  lookupFun e (Id "main")
  return e
addSigs env ((DFun result (Id "main") args ss) : ds) = do
  if (result /= Type_int)
    then Bad "main must have type int"
    else do
      if (args /= [])
        then Bad "main must have no arguments"
        else do
          env' <- updateFun env (Id "main") (argsToType args, result)
          addSigs env' ds
addSigs env ((DFun result id args ss) : ds) = do
  env' <- updateFun env id (argsToType args, result)
  addSigs env' ds



argsToType :: [Arg] -> [Type]
argsToType []                  = []
argsToType ((ADecl t id) : as) = t : argsToType as

updateVars :: Env -> [Id] -> Type -> Err Env
updateVars env ids typ = foldM (\env id -> updateVar env id typ) env ids

updateArgs :: Env -> [Arg] -> Err Env
updateArgs env []                      = return env
updateArgs env ((ADecl typ id) : args) = do
  env' <- updateVar env id typ
  updateArgs env' args
