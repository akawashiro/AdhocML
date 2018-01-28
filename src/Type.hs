module Type where

import Parse
import Control.Monad.State

data Type = TInt | TBool | TFun Type Type | TVar TVarIndex deriving (Eq, Show)
-- First argument of TypeScheme are bounded variable.
data TypeScheme = TypeScheme [TVarIndex] Type

type TVarIndex = Int
type Restriction = (Type, Type)
type Substituition = [Restriction]
-- First element of each element in TypeSchemeEnvironment is EVariable
type TypeSchemeEnvironment = [(Expr, TypeScheme)]
type MakeSubstituition = State TVarIndex

lookupTypeSchemeEnv :: Expr -> TypeSchemeEnvironment -> MakeSubstituition TypeScheme
lookupTypeSchemeEnv e [] = do
  i <- getNewTVarIndex
  return $ TypeScheme [] (TVar i)
lookupTypeSchemeEnv e1 ((e2,t):r)
  | e1 == e2 = return t
  | otherwise = lookupTypeSchemeEnv e1 r

getNewTVarIndex :: MakeSubstituition TVarIndex
getNewTVarIndex = do
  i <-get
  put (i+1)
  return i

exprToSubstituition :: Expr -> Maybe Substituition
exprToSubstituition e = unifySubstituition s
  where
    s = evalState (exprToSubstituition' [] (TVar 0) e) 1

-- First argument is bounder to given expr
exprToSubstituition' :: TypeSchemeEnvironment -> Type -> Expr -> MakeSubstituition Substituition
exprToSubstituition' env t e = case e of
  EInt _ -> return [(t, TInt)]
  EBool _ -> return [(t, TBool)]
  EBinOp op e1 e2 -> do
    sub1 <- exprToSubstituition' env TInt e1
    sub2 <- exprToSubstituition' env TInt e2
    return $ (rt, t) : sub1 ++ sub2
      where rt = if op == Lt then TBool else TInt
  EIf e1 e2 e3 -> do
    sub1 <- exprToSubstituition' env TBool e1
    sub2 <- exprToSubstituition' env t e2
    sub3 <- exprToSubstituition' env t e3
    return $ sub1 ++ sub2 ++ sub3
  ELet s1 e1 e2 -> do
    tv1 <- getNewTVar
    let env1 = (EVariable s1, tv1) : env
    sub1 <- exprToSubstituition' env1 tv1 e1
    sub2 <- exprToSubstituition' env1 t e2
    return $ sub1 ++ sub2
  EFun s1 e1 -> do
    tv1 <- getNewTVar
    tv2 <- getNewTVar
    sub1 <- exprToSubstituition' ((EVariable s1, tv1) : env) tv2 e1
    return $ (t, TFun tv1 tv2) : sub1
  EApp e1 e2 -> do
    tv2 <- getNewTVar
    sub1 <- exprToSubstituition' env (TFun tv2 t) e1
    sub2 <- exprToSubstituition' env tv2 e2
    return $ sub1 ++ sub2
  ELetRec s1 s2 e1 e2 -> do
    tv1 <- getNewTVar
    tv2 <- getNewTVar
    tv3 <- getNewTVar
    let env1 = (EVariable s1, TFun tv1 tv2) : (EVariable s2, tv3) : env
    sub1 <- exprToSubstituition' env1 tv2 e1
    sub2 <- exprToSubstituition' env1 t e2
    return $ sub1 ++ sub2
  EVariable s1 -> do
    t1 <- lookupTypeEnv (EVariable s1) env
    return [(t1, t)]
  where 
    tsf (TypeScheme f _) = f
    tss (TypeScheme _ s) = s

freeTVar :: Type -> [Type]
freeTVar (TVar i) = [TVar i]
freeTVar (TFun t1 t2) = freeTVar t1 ++ freeTVar t2
freeTVar _ = []

-- replace all t1 in t3 with t2
replace :: Type -> Type -> Type -> Type
replace t1 t2 t3 = if t1 == t3
  then t2
  else case t3 of
    TFun t4 t5 -> TFun (replace t1 t2 t4) (replace t1 t2 t5)
    otherwise -> t3

-- replace all t1 in s with t2
replaceSubstituition :: Type -> Type -> Substituition -> Substituition
replaceSubstituition t1 t2 s = map (\(x,y) -> ((replace t1 t2 x), (replace t1 t2 y))) s

unifySubstituition :: Substituition -> Maybe Substituition
unifySubstituition s = unifySubstituition' s []
unifySubstituition' [] b = return b
unifySubstituition' ((t1, t2) : r) b
  | t1 == t2 = unifySubstituition' r b
  | otherwise = case (t1,t2) of
    ((TFun t3 t4),(TFun t5 t6)) -> unifySubstituition' ((t3,t5):(t4,t6):r) b
    (TVar t3,t4) -> if TVar t3 `elem` freeTVar t4
      then Nothing 
      else do 
        u <- unifySubstituition' (replaceSubstituition (TVar t3) t4 (r)) ((TVar t3,t4) : replaceSubstituition (TVar t3) t4 b)
        return u
    (t4, TVar t3) -> unifySubstituition' ((TVar t3,t4) : r) b
