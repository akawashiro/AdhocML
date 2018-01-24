module Type where

import Parse
import Control.Monad.State

data Type = TInt | TBool | TFun Type Type | TVar Int deriving (Eq, Show)

type Restriction = (Type, Type)
type Substituition = [Restriction]
-- First element of each element in TypeEnvironment is EVariable
type TypeEnvironment = [(Expr, Type)]
type MakeSubstituition = State Int

getNewTVar :: MakeSubstituition Type
getNewTVar = do
  i <-get
  put (i+1)
  return (TVar i)

-- First argument is bounder to given expr
exprToSubstituition :: TypeEnvironment -> Type -> Expr -> MakeSubstituition (TypeEnvironment, Substituition)
exprToSubstituition env t e = case e of
  EBool _ -> return (env,[(t, TBool)])
  EInt _ -> return (env,[(t, TInt)])
  EIf e1 e2 e3 -> do
    (env1, sub1) <- exprToSubstituition env TBool e1
    (env2, sub2) <- exprToSubstituition env1 t e2
    (env3, sub3) <- exprToSubstituition env2 t e3
    return (env3,(sub1 ++ sub2 ++ sub3))
  ELet s1 e1 e2 -> do
    tv1 <- getNewTVar
    tv2 <- getNewTVar
    let env1 = (EVariable s1, tv1) : env
    (env2, sub1) <- exprToSubstituition env1 tv2 e1
    (env3, sub2) <- exprToSubstituition env2 t e2
    return (env3, sub2)
  EFun s1 e1 -> do
    tv1 <- getNewTVar
    tv2 <- getNewTVar
    (env1, sub1) <- exprToSubstituition ((EVariable s1, tv1) : env) tv2 e1
    return (env1, (t, TFun tv1 tv2) : sub1)
  EApp e1 e2 -> do
    tv1 <- getNewTVar
    tv2 <- getNewTVar
    (env1, sub1) <- exprToSubstituition env (TFun tv2 tv1) e1
    (env2, sub2) <- exprToSubstituition env1 tv2 e2
    return (env2, sub1 ++ sub2)
  ELetRec s1 s2 e1 e2 -> do


