module Type where

import Parse
import Control.Monad.State

data Type = TInt | TBool | TFun Type Type | TVar Int deriving (Eq, Show)

type Restriction = (Type, Type)
type Substituition = [Restriction]
type MakeSubst = State Int

getNewTVar :: MakeSubst Type
getNewTVar = do
  i <-get
  put (i+1)
  return (TVar i)

-- First argument is bounder to given expr
exprToSubstituition :: Type -> Expr -> MakeSubst Substituition
exprToSubstituition t e = case e of
  EBool _ -> return [(t, TBool)]
  EInt _ -> return [(t, TInt)]
  EIf e1 e2 e3 -> do
    s1 <- exprToSubstituition TBool e1
    s2 <- exprToSubstituition t e2
    s3 <- exprToSubstituition t e3
    return (s1 ++ s2 ++ s3)

