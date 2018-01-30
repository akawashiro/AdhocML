module Adhoc where

import Parse
import Data.Map as M
import Control.Monad
import Control.Monad.State

type NameToVarination = M.Map String [String]
type Variation = StateT NameToVarination []

valiationName :: String -> Variation String
valiationName s = do
  m <- get
  let vs = M.lookup s m
  case vs of
    Just vs' -> lift vs'
    Nothing -> return s

-- return value is translated name
registerName :: String -> Variation String
registerName s = do
  m <- get
  if M.member s m
  then do
    let s' = s ++ "_" ++ show (length (m M.! s))
    let m' = M.update (\l -> Just (s':l)) s m
    put m
    return s'
  else do
    let s' = s ++ "_0"
    put (M.insert s [s'] m)
    return s'

branchCode :: Expr -> [Expr]
branchCode e = evalStateT (branchCode' e) M.empty

branchCode' :: Expr -> Variation Expr
branchCode' e = case e of
  EInt _ -> return e
  EBool _ -> return e
  EBinOp op e1 e2 -> do
    e1' <- branchCode' e1
    e2' <- branchCode' e2
    return (EBinOp op e1' e2')
  EIf e1 e2 e3 -> do
    e1' <- branchCode' e1
    e2' <- branchCode' e2
    e3' <- branchCode' e3
    return (EIf e1' e2' e3')
  ELet s1 e1 e2 -> do
    s1' <- registerName s1
    e1' <- branchCode' e1
    e2' <- branchCode' e2
    return (ELet s1' e1' e2')
  EFun s1 e1 -> do
    s1' <- registerName s1
    e1' <- branchCode' e1
    return (EFun s1' e1)
  EApp e1 e2 -> do
    e1' <- branchCode' e1
    e2' <- branchCode' e2
    return (EApp e1' e2')
  ELetRec s1 s2 e1 e2 -> do
    s1' <- registerName s1
    s2' <- registerName s2
    e1' <- branchCode' e1
    e2' <- branchCode' e2
    return (ELetRec s1' s2' e1' e2')
  EVariable s1 -> do
    s1' <- valiationName s1
    return (EVariable s1')
