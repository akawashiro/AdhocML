module Adhoc where

import Parse

branchCode :: Expr -> [Expr]
branchCode e = case e of
  EInt _ -> [e]
  EBool -> [e]
  EBinOp op e1 e2 -> do
    e1
  EIf 
