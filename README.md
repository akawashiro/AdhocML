# hMLInterpreter
Mini ML interpreter made by Haskell.

## Install
git clone git@github.com:akawashiro/hMLInterpreter.git && stack init && stack build

## Usage
You can test this ML interpreter easily by run test.sh.
test.sh evaluate test.ml line by line.
If you can compile successfully, test.ml should output following.
```Shell
"------------input----------------"
let f = fun x -> x in f;;
fun x -> x;;
let f = fun x -> x in if f True then f 10 else 1;;
let rec f = fun x -> if 10 < x then 1 else  x * f (x + 1) in f 1;;
let f = 10 in f;;
2 * 2;;
19;;
let f = fun x -> fun y -> x + y in f;;

"------------AST----------------"
Right [ELet "f" (EFun "x" (EVariable "x")) (EVariable "f"),EFun "x" (EVariable "x"),ELet "f" (EFun "x" (EVariable "x")) (EIf (EApp (EVariable "f") (EBool True)) (EApp (EVariable "f") (EInt 10)) (EInt 1)),ELetRec "f" "x" (EIf (EBinOp Lt (EInt 10) (EVariable "x")) (EInt 1) (EBinOp Mult (EVariable "x") (EApp (EVariable "f") (EBinOp Plus (EVariable "x") (EInt 1))))) (EApp (EVariable "f") (EInt 1)),ELet "f" (EInt 10) (EVariable "f"),EBinOp Mult (EInt 2) (EInt 2),EInt 19,ELet "f" (EFun "x" (EFun "y" (EBinOp Plus (EVariable "x") (EVariable "y")))) (EVariable "f")]
"------------type check----------------"
Just ([(TVar 3,TVar 4),(TVar 1,TFun (TVar 4) (TVar 4)),(TVar 2,TVar 4),(TVar 0,TFun (TVar 4) (TVar 4))],TFun (TVar 4) (TVar 4))
Just ([(TVar 1,TVar 2),(TVar 0,TFun (TVar 2) (TVar 2))],TFun (TVar 2) (TVar 2))
Just ([(TVar 4,TBool),(TVar 3,TVar 7),(TVar 6,TInt),(TVar 5,TVar 7),(TVar 1,TFun (TVar 7) (TVar 7)),(TVar 2,TVar 7),(TVar 0,TInt)],TInt)
Just ([(TVar 4,TInt),(TVar 2,TInt),(TVar 3,TInt)],TInt)
Just ([(TVar 1,TInt),(TVar 0,TInt)],TInt)
Just ([(TVar 0,TInt)],TInt)
Just ([(TVar 0,TInt)],TInt)
Just ([(TVar 4,TVar 7),(TVar 2,TVar 6),(TVar 1,TFun (TVar 6) (TFun (TVar 7) TInt)),(TVar 3,TFun (TVar 7) TInt),(TVar 5,TInt),(TVar 0,TFun (TVar 6) (TFun (TVar 7) TInt))],TFun (TVar 6) (TFun (TVar 7) TInt))
"------------result----------------"
Right [Right (VProc "x",EVariable "x",[...]),Right (VProc "x",EVariable "x",[...]),Right (VInt 10),Right (VInt 3628800),Right (VInt 10),Right (VInt 4),Right (VInt 19),Right (VProc "x",EFun "y" (EBinOp Plus (EVariable "x") (EVariable "y")),[...])]
"------------result----------------"
(VProc "x",EVariable "x",[...])
(VProc "x",EVariable "x",[...])
(VInt 10)
(VInt 3628800)
(VInt 10)
(VInt 4)
(VInt 19)
(VProc "x",EFun "y" (EBinOp Plus (EVariable "x") (EVariable "y")),[...])
```
