# hMLInterpreter
Mini ML interpreter made by Haskell.

## Install
git clone git@github.com:akawashiro/hMLInterpreter.git && stack init && stack build

## Usage
You can test this ML interpreter easily by run test.sh.
test.sh evaluate test.ml line by line.
If you can compile successfully, test.ml should output following.
```Shell
------------input----------------
fun x -> x + 10;;
fun x -> x;;
let f = fun x -> fun y -> x + y in f;;
let rec f = fun x -> if 10 < x then 1 else  x * f (x + 1) in f 1;;
let f = 10 in f;;
2 * 2;;
19;;
let f = fun x -> x + 10 in f;;
let f = fun x -> x in f;;
let f = fun x -> x in if f True then f 10 else 1;;

------------AST----------------
EFun "x" (EBinOp Plus (EVariable "x") (EInt 10))
EFun "x" (EVariable "x")
ELet "f" (EFun "x" (EFun "y" (EBinOp Plus (EVariable "x") (EVariable "y")))) (EVariable "f")
ELetRec "f" "x" (EIf (EBinOp Lt (EInt 10) (EVariable "x")) (EInt 1) (EBinOp Mult (EVariable "x") (EApp (EVariable "f") (EBinOp Plus (EVariable "x") (EInt 1))))) (EApp (EVariable "f") (EInt 1))
ELet "f" (EInt 10) (EVariable "f")
EBinOp Mult (EInt 2) (EInt 2)
EInt 19
ELet "f" (EFun "x" (EBinOp Plus (EVariable "x") (EInt 10))) (EVariable "f")
ELet "f" (EFun "x" (EVariable "x")) (EVariable "f")
ELet "f" (EFun "x" (EVariable "x")) (EIf (EApp (EVariable "f") (EBool True)) (EApp (EVariable "f") (EInt 10)) (EInt 1))

------------type check----------------
Just ([(TVar 1,TInt),(TVar 2,TInt),(TVar 0,TFun TInt TInt)],TFun TInt TInt)
Just ([(TVar 1,TVar 2),(TVar 0,TFun (TVar 2) (TVar 2))],TFun (TVar 2) (TVar 2))
Just ([(TVar 1,TFun TInt (TFun TInt TInt)),(TVar 3,TFun TInt TInt),(TVar 2,TInt),(TVar 4,TInt),(TVar 5,TInt),(TVar 0,TFun TInt (TFun TInt TInt))],TFun TInt (TFun TInt TInt))
Just ([(TVar 4,TInt),(TVar 1,TInt),(TVar 2,TInt),(TVar 3,TInt)],TInt)
Just ([(TVar 1,TInt),(TVar 0,TInt)],TInt)
Just ([(TVar 0,TInt)],TInt)
Just ([(TVar 0,TInt)],TInt)
Just ([(TVar 1,TFun TInt TInt),(TVar 3,TInt),(TVar 2,TInt),(TVar 0,TFun TInt TInt)],TFun TInt TInt)
Just ([(TVar 3,TVar 4),(TVar 1,TFun (TVar 4) (TVar 4)),(TVar 2,TVar 4),(TVar 0,TFun (TVar 4) (TVar 4))],TFun (TVar 4) (TVar 4))
Just ([(TVar 4,TBool),(TVar 3,TVar 7),(TVar 6,TInt),(TVar 5,TVar 7),(TVar 1,TFun (TVar 7) (TVar 7)),(TVar 2,TVar 7),(TVar 0,TInt)],TInt)

------------result----------------
(VProc "x",EBinOp Plus (EVariable "x") (EInt 10),[...])
(VProc "x",EVariable "x",[...])
(VProc "x",EFun "y" (EBinOp Plus (EVariable "x") (EVariable "y")),[...])
(VInt 3628800)
(VInt 10)
(VInt 4)
(VInt 19)
(VProc "x",EBinOp Plus (EVariable "x") (EInt 10),[...])
(VProc "x",EVariable "x",[...])
(VInt 10)

------------result----------------
(VProc "x",EBinOp Plus (EVariable "x") (EInt 10),[...])
(VProc "x",EVariable "x",[...])
(VProc "x",EFun "y" (EBinOp Plus (EVariable "x") (EVariable "y")),[...])
(VInt 3628800)
(VInt 10)
(VInt 4)
(VInt 19)
(VProc "x",EBinOp Plus (EVariable "x") (EInt 10),[...])
(VProc "x",EVariable "x",[...])
(VInt 10)
```
