# AdhocML
Mini ML interpreter which support adhoc polymophism using brute force method.

## Install
```shell
git clone git@github.com:akawashiro/hMLInterpreter.git
stack init
stack build
```

## Usage
You can test this ML interpreter easily by run adhoc.sh.
adhoc.sh evaluate adhoc.ml line by line.
If you can compile successfully, adhoc.sh will output following.
```Shell
------------input----------------
let b = True in let b = 10 in if b then b else 100;;

------------AST----------------
ELet "b" (EBool True) (ELet "b" (EInt 10) (EIf (EVariable "b") (EVariable "b") (EInt 100)))

------------Adhoc translation----------------
ELet "b_0" (EBool True) (ELet "b_1" (EInt 10) (EIf (EVariable "b_1") (EVariable "b_1") (EInt 100)))
ELet "b_0" (EBool True) (ELet "b_1" (EInt 10) (EIf (EVariable "b_1") (EVariable "b_0") (EInt 100)))
ELet "b_0" (EBool True) (ELet "b_1" (EInt 10) (EIf (EVariable "b_0") (EVariable "b_1") (EInt 100)))
ELet "b_0" (EBool True) (ELet "b_1" (EInt 10) (EIf (EVariable "b_0") (EVariable "b_0") (EInt 100)))

------------type check----------------
Nothing
Nothing
Just ([(TVar 2,TInt),(TVar 1,TBool),(TVar 0,TInt)],TInt)
Nothing

------------result----------------
Nothing
Nothing
Just (Right (VInt 10))
Nothing

```

## How it works
This interpreter take a program which contains adhoc polymophism(overload). It branches a program for all candiates which replace all overloaded function with its candiates functions. For example, this code
```OCaml
let b = True in let b = 10 in if b then b else 100;;
```
is branced to 4 codes.
```OCaml
let b_0 = True in let b_1 = 10 in if b_0 then b_0 else 100;;
let b_0 = True in let b_1 = 10 in if b_0 then b_1 else 100;;
let b_0 = True in let b_1 = 10 in if b_1 then b_0 else 100;;
let b_0 = True in let b_1 = 10 in if b_1 then b_1 else 100;;
```
Type checking is only succeeded in the third code. And only third code is executed.
