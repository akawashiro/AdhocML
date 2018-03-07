# AdhocML
Mini ML interpreter which support adhoc polymophism using brute force method.

## Install
```shell
git clone git@github.com:akawashiro/hMLInterpreter.git
stack init
stack build
```

## Usage
You can test this ML interpreter easily by run `adhoc.sh`.
`adhoc.sh` evaluate `adhoc.ml` line by line.
If you can compile successfully, `adhoc.sh` will output following.
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

## Improve brute force search using SMT solver
Brute force is not efficient approach to solve the type checking problem.
So I imporve type checking using SMT solver.
You can check how this approach works by runnnng `z3.sh`.
`z3.sh` will output following.

```Shell
# ------------input----------------
# let b = True in let b = 10 in if b then b else 100;;
# ------------AST----------------
# ELet "b" (EBool True) (ELet "b" (EInt 10) (EIf (EVariable "b") (EVariable "b") (EInt 100)))

# ------------Type substitution ----------------
# Right [[(TVar 0,[TVar 0]),(TVar 1,[TBool]),(TVar 0,[TVar 0]),(TVar 2,[TInt]),(TBool,[TBool]),(TVar 0,[TInt]),(TBool,[TVar 2,TVar 1]),(TVar 0,[TVar 2,TVar 1]),(TVar 0,[TInt])]]
# ------------Z3 code ----------------
from z3 import Datatype, Solver, Const, Or
MLType = Datatype('MLType')
MLType.declare('a')
MLType.declare('b')
MLType.declare('int')
MLType.declare('bool')
MLType.declare('fun', ('arg', MLType), ('body', MLType))
MLType = MLType.create()
s = Solver()
ty0 = Const('ty0', MLType)
ty1 = Const('ty1', MLType)
ty2 = Const('ty2', MLType)
ty3 = ty0
s.add(Or(ty0 == ty3))
ty4 = MLType.bool
s.add(Or(ty1 == ty4))
ty5 = ty0
s.add(Or(ty0 == ty5))
ty6 = MLType.int
s.add(Or(ty2 == ty6))
ty7 = MLType.bool
s.add(Or(MLType.bool == ty7))
ty8 = MLType.int
s.add(Or(ty0 == ty8))
ty9 = ty2
ty10 = ty1
s.add(Or(MLType.bool == ty9, MLType.bool == ty10))
ty11 = ty2
ty12 = ty1
s.add(Or(ty0 == ty11, ty0 == ty12))
ty13 = MLType.int
s.add(Or(ty0 == ty13))
print(s.check())
print(s.model())

# ------------Result of Z3----------------
# ty0 is the type of return value.
sat
[ty1 = bool, ty0 = int, ty2 = int]
```
