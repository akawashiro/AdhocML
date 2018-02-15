# ------------input----------------
# let b = True in let b = 10 in if b then b else 100;;

# ------------AST----------------
# ELet "b" (EBool True) (ELet "b" (EInt 10) (EIf (EVariable "b") (EVariable "b") (EInt 100)))

# ------------Type substitution ----------------
# Right [[(TVar 0,[TVar 0]),(TVar 1,[TBool]),(TVar 0,[TVar 0]),(TVar 2,[TInt]),(TBool,[TBool]),(TVar 0,[TInt]),(TBool,[TVar 2,TVar 1]),(TVar 0,[TVar 2,TVar 1]),(TVar 0,[TInt])]]
# ------------Z3 code ----------------
from z3 import Datatype, IntSort, CreateDatatypes, simplify, Consts, solve, Distinct, Solver, Const, Or
MLType = Datatype('MLType')
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

