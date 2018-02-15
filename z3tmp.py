# ------------input----------------
# let f = fun x -> x + 11 in f;;
# ------------AST----------------
# ELet "f" (EFun "x" (EBinOp Plus (EVariable "x") (EInt 11))) (EVariable "f")

# ------------Type substitution ----------------
# Right [[(TVar 0,[TVar 0]),(TVar 1,[TFun (TVar 2) (TVar 3)]),(TInt,[TVar 3]),(TInt,[TVar 3]),(TInt,[TInt]),(TInt,[TInt]),(TInt,[TVar 2]),(TInt,[TInt]),(TVar 0,[TVar 1])]]
# ------------Z3 code ----------------
from z3 import Datatype, Solver, Const, Or
MLType = Datatype('MLType')
MLType.declare('int')
MLType.declare('bool')
MLType.declare('fun', ('arg', MLType), ('body', MLType))
MLType = MLType.create()
s = Solver()
ty0 = Const('ty0', MLType)
ty1 = Const('ty1', MLType)
ty2 = Const('ty2', MLType)
ty3 = Const('ty3', MLType)
ty4 = ty0
s.add(Or(ty0 == ty4))
ty5 = MLType.fun(ty2,ty3)
s.add(Or(ty1 == ty5))
ty6 = ty3
s.add(Or(MLType.int == ty6))
ty7 = ty3
s.add(Or(MLType.int == ty7))
ty8 = MLType.int
s.add(Or(MLType.int == ty8))
ty9 = MLType.int
s.add(Or(MLType.int == ty9))
ty10 = ty2
s.add(Or(MLType.int == ty10))
ty11 = MLType.int
s.add(Or(MLType.int == ty11))
ty12 = ty1
s.add(Or(ty0 == ty12))
print(s.check())
print(s.model())

