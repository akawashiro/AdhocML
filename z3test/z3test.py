# Copyright (c) Microsoft Corporation 2015, 2016

# The Z3 Python API requires libz3.dll/.so/.dylib in the
# PATH/LD_LIBRARY_PATH/DYLD_LIBRARY_PATH
# environment variable and the PYTHON_PATH environment variable
# needs to point to the `python' directory that contains `z3/z3.py'
# (which is at bin/python in our binary releases).

# If you obtained example.py as part of our binary release zip files,
# which you unzipped into a directory called `MYZ3', then follow these
# instructions to run the example:

# Running this example on Windows:
# set PATH=%PATH%;MYZ3\bin
# set PYTHONPATH=MYZ3\bin\python
# python example.py

# Running this example on Linux:
# export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:MYZ3/bin
# export PYTHONPATH=MYZ3/bin/python
# python example.py

# Running this example on OSX:
# export DYLD_LIBRARY_PATH=$DYLD_LIBRARY_PATH:MYZ3/bin
# export PYTHONPATH=MYZ3/bin/python
# python example.py


from z3 import Datatype, IntSort, CreateDatatypes, \
        simplify, Consts, solve, Distinct, Solver, Const, Or

MLType = Datatype('MLType')
MLType.declare('int')
MLType.declare('bool')
MLType.declare('fun', ('arg', MLType), ('body', MLType))
MLType = MLType.create()
s = Solver()
ty1 = Const('ty1', MLType)
ty2 = Const('ty2', MLType)
# ty3 = Const('ty2', MLType)
ty3 = MLType.fun(MLType.int, ty2)
s.add(ty1 == ty3)
print(s.check())
print(s.model())


TreeList = Datatype('TreeList')
Tree = Datatype('Tree')
Tree.declare('leaf', ('val', IntSort()))
Tree.declare('node', ('left', TreeList), ('right', TreeList))
TreeList.declare('nil')
TreeList.declare('cons', ('car', Tree), ('cdr', TreeList))

Tree, TreeList = CreateDatatypes(Tree, TreeList)

t1 = Tree.leaf(10)
tl1 = TreeList.cons(t1, TreeList.nil)
t2 = Tree.node(tl1, TreeList.nil)
print(t2)
print(simplify(Tree.val(t1)))

t1, t2, t3 = Consts('t1 t2 t3', TreeList)

solve(Distinct(t1, t2, t3))

# Declare a List of integers
# List = z3.Datatype('List')
# Constructor cons: (Int, List) -> List
# List.declare('cons', ('car', z3.IntSort()), ('cdr', List))
# Constructor nil: List
# List.declare('nil')
# List = List.create()
# print(z3.is_sort(List))
# cons = List.cons
# car = List.car
# cdr = List.cdr
# nil = List.nil
# cons, car and cdr are function declarations, and nil a constant
# print(z3.is_func_decl(cons))
# print(z3.is_expr(nil))
#
# l1 = cons(10, cons(20, nil))
# print(l1)
# print(z3.simplify(cdr(l1)))
# print(z3.simplify(car(l1)))
# print(z3.simplify(l1 == nil))
#
# x = z3.Real('x')
# s = z3.Solver()
# s.add(z3.Or(x == 0, x == 1))
# print(s.check())
# print(s.model())

