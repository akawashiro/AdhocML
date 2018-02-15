#! /bin/sh

stack build && cat z3.ml | stack exec hMLInterpreter-Z3-exe > z3tmp.py
cat z3tmp.py
echo "# ------------Result of Z3----------------"
echo "# ty0 is the type of return value."
python z3tmp.py
