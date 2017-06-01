# hMLInterpreter

Mini ML interpreter made by Haskell.

## Install

git clone git@github.com:akawashiro/hMLInterpreter.git && stack init && stack build

## Usage

You can test this ML interpreter easily by run test.sh.
test.sh evaluate test.ml line by line.
If you can compile successfully, test.ml should output following.
```Shell
VInt 20
VInt 3628800
"Cannot lookup variable."
```
