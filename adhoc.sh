#! /bin/sh

stack build && cat adhoc.ml | stack exec hMLInterpreter-exe
