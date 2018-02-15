module Main where

import Parse
import Eval
import Z3
import Adhoc
import Control.Monad
import Data.List(intersperse)

printResult (Right xs) = printResult' xs
printResult (Left s) = show s

printResult' [] = ""
printResult' (Right x:xs) = show x ++ "\n" ++ printResult' xs
printResult' (Left x:xs) = show x ++ "\n" ++ printResult' xs

printEnvAndSubs [] = "" 
printEnvAndSubs (a:as) = show a ++ "\n" ++ printEnvAndSubs as

main :: IO ()
main = do
  input <- getContents
  putStrLn "------------input----------------"
  putStrLn input

  let ast = stringToProgram input
  putStrLn "------------AST----------------"
  putStrLn $ g $ ast

  let sub = (liftM (liftM exprToSubstituition)) ast
  putStrLn "------------Type substitution ----------------"
  print sub

  let z3 = either (\x->"Parse Failed\n") (exprToZ3.head) ast
  putStrLn "------------Z3 code ----------------"
  putStrLn z3

    where
      f err (Right s) = s ++ "\n"
      f err (Left _) = err
      g (Right l) = (concat $ intersperse "\n" $ map show l) ++ "\n"
      g (Left _) = "Parse failed."
