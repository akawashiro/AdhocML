module Main where

import Parse
import Eval
import Type
import Control.Monad

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
  print "------------input----------------"
  putStrLn input
  print "------------AST----------------"
  print $ stringToProgram input
  print "------------type check----------------"
  print $ printEnvAndSubs `liftM` (map exprToSubstituition) `liftM` stringToProgram input
  print "------------result----------------"
  print $ programToExVal `liftM` stringToProgram input
  print "------------result----------------"
  putStrLn $ printResult $ (programToExVal `liftM` stringToProgram input)

