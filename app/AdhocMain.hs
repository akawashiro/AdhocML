module Main where

import Parse
import Eval
import Type
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

  let adhoc = (liftM (liftM branchCode)) ast >>= return.concat
  putStrLn "------------Adhoc translation----------------"
  putStrLn $ g adhoc

  let typeCheck = (liftM (liftM exprToSubstituition)) adhoc
  putStrLn "------------type check----------------"
  putStr $ f "Type check failed" $ printEnvAndSubs `liftM` (map exprToSubstituition) `liftM` adhoc
  let as = (liftM2 zip) typeCheck adhoc
  let h (x,y) = if x == Nothing then Nothing else Just (exprToExVal [] y)
  let res = liftM (map h) as
  putStrLn "------------result----------------"
  putStrLn $ g res
    where
      f err (Right s) = s ++ "\n"
      f err (Left _) = err
      g (Right l) = (concat $ intersperse "\n" $ map show l) ++ "\n"
      g (Left _) = "Parse failed."
