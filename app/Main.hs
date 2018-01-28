module Main where

import Parse
import Eval
import Type
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
  putStrLn "------------AST----------------"
  putStrLn $ g $ stringToProgram input
  putStrLn "------------type check----------------"
  putStr $ f "Type check failed" $ printEnvAndSubs `liftM` (map exprToSubstituition) `liftM` stringToProgram input
  putStrLn "------------result----------------"
  putStrLn $ printResult $ programToExVal `liftM` stringToProgram input
  putStrLn "------------result----------------"
  putStrLn $ printResult $ (programToExVal `liftM` stringToProgram input)
    where
      f err (Right s) = s ++ "\n"
      f err (Left _) = err
      g (Right l) = (concat $ intersperse "\n" $ map show l) ++ "\n"
      g (Left _) = "Parse failed."
