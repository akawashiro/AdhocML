module Z3 where
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.List(nub,intersperse)
import Data.Maybe(fromMaybe)
import Parse
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.List(nub)
import Data.Maybe(fromMaybe)

data Type = TInt | TBool | TFun Type Type | TVar TVarIndex deriving (Eq, Show)

type TVarIndex = Int
type Restriction = (Type, [Type])
type Substituition = [Restriction]
-- First element of each element in TypeSchemeEnvironment is EVariable
type TypeEnvironment = [(Expr, Type)]
type MakeSubstituition = (State TVarIndex)

lookupTypeEnv :: Expr -> TypeEnvironment -> MakeSubstituition [Type]
lookupTypeEnv e [] = return []
lookupTypeEnv e1 ((e2,t):r)
  | e1 == e2 = do
    ts <- lookupTypeEnv e1 r
    return (t:ts)
  | otherwise = lookupTypeEnv e1 r

getNewTVarIndex :: MakeSubstituition TVarIndex
getNewTVarIndex = do
  i <-get
  put (i+1)
  return i

exprToZ3 :: Expr -> String
exprToZ3 e = z3init ++ varInit 0 nvarInML ++ subsz3 ++ z3end ++ check
  where
    subs = fst $ evalState (exprToSubstituition' [] (TVar 0) e) 1
    nvarInML = execState (exprToSubstituition' [] (TVar 0) e) 1
    (subsz3,nvar) = runState (subToZ3 subs) nvarInML
    check = evalState (z3CheckUnique nvarInML) nvar

z3CheckUnique :: Int -> MakeSubstituition String
z3CheckUnique nvarInML = do
  strs <- mapM f [0..(nvarInML-1)]
  return $ "\n" ++ concat (map fst strs ++ map snd strs) ++ z3CheckUniqueCode
    where
      f x = do
        a <- getNewTVarIndex
        return $ ("ty" ++ show a ++ " = s.model()[ty" ++ show x ++ "]\n", "s.add(Not(" ++ "ty" ++ show a ++ " == ty" ++ show x ++ "))\n")

z3CheckUniqueCode = "\
  \print(\"Second, I check the uniqueness of the solution.\")\n\
  \print(s.check())\n\
  \print(s.model())\n"



typeToZ3 :: Type -> String
typeToZ3 TInt = "MLType.int"
typeToZ3 TBool = "MLType.bool"
typeToZ3 (TFun s t) = "MLType.fun(" ++ typeToZ3 s ++ "," ++ typeToZ3 t ++ ")"
typeToZ3 (TVar i) = "ty" ++ show i

subToZ3 :: Substituition -> State Int String
subToZ3 sub = fmap concat $ mapM resToZ3 sub

resToZ3 :: Restriction -> State Int String
resToZ3 (s,ts) = do
  i <- get
  let tys = map (\i -> "ty" ++ show i) [i..i + length ts - 1]
  let tyi = concat $ map (\(a,t) -> a ++ " = " ++ typeToZ3 t ++ "\n") (zip tys ts)
  put (i + length ts)
  let or = "s.add(Or(" ++ (concat (intersperse ", " (map (\a -> typeToZ3 s ++ " == " ++ a) tys))) ++ "))\n"
  return (tyi ++ or)

varInit :: Int -> Int -> String
varInit nvars nvare = concat $ (map (\x -> "ty" ++ show x ++ " = Const('ty" ++ show x ++ "', MLType)\n") [nvars..nvare-1])

z3init :: String
z3init = "\
  \from z3 import Datatype, Solver, Const, Or, Not\n\
  \MLType = Datatype('MLType')\n\
  \MLType.declare('a')\n\
  \MLType.declare('b')\n\
  \MLType.declare('int')\n\
  \MLType.declare('bool')\n\
  \MLType.declare('fun', ('arg', MLType), ('body', MLType))\n\
  \MLType = MLType.create()\n\
  \s = Solver()\n"

z3end :: String
z3end = "\
  \print(\"First, I try to find a solution.\")\n\
  \print(s.check())\n\
  \print(s.model())\n"

exprToSubstituition :: Expr -> Substituition
exprToSubstituition e = fst a
  where a = evalState (exprToSubstituition' [] (TVar 0) e) 1
-- First argument is bounder to given expr
exprToSubstituition' :: TypeEnvironment -> Type -> Expr -> MakeSubstituition (Substituition, Type)
exprToSubstituition' env t e = case e of
  EInt _ -> return ([(t, [TInt])], TInt)
  EBool _ -> return ([(t, [TBool])], TBool)
  EBinOp op e1 e2 -> do
    (sub1, t1) <- exprToSubstituition' env TInt e1
    (sub2, t2) <- exprToSubstituition' env TInt e2
    let sub3 = (rt, [t]) : (t1, [TInt]) : (t2, [TInt]) : sub1 ++ sub2
    return $ (sub3, rt)
      where rt = if op == Lt then TBool else TInt
  EIf e1 e2 e3 -> do
    (sub1, t1) <- exprToSubstituition' env TBool e1
    (sub2, t2) <- exprToSubstituition' env t e2
    (sub3, t3) <- exprToSubstituition' env t e3
    let sub4 = (t1, [TBool]) : (t2, [t3]) : sub1 ++ sub2 ++ sub3
    return $ (sub4, t2)
  ELet s1 e1 e2 -> do
    tv1 <- getNewTVarIndex
    (sub1, t1) <- exprToSubstituition' env (TVar tv1) e1
    let env1 = (EVariable s1, (TVar tv1)) : env
    (sub2, t2) <- exprToSubstituition' env1 t e2
    let sub3 = (t, [t2]) : sub1 ++ sub2
    return $ (sub3, t)
  EFun s1 e1 -> do
    tv1 <- getNewTVarIndex
    tv2 <- getNewTVarIndex
    let env1 = (EVariable s1, (TVar tv1)) : env
    (sub1, t1) <- exprToSubstituition' env1 (TVar tv2) e1
    let sub2 = (t, [TFun (TVar tv1) (TVar tv2)]) : (t1, [TVar tv2]) : sub1
    return $ (sub2, (TFun (TVar tv1) (TVar tv2)))
  EApp e1 e2 -> do
    tv2 <- getNewTVarIndex
    (sub1, t1) <- exprToSubstituition' env (TFun (TVar tv2) t) e1
    (sub2, t2) <- exprToSubstituition' env (TVar tv2) e2
    let sub3 = sub1 ++ sub2
    return $ (sub3, t2)
  ELetRec s1 s2 e1 e2 -> do
    tv1 <- getNewTVarIndex
    tv2 <- getNewTVarIndex
    let env1 = (EVariable s1, (TFun (TVar tv1) (TVar tv2))) : (EVariable s2, (TVar tv1)) : env
    (sub1, t1) <- exprToSubstituition' env1 (TVar tv2) e1
    let sub2 = sub1
    let t2 = (TFun (TVar tv1) t1)
    let env2 = (EVariable s1, t2) : env
    (sub2, t3) <- exprToSubstituition' env1 t e2
    let sub3 = (sub1 ++ sub2)
    return $ (sub3, t3)
  EVariable s1 -> do
    ts <- lookupTypeEnv (EVariable s1) env
    return ([(t, ts)], t)
