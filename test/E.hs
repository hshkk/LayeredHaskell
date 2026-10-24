module Expr where

import Data.Map (Map)

import qualified Data.Map as Map

data Expr
    = LHsVariable String
    | LHsConstructor String
    | LHsLet String Expr Expr
    | LHsApp Expr Expr

data Val = LHsVConstructor String [Val]

type Env = Map String Val

eval :: Env -> Expr -> Val
eval env (LHsVariable x) =
    case Map.lookup x env of
        Just v  -> v
        Nothing -> error ("unbound variable: " ++ x)
eval _ (LHsConstructor c) = LHsVConstructor c []
eval env (LHsLet x e1 e2) =
    let v = eval env e1 in
    eval (Map.insert x v env) e2 
eval env (LHsApp f e) = case eval env f of
    LHsVConstructor c vs -> LHsVConstructor c (vs ++ [eval env e])