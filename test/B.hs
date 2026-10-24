module Expr where

import Data.Map (Map)

import qualified Data.Map as Map

data Expr
    = LHsInt  Int
    | LHsBool Bool
    | LHsAdd  Expr Expr
    | LHsMul  Expr Expr
    | LHsOr   Expr Expr
    | LHsAnd  Expr Expr

data Val
  = LHsVInt Int
  | LHsVBool Bool

type Env = Map.Map String Val

eval :: Env -> Expr -> Val
eval _ (LHsInt  n) = LHsVInt  n
eval _ (LHsBool b) = LHsVBool b

eval env (LHsAdd e1 e2) =
    let n = evalInt env e1
        m = evalInt env e2
    in LHsVInt (n + m)
eval env (LHsMul e1 e2) =
    let n = evalInt env e1
        m = evalInt env e2
    in LHsVInt (n * m)

eval env (LHsOr e1 e2) =
    let b = evalBool env e1
        c = evalBool env e2
    in LHsVBool (b || c)
eval env (LHsAnd e1 e2) =
    let b = evalBool env e1
        c = evalBool env e2
    in LHsVBool (b && c)

evalInt :: Env -> Expr -> Int
evalInt env e = case eval env e of
    LHsVInt n  -> n
    LHsVBool _ -> error "type error: expected an integer but got a Boolean"

evalBool :: Env -> Expr -> Bool
evalBool env e = case eval env e of
    LHsVBool b -> b
    LHsVInt _  -> error "type error: expected a Boolean but got an integer"