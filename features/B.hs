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
    | LHsIf   Expr Expr Expr
    | LHsEq   Expr Expr
    deriving Eq

data Val
  = LHsVInt Int
  | LHsVBool Bool
  deriving (Eq, Show)

type Env = Map.Map Var Val

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
eval env (LHsIf i t e) =
    let b = evalBool env i in
    if b then eval env t else eval env e

eval env (LHsEq e1 e2) = LHsVBool (eval env e1 == eval env e2)

evalInt :: Env -> Expr -> Int
evalInt env e = case eval env e of
    LHsVInt n  -> n
    LHsVBool _ -> error "type error: expected an integer but got a Boolean"

evalBool :: Env -> Expr -> Bool
evalBool env e = case eval env e of
    LHsVBool b -> b
    LHsVInt _  -> error "type error: expected a Boolean but got an integer"