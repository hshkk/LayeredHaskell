module Expr where

data Decl
    = LHsData Con [(Con, [Con])]
    deriving Eq 