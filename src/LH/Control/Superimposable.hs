module LH.Control.Superimposable where

import LH.Data.Fuzzy (Haskell, Happy)

infixr 6 -->

class Superimposable a where
    (-->) :: a -> a -> a

instance Superimposable Haskell where
    h1 --> h2 = undefined

instance Superimposable Happy where
    h1 --> h2 = undefined