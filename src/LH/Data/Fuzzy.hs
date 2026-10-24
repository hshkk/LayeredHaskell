module LH.Data.Fuzzy where

import Data.Map (Map)
import Data.Set (Set)

type Module = String
type Import = String

type Ident  = String

data Haskell 
    = Haskell 
        { name      :: Module
        , imports   :: Map Module (Set Import)
        , datatypes :: Map Ident  (Set Constructor)
        , functions :: Map Ident  (Set Equation) }

data Constructor = Ident ::= [Ident]
data Pattern
    = Variable Ident
    | Any
    | Constructor Constructor

type Expression = String
data Equation = (Ident, [Pattern]) := Expression

data Happy
    = Happy 
        { top    :: Haskell
        , names  :: Map Ident Ident
        , tokens :: Map Ident Expression
        , rules  :: Map Ident (Set Production)
        , bottom :: Haskell }

type Production = (Ident, Expression)
