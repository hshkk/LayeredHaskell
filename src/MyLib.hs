module MyLib where

import Data.Map (Map)
import Data.Set (Set)

type Module = String
type Import = String

type Ident  = String
type Expression = String

data Haskell 
    = Haskell { name :: Module
              , imports :: Map Module (Set Import)
              , datatypes :: Map Ident (Set Constructor)
              , functions :: Map Ident (Set Equation) }

data Happy
    = Happy { top :: Haskell
            , parsers :: Map Ident Ident
            , tokens :: Map Ident Expression
            , rules :: Map Ident (Set Production)
            , bottom :: Haskell }

type Constructor = (Ident, [Ident])
data Pattern
    = Variable Ident
    | Any
    | Constructor Constructor
type Equation = (Ident, [Pattern], Expression) 

type Production = (Ident, Expression)
