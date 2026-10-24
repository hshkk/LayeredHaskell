module LH.Data.Dependency where

import LH.Data.Assoc (AssocName)

data Dependency = Has AssocName | All [Dependency] | Any [Dependency]

infixr 3 /\
(/\) :: Dependency -> Dependency -> Dependency
a /\ b = All [a, b]

infixr 2 \/
(\/) :: Dependency -> Dependency -> Dependency
a \/ b = Any [a, b]

concatInner :: [[a]] -> [[a]] -> [[a]]
concatInner = liftA2 (++)

-- disjunctive normal form for dependencies
dnf :: Dependency -> [[AssocName]]
dnf (Has name) = [[name]]
dnf (Any deps) = concatMap dnf deps
dnf (All deps) = foldr (concatInner . dnf) [[]] deps

-- conjunctive normal form for dependencies
cnf :: Dependency -> [[AssocName]]
cnf (Has name) = [[name]]
cnf (Any deps) = foldr (concatInner . cnf) [[]] deps
cnf (All deps) = concatMap cnf deps