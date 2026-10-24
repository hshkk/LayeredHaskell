module LH.Data.Dependency where

import LH.Data.Feature (FeatureName)

data Dependency = Has FeatureName | All [Dependency] | Any [Dependency]

infixr 3 /\
(/\) :: Dependency -> Dependency -> Dependency
a /\ b = All [a, b]

infixr 2 \/
(\/) :: Dependency -> Dependency -> Dependency
a \/ b = Any [a, b]

-- disjunctive normal form for dependencies
dnf :: Dependency -> [[FeatureName]]
dnf (Has name) = [[name]]
dnf (Any deps) = concatMap dnf deps
dnf (All deps) = foldr (liftA2 (++) . dnf) [[]] deps

-- conjunctive normal form for dependencies
cnf :: Dependency -> [[FeatureName]]
cnf (Has name) = [[name]]
cnf (Any deps) = foldr (liftA2 (++) . cnf) [[]] deps
cnf (All deps) = concatMap cnf deps