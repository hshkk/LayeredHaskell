{- |
Module      : LH.Data.Dependency
Description : A small DSL for specifying dependencies between 'LH.Data.Feature.Feature's.
Copyright   : (c) Hangil Kim, 2026
                  Kairon Johnson, 2026
                  Leonidas Sallos, 2026
                  Oscar Abelanet, 2026
License     : MIT
Maintainer  : kimhang@oregonstate.edu
Stability   : experimental

This module provides a number of definitions aiding the specification of dependencies between 'LH.Data.Feature.Feature's.
-}
module LH.Data.Dependency (
    -- * Dependencies
    FeatureName, Dependency (..), 
    -- ** Convenient synonyms
    (/\), (\/), 
    -- ** Normalization
    dnf, cnf
    ) where

import Data.Set (Set)

import qualified Data.Set as Set

-- | The name of a 'LH.Data.Feature.Feature'.
type FeatureName = String

-- | A dependency is effectively a Boolean formula of feature names.
data Dependency 
    = None                -- ^ A dependency that can always be satisfied.
    | Has FeatureName     -- ^ A dependency on a single feature.
    | All [Dependency]    -- ^ All specified dependencies must be satisfied.
    | Any [Dependency]    -- ^ Any specified dependency can be satisfied.

-- | A convenient synonym for describing a /conjunction/ of dependencies, 
--   that is, @A '/\' B@ means @A@ /and/ @B@ must be enabled.
(/\) :: Dependency -> Dependency -> Dependency
a /\ b = All [a, b]

infixr 3 /\

-- | A convenient synonym for describing a /disjunction/ of dependencies, 
--   that is, @A '\/' B@ means @A@ /or/ @B@ must be enabled.
(\/) :: Dependency -> Dependency -> Dependency
a \/ b = Any [a, b]

infixr 2 \/

-- | Takes the cartesian product of two nested sets and unions each pair.
combine :: Ord a => Set (Set a) -> Set (Set a) -> Set (Set a)
combine xs ys = Set.map (uncurry Set.union) (Set.cartesianProduct xs ys)

-- | Computes the disjunctive normal form (DNF) represented by a 'Dependency'.
--   See 'LH.Data.Feature.impliedBy'.
--
--   DNF denotes a set of requirements, each a set of features that of which /one/ must be enabled.
dnf :: Dependency -> Set (Set FeatureName)
dnf None       = Set.empty
dnf (Has name) = Set.singleton (Set.singleton name)
dnf (Any deps) = foldMap dnf deps
dnf (All deps) = foldr (combine . dnf) (Set.singleton Set.empty) deps

-- | Computes the conjunctive normal form (CNF) represented by a 'Dependency'.
--   See 'LH.Data.Feature.dependsOn'.
--
--   CNF denotes a set of alternatives, each a set of features that must /all/ be enabled.
cnf :: Dependency -> Set (Set FeatureName)
cnf None       = Set.empty
cnf (Has name) = Set.singleton (Set.singleton name)
cnf (Any deps) = foldr (combine . cnf) (Set.singleton Set.empty) deps
cnf (All deps) = foldMap cnf deps
