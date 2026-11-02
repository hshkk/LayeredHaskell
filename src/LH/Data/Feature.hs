{- |
Module      : LH.Data.Feature
Description : Language features and dependencies between them.
Copyright   : (c) Hangil Kim, 2026
                  Kairon Johnson, 2026
                  Leonidas Sallos, 2026
                  Oscar Abelanet, 2026
License     : MIT
Maintainer  : kimhang@oregonstate.edu
Stability   : experimental

This module provides datatype definitions for language features (and collections of them), 
and a merging operation on a set of features. 
It also provides a number of definitions supporting the specification of dependencies between features.
-}
module LH.Data.Feature (
    -- * Features
    FeatureName, ModuleName, Feature (..),
    -- ** Registries
    Registry, registry,
    -- ** Merging features
    merge,
    -- * Dependencies
    Dependency (..), 
    -- ** Convenient synonyms
    (/\), (\/), 
    -- ** Normalization
    cnf, dnf
    ) where

import Control.Monad (unless)
import Data.Map      (Map)
import Data.Set      (Set)

import qualified Data.Graph as Graph
import qualified Data.Map   as Map
import qualified Data.Set   as Set

import Happy.Frontend.AbsSyn
import Language.Haskell.Syntax

import LH.Control.Superimposition ((-->))

-- | The name of a 'LH.Data.Feature.Feature'.
type FeatureName = String

-- | The name of a Haskell module or Happy grammar (which itself denotes a Haskell module).
type ModuleName = String
 
-- | A language feature \(f\) is represented using four components: the files holding its Haskell modules,
--   at most one Happy grammar file, and two nested sets of feature names that represent what \(f\) depends on,
--   and in what cases \(f\) is automatically enabled.
--
--   'dependsOn' and 'impliedBy' use conjunctive and disjunctive normal form respectively
--   as those choices lead to the most "natural" implementation.
--
--      ['dependsOn']: ...denotes /all/ conditions holding.
--      ['impliedBy']: ...denotes /some/ condition holding.
data Feature = Feature
    { modules   :: [FilePath]                -- ^ Haskell source files.
    , grammar   :: Maybe (ModuleName, FilePath)
        -- ^ A Happy grammar, if any. 
        --   
        --   We also include the name of the Haskell module it generates 
        --   since Happy's abstract syntax doesn't provide that information.
    , dependsOn :: Set (Set FeatureName)
        -- ^ What \(f\) depends on /in conjunctive normal form/.
        --
        --   @{{A}, {B, C}}@ means @A@ and (@B@ or @C@). See 'cnf'.
    , impliedBy :: Set (Set FeatureName)
        -- ^ What features imply \(f\) /in disjunctive normal form/.
        --
        --   @{{A}, {B, C}}@ means @A@ or (@B@ and @C@). See 'dnf'.
    }
 
-- | Builds a 'Feature' from its files, what it depends on, and what implies it.
--   The descriptions are automatically converted into the respective normal forms.
feature
    :: [FilePath]                   -- ^ Haskell source files.
    -> Maybe (ModuleName, FilePath) -- ^ Happy grammar, if any.
    -> Dependency                   -- ^ What it depends on. See 'dependsOn'.
    -> Dependency                   -- ^ What implies it. See 'impliedBy'.
    -> Feature
feature modls gram depends implied = 
    Feature modls gram (cnf depends) (dnf implied)
 
-- | A registry is a set of features accompanied by its name.
type Registry = Map FeatureName Feature
 
-- | The set of /all/ features.
registry :: Registry
registry = Map.fromList
    [ ("E", feature [qualify "E/AbsSyn.hs"] 
                    (Just ("LH.Data.Feature.E.Parser", qualify "E/Parser.y")) 
                    None 
                    None)
    , ("B", feature [qualify "B/AbsSyn.hs"] 
                    (Just ("LH.Data.Feature.B.Parser", qualify "B/Parser.y")) 
                    (Has "B") 
                    None)
    , ("D", feature [qualify "D/AbsSyn.hs"] 
                    (Just ("LH.Data.Feature.D.Parser", qualify "D/Parser.y")) 
                    (Has "D") 
                    None)
    ]
    where qualify file = "src/LH/Feature/" ++ file
 
-- | Repeatedly adds to a set of feature names until nothing new gets added.
select :: Registry -> Set FeatureName -> Set FeatureName
select reg names
    | Set.size added == Set.size names = names
    | otherwise                        = select reg added
    where
        added = Set.unions [names, dependencies, implications]
        dependencies = foldMap (Set.unions . Set.filter ((== 1) . Set.size) . dependsOn)
                               (Map.restrictKeys reg names)
        implications = Map.keysSet (Map.filter (any (`Set.isSubsetOf` names) . impliedBy) reg)
 
-- | Attempt to produce the features in a valid order.
--   Specifically, every feature should occur before the features the 'dependsOn' and 'impliedBy' fields refer to.
order :: Registry -> Set FeatureName -> Either String [FeatureName]
order reg names = fmap reverse (traverse acyclic (Graph.stronglyConnComp nodes))
    where
        nodes = [ (name, name, Set.toList (Set.unions (dependsOn feat <> impliedBy feat)))
                | (name, feat) <- Map.toList (Map.restrictKeys reg names) ]
        acyclic (Graph.AcyclicSCC name)   = Right name
        acyclic (Graph.CyclicSCC members) = Left ("these are cyclic: " ++ unwords members)
 
-- | Lists every problem with a set of feature names.
--   An empty result means that there are no problems with the set.
check :: Registry -> Set FeatureName -> [String]
check reg names =
    [ "Unknown feature: " ++ name
    | name <- Set.toList (Set.filter (`Map.notMember` reg) names) ] ++
    [ "Can't automatically resolve dependencies of " ++ name ++ ", explicitly enable one of: " ++ unwords (Set.toList clause)
    | (name, feat) <- Map.toList (Map.restrictKeys reg names)
    , clause <- Set.toList (dependsOn feat)
    , Set.disjoint clause names ]
 
-- | Decides which features are really needed for a selection.
resolve :: Registry -> Set FeatureName -> Either String [Feature]
resolve reg names = do
    let selected = select reg names
        errs     = check reg selected
    unless (null errs) (Left (unlines errs))
    fmap (map (reg Map.!)) (order reg selected)

-- | Reads and parses the files corrresponding to a language feature, 
--   producing its Haskell modules and its grammar (if any) as abstract syntax.
load :: Feature -> IO (Map ModuleName HsModule, Map ModuleName BookendedAbsSyn)
load _ = undefined -- Currently, this is undefined!
 
-- | Selects the features corresponding to a set of names, then superimposes their modules and grammars.
merge :: Registry -> Set FeatureName -> IO (Either String (Map ModuleName HsModule, Map ModuleName BookendedAbsSyn))
merge reg names = case resolve reg names of
    Left err    -> return (Left err)
    Right feats -> do
        (modls, grams) <- fmap unzip (mapM load feats)
        return (Right (Map.unionsWith (-->) modls, Map.unionsWith (-->) grams))

-------------------------------------------------------------------------------
-- Dependencies

-- | A dependency is expressed as a Boolean formula of feature names.
data Dependency 
    = None                -- ^ A dependency that can always be satisfied.
    | Has FeatureName     -- ^ A dependency on a single feature.
    | All [Dependency]    -- ^ All specified dependencies must be satisfied.
    | Any [Dependency]    -- ^ At least one specified dependency must be satisfied.
    deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Composing and normalizing dependencies

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

-- | Computes the conjunctive normal form (CNF) represented by a 'Dependency'.
--   See 'LH.Data.Feature.dependsOn'.
--
--   CNF denotes a set of requirements, each a set of features that must /all/ be enabled.
cnf :: Dependency -> Set (Set FeatureName)
cnf None       = Set.empty
cnf (Has name) = Set.singleton (Set.singleton name)
cnf (Any deps) = foldr (combine . cnf) (Set.singleton Set.empty) deps
cnf (All deps) = foldMap cnf deps

-- | Computes the disjunctive normal form (DNF) represented by a 'Dependency'.
--   See 'LH.Data.Feature.impliedBy'.
--
--   DNF denotes a set of alternatives, each a set of features that of which /one/ must be enabled.
dnf :: Dependency -> Set (Set FeatureName)
dnf None       = Set.empty
dnf (Has name) = Set.singleton (Set.singleton name)
dnf (Any deps) = foldMap dnf deps
dnf (All deps) = foldr (combine . dnf) (Set.singleton Set.empty) deps