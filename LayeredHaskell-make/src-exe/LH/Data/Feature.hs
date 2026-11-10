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
    FeatureName, ModuleName, Feature (..), feature,
    -- ** Registries
    Registry, registry,
    -- ** Merging features
    FeatureSet, merge,
    -- ** Generating feature combinations
    generate,
    -- * Dependencies
    Dependency (..),
    -- ** Convenient synonyms
    (/\), (\/),
    -- ** Normalization
    cnf, dnf
    ) where

import Control.Monad (forM, unless)
import Data.List     (intercalate)
import Data.Map      (Map)
import Data.Set      (Set)

import qualified Data.Graph as Graph
import qualified Data.Map   as Map
import qualified Data.Set   as Set

import Happy.Frontend (parseYFileContents)
import Happy.Frontend.AbsSyn
import Language.Haskell.Syntax
import Language.Haskell.Parser

import LH.Control.Superimposition ((-->))
import LH.Text.Label (infoLabel, errLabel)

-------------------------------------------------------------------------------
-- Features

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
--
--   See 'feature'.
data Feature = Feature
    { modules   :: [FilePath]                -- ^ Haskell source files.
    , grammar   :: Maybe FilePath
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
    , hidden    :: Bool
        -- ^ If 'True', \(f\) may only be enabled automatically.
    }

-- | Builds a 'Feature' \(f\) specified in terms of values of type 'Dependency'.
--   The two 'Dependency' arguments are automatically converted into the respective normal forms. See 'cnf' and 'dnf'.
--
--   Since 'feature' takes 'Dependency' arguments, it's easier to construct features using this over the original constructor 'Feature'.
feature
    :: [FilePath]        -- ^ Haskell source files (specified /relatively/, e.g. @E/AbsSyn.hs@).
    -> Maybe FilePath    -- ^ A Happy grammar, if any (specified /relatively/, e.g. @E/Parser.y@).
    -> Dependency        -- ^ What \(f\) depends on. See 'dependsOn'.
    -> Dependency        -- ^ What implies \(f\). See 'impliedBy'.
    -> Bool              -- ^ Whether \(f\) may only be enabled automatically. See 'hidden'.
    -> Feature
feature modls gram depends implied hide =
    Feature (map qualify modls) (fmap qualify gram) (cnf depends) (dnf implied) hide
    where qualify file = "LayeredHaskell-make/src-exe/LH/Feature/" ++ file

-------------------------------------------------------------------------------
-- Registries

-- | A registry maps names to their 'Feature's.
type Registry = Map FeatureName Feature

-- | The set of /all/ features.
registry :: Registry
registry = Map.fromList
    [ ("E", feature ["E/AbsSyn.hs"]
                    (Just ("E/Parser.y"))
                    None
                    None
                    True)
    , ("B", feature ["B/AbsSyn.hs"]
                    (Just ("B/Parser.y"))
                    (Has "E")
                    None
                    False)
    , ("D", feature ["D/AbsSyn.hs"]
                    (Just ("D/Parser.y"))
                    (Has "E")
                    None
                    False)
    , ("F", feature ["F/AbsSyn.hs"]
                    (Just ("F/Parser.y"))
                    (Has "E" /\ (Has "B" \/ Has "D"))
                    None
                    False)
    , ("C", feature ["C/AbsSyn.hs"]
                    (Just ("C/Parser.y"))
                    None
                    (Has "D" /\ Has "F")
                    True)
    , ("P", feature ["P/AbsSyn.hs"]
                    (Just ("P/Parser.y"))
                    (Has "D")
                    None
                    False)
    , ("L", feature ["L/AbsSyn.hs"]
                    (Just ("L/Parser.y"))
                    (Has "D" /\ Has "F")
                    None
                    False)
    ]

-------------------------------------------------------------------------------
-- Merging features

-- | A feature set is a set of features, which we simply identify by their names.
type FeatureSet = Set FeatureName

-- | Repeatedly adds to a set of feature names until nothing new gets added.
select :: Registry -> FeatureSet -> FeatureSet
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
order :: Registry -> FeatureSet -> Either String [FeatureName]
order reg names = fmap reverse (traverse acyclic (Graph.stronglyConnComp nodes))
    where
        nodes = [ (name, name, Set.toList (Set.unions (dependsOn feat <> impliedBy feat)))
                | (name, feat) <- Map.toList (Map.restrictKeys reg names) ]
        acyclic (Graph.AcyclicSCC name)   = Right name
        acyclic (Graph.CyclicSCC members) = Left ("These features have cyclic dependencies: " ++ unwords members)

-- | Lists every unknown feature name.
unknown :: Registry -> FeatureSet -> [String]
unknown reg names =
    [ "Unknown feature: " ++ name
    | name <- Set.toList (Set.filter (`Map.notMember` reg) names) ]

-- | Lists every problem with a set of feature names.
--   An empty result means that there are no problems with the set.
check :: Registry -> FeatureSet -> [String]
check reg names =
    unknown reg names ++
    [ "Can't automatically resolve dependencies of " ++ name ++ ", explicitly enable one of: " ++ unwords (Set.toList clause)
    | (name, feat) <- Map.toList (Map.restrictKeys reg names)
    , clause <- Set.toList (dependsOn feat)
    , Set.disjoint clause names ]

-- | Decides which features are really needed for a selection.
--  
--   'resolve' generates an error if any of the following are true.
--       * One or more 'hidden' features were manually enabled.
--       * The 'registry' is ill-formed in that it contains unknown features.
--       * The enabled features have a dependency that cannot be unambiguously resolved.
--
--   The last case occurs when, say, a feature @A@ depends on features @B@ /or/ @C@, but only @A@ was manually enabled.
--   To avoid an arbitrary choice between @B@ and @C@, an error is generated.
resolve :: Registry -> FeatureSet -> Either String [Feature]
resolve reg names = do
    let hide =
            [ name
            | name <- Set.toList names
            , Just feat <- [Map.lookup name reg]
            , hidden feat
            ]
    unless (null hide) $
        Left ("Hidden features cannot be selected directly: "
              ++ intercalate " " hide)
    let selected = select reg names
        errs     = check reg selected
    unless (null errs) (Left (intercalate "\n" errs))
    fmap (map (reg Map.!)) (order reg selected)

-- | Reads and parses the files corresponding to a language feature, 
--   producing its Haskell modules and its grammar (if any) as abstract syntax.
--
--   The result of 'load' also carries around corresponding module names, 
--   since we only want to superimpose modules of the same name.
load :: Feature -> IO (Either String (Map ModuleName HsModule, Map ModuleName BookendedAbsSyn))
load feat = do
    haskellSrcs <- forM (modules feat) (\file -> do
        src <- readFile file
        return (file, src))
    happySrcs <- forM (maybe [] return (grammar feat)) (\file -> do
        src <- readFile file
        return (file, src))
    return $ do
        haskell <- forM haskellSrcs (\(file, src) -> do
            modl@(HsModule _ (Module name) _ _ _) <- parse file src
            return (name, modl))
        happy <- forM happySrcs (\(file, src) -> do
            gram <- either (Left . ((file ++ ": ") ++)) Right (parseYFileContents src)
            header <- case gram of
                BookendedAbsSyn _ (Just header) _ _ -> Right header
                _ -> Left (file ++ ": The grammar header is empty, so I can't identify it")
            HsModule _ (Module name) _ _ _ <- parse file header
            return (name, gram))
        return (Map.fromList haskell, Map.fromList happy)
    where
        parse file src = case parseModule src of
            ParseOk modl -> Right modl
            ParseFailed (SrcLoc _ l c) err ->
                Left (file ++ "@" ++ show l ++ ":" ++ show c ++ ": " ++ err)

-- | Resolves the dependencies inherent to a 'FeatureSet', then superimposes modules and grammars 
--   such that a single set of Haskell modules and at most one Happy grammar is produced.
merge :: Registry -> FeatureSet -> IO (Either String (Map ModuleName HsModule, Map ModuleName BookendedAbsSyn))
merge reg names = case resolve reg names of
    Left err    -> return (Left (errLabel err))
    Right feats -> do
        putStrLn $ infoLabel "These features are enabled: " ++ intercalate " " (Set.toList names)
        let newfeats = Set.toList (Set.difference (select reg names) names)
        unless (null newfeats) (putStrLn 
            $  infoLabel "These features were automatically enabled: " 
            ++ intercalate " " newfeats)
        -- While each feature only has at most one grammar, 
        -- the Map-based representation leads to a simpler implementation.
        results <- mapM load feats
        case sequence results of
            Left err    -> return (Left (errLabel err))
            Right pairs -> do
                let (modls, grams) = unzip pairs
                return (Right (Map.unionsWith (-->) modls, Map.unionsWith (-->) grams))

-------------------------------------------------------------------------------
-- Generating feature combinations

-- | Generates and prints a list of all possible (non-redundant) feature combinations, 
--   with dependencies and implications automatically resolved.
--   Dependencies that must be manually enabled are printed in color.
generate :: IO ()
generate = do
    putStrLn $ infoLabel "Listing all (non-redundant) feature combinations..."
    putStrLn $ infoLabel "Only \ESC[35mcolored\ESC[0m features need to be enabled manually. "
            ++ "The rest can be automatically resolved."
    let combos = do
            mapM_ Left (unknown registry (foldMap (Set.unions . dependsOn) registry))
            traverse (\feats -> fmap (map (color feats)) (order registry feats)) 
                    (filter (null . check registry)
                            (Set.toList (foldr (\name acc -> Set.union acc (Set.map (select registry . Set.insert name) acc)) 
                                               (Set.singleton (select registry Set.empty)) 
                                               (Map.keys (Map.filter (not . hidden) registry)))))
    either (putStrLn . errLabel) report combos
    where
        report combos = do
            let shown = filter (not . null) combos
            mapM_ (putStrLn . infoLabel . unwords) shown
            putStrLn $ infoLabel (show (length shown)) ++ " feature combinations found!"
        -- | Colors only non-redundant features.
        color feats name
            | name `Set.member` select registry (Set.delete name feats) = name
            | otherwise = "\ESC[35m" ++ name ++ "\ESC[0m"

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
