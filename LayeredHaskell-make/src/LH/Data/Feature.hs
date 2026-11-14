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

This module provides definitions for language features and operations on them,
such as superimposition and combination generation.
It also provides a number of definitions supporting the specification of dependencies between features.
-}
module LH.Data.Feature (
    -- * Features
    FeatureName, ModuleName, Feature (..), feature,
    -- ** Registries
    Registry, registry,
    -- ** Superimposing features
    FeatureSet, resolve, superimpose,
    -- ** Listing features
    list, listAll, listDeps,
    -- ** Listing feature combinations
    generate, generateCombos,
    -- * Dependencies
    Dependency (..),
    -- ** Convenient synonyms
    (/\), (\/),
    -- ** Normalization
    cnf, dnf
    ) where

import Control.Monad    (forM, forM_, unless, when)
import Data.Bifunctor   (first)
import Data.List        (intercalate)
import Data.Map         (Map)
import Data.Maybe       (maybeToList)
import Data.Set         (Set)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit      (die)
import System.FilePath  (takeDirectory, (</>), (<.>))
import System.IO        (readFile')

import Happy.Frontend          (parseYFileContents)
import Happy.Frontend.AbsSyn
import Language.Haskell.Syntax
import Language.Haskell.Parser (parseModule, ParseResult (..))
import Language.Haskell.Pretty (prettyPrint)

import qualified Data.Graph as Graph
import qualified Data.Map   as Map
import qualified Data.Set   as Set

import LH.Control.Superimposition ((-->))
import LH.Data.Error              (LHsMakeError)
import LH.Text.Happy.Pretty       (pprHappy)
import LH.Text.Label              (infoLabel, errLabel)

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
    { descrip   :: String                  -- ^ A short description of \(f\).
    , modules   :: [FilePath]              -- ^ Haskell source files.
    , grammar   :: Maybe FilePath          -- ^ A Happy grammar, if any.
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
--   The filepaths 'feature' expects should be written relative to @LayeredHaskell-make\/src\/LH\/Feature\/@.
--
--   Note that since 'feature' takes 'Dependency' arguments, it's easier to construct features using this over the original constructor 'Feature'.
feature
    :: String            -- ^ A short description of \(f\). See 'descrip'.
    -> [FilePath]        -- ^ Haskell source files (specified /relatively/, e.g. @E/AbsSyn.hs@). See 'modules'.
    -> Maybe FilePath    -- ^ A Happy grammar, if any (specified /relatively/, e.g. @E/Parser.y@). See 'grammar'.
    -> Dependency        -- ^ What \(f\) depends on. See 'dependsOn'.
    -> Dependency        -- ^ What implies \(f\). See 'impliedBy'.
    -> Bool              -- ^ Whether \(f\) may only be enabled automatically. See 'hidden'.
    -> Feature
feature desc modls gram depends implied hide =
    Feature desc (map qualify modls) (fmap qualify gram) (cnf depends) (dnf implied) hide
    where qualify file = "LayeredHaskell-make" </> "src" </> "LH" </> "Feature" </> file

-------------------------------------------------------------------------------
-- Registries

-- | A registry maps names to their 'Feature's.
type Registry = Map FeatureName Feature

-- | The set of /all/ features.
--
--   ==== __Features__
--
--   Layered Haskell defines the following features.
--   Hidden features are /italicized/.
--
--      [@E@]: /Expressions/.
--             All other features, whether it be directly or indirectly, depends on @E@.
--      [@B@]: Basic types, namely @Int@ and @Bool@.
--             Depends on @E@.
--      [@D@]: (Monomorphic) algebraic datatypes.
--             Depends on @E@.
--      [@F@]: Functions and basic pattern matching.
--             Depends on @B@ or @D@.
--      [@C@]: /Case expressions/.
--             Implied by @F@ and @D@.
--      [@P@]: Prenex polymorphism (with implicit @forall@s).
--             Depends on @D@.
--      [@L@]: List and tuple syntax.
--             Depends on @F@ and @D@.
registry :: Registry
registry = Map.fromList
    [ ("E", feature "Expressions"
                    ["E" </> "AbsSyn" <.> "hs"]
                    (Just ("E" </> "Parser" <.> "y"))
                    None
                    None
                    True)
    , ("B", feature "Basic types"
                    ["B" </> "AbsSyn" <.> "hs"]
                    (Just ("B" </> "Parser" <.> "y"))
                    (Has "E")
                    None
                    False)
    , ("D", feature "Monomorphic algebraic datatypes"
                    ["D" </> "AbsSyn" <.> "hs"]
                    (Just ("D" </> "Parser" <.> "y"))
                    (Has "E")
                    None
                    False)
    , ("F", feature "Functions"
                    ["F" </> "AbsSyn" <.> "hs"]
                    (Just ("F" </> "Parser" <.> "y"))
                    (Has "B" \/ Has "D")
                    None
                    False)
    , ("C", feature "Case expressions"
                    ["C" </> "AbsSyn" <.> "hs"]
                    (Just ("C" </> "Parser" <.> "y"))
                    None
                    (Has "D" /\ Has "F")
                    True)
    , ("P", feature "Prenex polymorphism"
                    ["P" </> "AbsSyn" <.> "hs"]
                    (Just ("P" </> "Parser" <.> "y"))
                    (Has "D")
                    None
                    False)
    , ("L", feature "List and tuple syntax"
                    ["L" </> "AbsSyn" <.> "hs"]
                    (Just ("L" </> "Parser" <.> "y"))
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
order :: Registry -> FeatureSet -> LHsMakeError [FeatureName]
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
--
--       * One or more 'hidden' features were manually enabled.
--       * The 'registry' is ill-formed in that it contains unknown features.
--       * The enabled features have dependencies that cannot be unambiguously resolved.
--
--   The last case occurs when, say, a feature @A@ depends on features @B@ or @C@, but only @A@ was manually enabled.
--   To avoid an arbitrary choice between @B@ and @C@, an error is emitted.
resolve :: Registry -> FeatureSet -> LHsMakeError [FeatureName]
resolve reg names = do
    let hide     = Map.keys (Map.filter hidden (Map.restrictKeys reg names))
        selected = select reg names
        errs     = check reg selected
    unless (null hide) $
        Left ("Hidden features cannot be selected directly: " ++ unwords hide)
    unless (null errs) $
        Left (intercalate "\n" errs)
    order reg selected

-- | Reads and parses the files corresponding to a language feature, 
--   producing its Haskell modules and its grammar (if any) as abstract syntax.
--
--   The result of 'load' also carries around corresponding module names, 
--   since we only want to superimpose modules of the same name.
load :: Feature -> IO (LHsMakeError (Map ModuleName HsModule, Map ModuleName BookendedAbsSyn))
load feat = do
    haskellSrcs <- traverse loadFileWithPath (modules feat)
    happySrcs   <- traverse loadFileWithPath (maybeToList (grammar feat))
    return $ do
        haskell <- forM haskellSrcs (\(file, src) -> do
            modl@(HsModule _ (Module name) _ _ _) <- parse file src
            return (name, modl))
        happy <- forM happySrcs (\(file, src) -> do
            gram <- first ((file ++ ": ") ++) (parseYFileContents src)
            header <- case gram of
                BookendedAbsSyn _ (Just header) _ _ -> Right header
                _ -> Left (file ++ ": The grammar header is empty, so I can't identify it")
            HsModule _ (Module name) _ _ _ <- parse file header
            return (name, gram))
        return (Map.fromList haskell, Map.fromList happy)
    where
        loadFileWithPath file = do
            src <- readFile file
            return (file, src)
        parse file src = case parseModule src of
            ParseOk modl -> Right modl
            ParseFailed (SrcLoc _ l c) err ->
                Left (file ++ "@" ++ show l ++ ":" ++ show c ++ ": " ++ err)

-- | Resolves the dependencies inherent to a 'FeatureSet', then superimposes modules and grammars 
--   such that a single set of Haskell modules and at most one Happy grammar is produced.
merge :: Registry -> FeatureSet -> IO (LHsMakeError (Map ModuleName HsModule, Map ModuleName BookendedAbsSyn))
merge reg names = case resolve reg names of
    Left err      -> return (Left (errLabel err))
    Right enabled -> do
        putStrLn $ infoLabel "These features are enabled: " ++ unwords (Set.toList names)
        let automatic = filter (`Set.notMember` names) enabled
        unless (null automatic) (putStrLn
            $  infoLabel "These features were automatically enabled: "
            ++ unwords automatic)
        -- While each feature only has at most one grammar,
        -- the Map-based representation leads to a slightly simpler implementation.
        loaded <- traverse (load . (reg Map.!)) enabled
        let mergeAll maps = sequence (Map.unionsWith (\x y -> x >>= \a -> y >>= (a -->)) 
                                                     (map (fmap Right) maps))
        return $ first errLabel $ do
            (modls, grams) <- fmap unzip (sequence loaded)
            (,) <$> mergeAll modls <*> mergeAll grams

-- | Superimposes features (and implicit dependencies), and outputs files to a specified directory.
--   The directory should written in the format, say, @src\/LH\/Feature@, 
--   since 'superimpose' simply appends it to the front of the filepath using '(</>)'. 
superimpose :: [FeatureName] -> FilePath -> IO ()
superimpose names dir = do
    putStrLn $ infoLabel "LayeredHaskell-make is starting up..."
    (haskell, happy) <- merge registry (Set.fromList names) >>= either die return
    forM_ (Map.toList haskell) $ \(name, modl) ->
        writeIfChanged
            (dir </> end name <.> "hs")
            ("{-# OPTIONS_HADDOCK hide #-}\n" ++ prettyPrint modl)
    forM_ (Map.toList happy) $ \(name, gram) ->
        writeIfChanged
            (dir </> end name <.> "y")
            (pprHappy gram)
    putStrLn $ infoLabel "Done!"
    where
        end = reverse . takeWhile (/= '.') . reverse

-- | Writes to a file only if its content differs.
--   This way, we don't need to recompile everything on every build.
writeIfChanged :: FilePath -> String -> IO ()
writeIfChanged path new = length new `seq` do
    createDirectoryIfMissing True (takeDirectory path)
    exists <- doesFileExist path
    same   <- if exists then fmap (== new) (readFile' path) else return False
    unless same $ do
        writeFile path new
        putStrLn $ infoLabel "Updated source file " ++ path

-------------------------------------------------------------------------------
-- Listing features

-- | Colors a string representing features that can be enabled manually.
colored :: String -> String
colored s = "\ESC[35m" ++ s ++ "\ESC[0m"

-- | Prints a heading, then on each line, a feature's name and an additional detail, if applicable.
--   Names that can be enabled manually (that is, those that aren't 'hidden') are 'colored'
--   alongside a note explaining the coloring whenever the registry has any hidden features.
printFeatures :: String -> Registry -> (Feature -> String) -> IO ()
printFeatures heading reg det = do
    putStrLn $ infoLabel heading
    when (any hidden reg) $
        putStrLn $ infoLabel ("Note that only " ++ colored "colored" ++ " features can be enabled manually.")
    forM_ (Map.toList reg) $ \(name, feat) ->
        putStrLn $ infoLabel ((if hidden feat then name else colored name) ++ ": " ++ det feat)

-- | Lists all non-'hidden' features in the 'registry' including their descriptions. See 'descrip'.
list :: IO ()
list = printFeatures "Listing features..." (Map.filter (not . hidden) registry) descrip

-- | Lists all features, including 'hidden' ones, in the 'registry' including their descriptions. See 'descrip'.
listAll :: IO ()
listAll = printFeatures "Listing all features..." registry descrip

-- | Lists all feature dependencies and implications.
listDeps :: IO ()
listDeps = printFeatures "Listing all dependencies and joint implications..." registry detail
    where
        detail feat
            | null parts = "none"
            | otherwise  = intercalate ", " parts
            where parts = describe "depends on "  " or "  " and " (dependsOn feat)
                       ++ describe "implied by " " and "  " or "  (impliedBy feat)
        -- Pretty-prints a nested set of names, or nothing if it is empty.
        describe prefix inner outer xss =
            [ prefix ++ intercalate outer
                [ if Set.size xs > 1 && Set.size xss > 1 then "(" ++ s ++ ")" else s
                | xs <- Set.toList xss, let s = intercalate inner (Set.toList xs) ]
            | not (Set.null xss) ]

-------------------------------------------------------------------------------
-- Listing feature combinations

-- | Generates a list of all feature combinations.
combos :: LHsMakeError [[(FeatureName, Bool)]]
combos = do
    mapM_ Left (unknown registry (foldMap (Set.unions . dependsOn) registry))
    fmap (filter (not . null)) $
        forM (filter (null . check registry)
                     (Set.toList
                         (Set.map (select registry)
                                  (Set.powerSet (Map.keysSet (Map.filter (not . hidden) registry))))))
             (\feats -> fmap (map (\name -> (name, manual feats name)))
                             (order registry feats))
    where
        -- A feature for which manual returns True must be selected manually.
        -- False means that the feature can be automatically enabled as a dependency.
        manual feats name = not (name `Set.member` select registry (Set.delete name feats))

-- | Generates and prints a list of all possible (non-redundant) feature combinations, 
--   with dependencies and implications automatically resolved.
--   Dependencies that must be manually enabled are printed in color.
generate :: IO ()
generate = do
    putStrLn $ infoLabel "Listing all (non-redundant) feature combinations..."
    putStrLn $ infoLabel ("Note that only " ++ colored "colored" ++ " features need to be enabled manually. ")
            ++ "The rest can be automatically resolved."
    either (putStrLn . errLabel) report combos
    where
        report entries = do
            mapM_ (putStrLn . infoLabel . unwords . map paint) entries
            putStrLn $ infoLabel (show (length entries)) ++ " feature combinations found!"
        paint (name, manual) = if manual then colored name else name

-- | Produces the feature combinations colored by 'generate'.
generateCombos :: IO [[FeatureName]]
generateCombos =
    either (die . errLabel)
           (return . filter (not . null) . map (map fst . filter snd))
           combos

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

-- | Computes the conjunctive normal form (CNF) of a 'Dependency'.
--   See 'LH.Data.Feature.dependsOn'.
--
--   CNF denotes a set of requirements, each a set of features that must /all/ be enabled.
cnf :: Dependency -> Set (Set FeatureName)
cnf None       = Set.empty
cnf (Has name) = Set.singleton (Set.singleton name)
cnf (Any deps) = foldr (combine . cnf) (Set.singleton Set.empty) deps
cnf (All deps) = foldMap cnf deps

-- | Computes the disjunctive normal form (DNF) of a 'Dependency'.
--   See 'LH.Data.Feature.impliedBy'.
--
--   DNF denotes a set of alternatives, each a set of features that of which /one/ must be enabled.
dnf :: Dependency -> Set (Set FeatureName)
dnf None       = Set.empty
dnf (Has name) = Set.singleton (Set.singleton name)
dnf (Any deps) = foldMap dnf deps
dnf (All deps) = foldr (combine . dnf) (Set.singleton Set.empty) deps
