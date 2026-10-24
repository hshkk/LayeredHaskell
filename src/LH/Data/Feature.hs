module LH.Data.Feature where

import Data.Map (Map)
import Data.Set (Set)

import qualified Data.Graph as Graph
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Language.Haskell.Syntax as Haskell

import LH.Control.Superimposable ((-->))

type FeatureName = String
type ModuleName  = String

data Feature
    = Feature
        { modules   :: Map ModuleName Haskell.HsModule
        , dependsOn :: [[FeatureName]]
        , impliedBy :: [[FeatureName]] }

type Registry = Map FeatureName Feature

-- repeatedly expand a set of feature names until the set no longer changes
select :: Registry -> Set FeatureName -> Set FeatureName
select reg names
    | names' == names = names
    | otherwise       = select reg names'
    where
        names' = Set.unions
            [ names
            , Set.fromList [dep | name <- Set.toList names
                                    , Just feat <- [reg Map.!? name]
                                    , [dep] <- dependsOn feat ]
            , Set.fromList [name | (name, feat) <- Map.toList reg
                                 , any (all (flip Set.member names)) (impliedBy feat) ] ]

-- attempt to produce a list of features in a valid order
order :: Registry -> Set FeatureName -> Either String [FeatureName]
order reg names = fmap reverse (traverse acyclic (Graph.stronglyConnComp nodes))
    where
        nodes = [ (name, name, concatMap (filter (flip Set.member names)) (dependsOn feat ++ impliedBy feat))
                | name <- Set.toList names, let feat = reg Map.! name ]
        acyclic (Graph.AcyclicSCC name)  = Right name
        acyclic (Graph.CyclicSCC names') = Left ("these are cyclic: " ++ unwords names')

check :: Registry -> Set FeatureName -> [String]
check reg names =
    [ "Unknown feature: " ++ name
    | name <- Set.toList names, Map.notMember name reg ] ++
    [ "Can't automatically resolve dependencies of " ++ name ++ ", explicitly enable one of: " ++ unwords clause
    | name <- Set.toList names
    , Just feat <- [reg Map.!? name]
    , clause <- dependsOn feat
    , not (any (flip Set.member names) clause) ]

merge :: Registry -> Set FeatureName -> Either String (Map ModuleName Haskell.HsModule)
merge reg names = do
    let sel = select reg names
    case check reg sel of
        []   -> return ()
        errs -> Left (unlines errs)
    names' <- order reg sel
    return (Map.unionsWith (-->) [modules (reg Map.! name) | name <- names'])