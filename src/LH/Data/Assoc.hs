module LH.Data.Assoc where

import Data.Map (Map)
import Data.Set (Set)

import qualified Data.Graph as Graph
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Language.Haskell.Syntax as Haskell

import LH.Control.Superimposable ((-->))

type AssocName  = String
type ModuleName = String

data Assoc
    = Assoc
        { modules   :: Map ModuleName Haskell.HsModule
        , dependsOn :: [[AssocName]]
        , impliedBy :: [[AssocName]] }

type Registry = Map AssocName Assoc

select :: Registry -> Set AssocName -> Set AssocName
select reg names
    | names' == names = names
    | otherwise       = select reg names'
    where
        names' = Set.unions
            [ names
            , Set.fromList [names'' | name <- Set.toList names
                                    , Just assoc <- [reg Map.!? name]
                                    , [names''] <- dependsOn assoc ]
            , Set.fromList [name | (name, assoc) <- Map.toList reg
                                 , any (all (flip Set.member names)) (impliedBy assoc) ] ]

order :: Registry -> Set AssocName -> Either String [AssocName]
order reg names = fmap reverse (traverse acyclic (Graph.stronglyConnComp nodes))
    where
        nodes = [ (name, name, concatMap (filter (flip Set.member names)) (dependsOn assoc ++ impliedBy assoc))
                | name <- Set.toList names, let assoc = reg Map.! name ]
        acyclic (Graph.AcyclicSCC name)  = Right name
        acyclic (Graph.CyclicSCC names') = Left ("these are cyclic: " ++ unwords names')

merge :: Registry -> Set AssocName -> Either String (Map ModuleName Haskell.HsModule)
merge reg names = do
    names' <- order reg names
    return (Map.unionsWith (-->) [modules (reg Map.! name) | name <- names'])