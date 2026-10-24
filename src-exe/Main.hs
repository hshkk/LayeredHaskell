module Main where

import Prelude hiding (mod)

import Control.Monad (forM_)
import Language.Haskell.Syntax
import Language.Haskell.Parser (parseModule, ParseResult(..))
import Language.Haskell.Pretty (prettyPrint)
import System.Environment (getArgs)
import System.Exit (die)

import qualified Data.Map as Map
import qualified Data.Set as Set

import LH.Data.Feature
import LH.Data.Dependency

main :: IO ()
main = do
    names <- getArgs
    reg <- registry
        [ ("E", ["features/E.hs"], [], [])
        , ("B", ["features/B.hs"], cnf (Has "E"), [])
        , ("D", ["features/D.hs"], cnf (Has "E"), [])
        ]
    case merge reg (Set.fromList names) of
        Left err   -> die err
        Right mods -> forM_ (Map.toList mods) (\(_, mod) -> do
            let out = prettyPrint mod
            length out `seq` writeFile ("features/sup/" ++ concat names ++ ".hs") out)

parseFile :: FilePath -> IO HsModule
parseFile file = do
    src <- readFile file
    case parseModule src of
        ParseOk mod -> return mod
        ParseFailed src' msg -> 
            die (file ++ "@" ++ show (srcLine src') ++ ":" ++ show (srcColumn src') ++ " - " ++ msg)

-- (..., files, dependsOn, impliedBy)
registry :: [(FeatureName, [FilePath], [[FeatureName]], [[FeatureName]])] -> IO Registry
registry infos = fmap Map.fromList (mapM reify infos)
    where
        reify (name, files, deps, imps) = do
            mods <- mapM parseFile files
            return (name, Feature 
                { modules = Map.fromList [(nameOf mod, mod) | mod <- mods]
                , dependsOn = deps
                , impliedBy = imps })
        nameOf (HsModule _ (Module name) _ _ _) = name