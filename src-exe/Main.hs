module Main where

import Control.Monad (forM_)
import System.Environment (getArgs)
import System.Exit (die)

import Language.Haskell.Syntax
import Language.Haskell.Parser (parseModule, ParseResult(..))
import Language.Haskell.Pretty (prettyPrint)

import qualified Data.Map as Map
import qualified Data.Set as Set

import LH.Data.Feature
import LH.Text.Happy.Pretty

main :: IO ()
main = do
    names <- getArgs
    merged <- merge registry (Set.fromList names)
    case merged of
        Left err   -> die err
        Right (haskell, happy) -> do
            forM_ (Map.toList haskell) (\(_, modl) -> do
                let out = prettyPrint modl
                length out `seq` writeFile (concat names ++ ".hs") out)
            forM_ (Map.toList happy) (\(_, gram) -> do
                let out = pprHappy gram
                length out `seq` writeFile (concat names ++ ".y") out)


parseFile :: FilePath -> IO HsModule
parseFile file = do
    src <- readFile file
    case parseModule src of
        ParseOk modl -> return modl
        ParseFailed src' msg -> 
            die (file ++ "@" ++ show (srcLine src') ++ ":" ++ show (srcColumn src') ++ " - " ++ msg)
