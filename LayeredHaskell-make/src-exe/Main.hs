module Main where

import Control.Monad    (forM_, unless)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath  (takeDirectory)
import System.Exit      (die)
import System.IO        (readFile')

import Language.Haskell.Pretty (prettyPrint)

import qualified Data.Map as Map
import qualified Data.Set as Set

import LH.Data.Feature      (registry, merge)
import LH.Text.Happy.Pretty (pprHappy)

-- | Superimposes the features (and implicit dependencies) indicated in the @src/Features@ file.
--   Outputs files to @src/LH/Feature/@.
main :: IO ()
main = do
    args <- readFile "LayeredHaskell-make/src-exe/LH/Feature/Features"
    let names = filter (not . null) (lines args)
    merged <- merge registry (Set.fromList names)
    case merged of
        Left err -> die err
        Right (haskell, happy) -> do
            forM_ (Map.toList haskell) (\(name, modl) -> do
                let out = prettyPrint modl
                length out `seq` writeIfChanged (qualify (end name) ++ ".hs") out)
            forM_ (Map.toList happy) (\(name, gram) -> do
                let out = pprHappy gram
                length out `seq` writeIfChanged (qualify (end name) ++ ".y") out)
    putStrLn $ "<INFO> Done!"
    where
        qualify file = "src/LH/Feature/" ++ file
        end = reverse . takeWhile (/= '.') . reverse

-- | Writes to a file only if its content differs.
--   This way, we don't need to recompile everything on every build.
writeIfChanged :: FilePath -> String -> IO ()
writeIfChanged path new = do
    createDirectoryIfMissing True (takeDirectory path)
    exists <- doesFileExist path
    old <- if exists then readFile' path else pure ""
    unless (exists && old == new) $ do
        writeFile path new
        putStrLn $ "<INFO> Updated source file " ++ path