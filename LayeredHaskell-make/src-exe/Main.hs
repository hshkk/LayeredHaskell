module Main (main) where

import Data.Version       (showVersion)
import System.Environment (getArgs)
import System.FilePath    ((</>))

import qualified Paths_LayeredHaskell_make as LH (version)

import LH.Data.Feature 
    (superimpose, 
     list, listAll, listDeps, 
     generate)
import LH.Text.Label      (infoLabel, hintLabel, warnLabel, errLabel)

-- | The file containing the features that @LayeredHaskell-make@ will collate and generate source files for.
features :: FilePath
features = "LayeredHaskell-make" </> "src" </> "LH" </> "Feature" </> "Features"

-- | The destination directory for generated source files.
dir :: FilePath
dir = "src" </> "LH" </> "Feature"

-- | The @LayeredHaskell-make@ executable facilitates superimposition.
--   If @info@ is passed in as a command-line argument, 
--   a list of all possible (non-redundant) feature combinations is printed.
--   See 'generate'.
main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            names <- getNames
            superimpose names dir
        -- General information
        ["help"]    -> help
        ["version"] -> version
        -- Listing
        ["ls"]      -> list
        ["ls", "a"] -> listAll
        ["ls", "d"] -> listDeps
        ["ls", "c"] -> generate
        -- Setting and showing features
        ["show"]    -> do
            names <- getNames
            putStrLn $ infoLabel "Listing enabled features..."
            putStrLn $ infoLabel (unwords names)
        "set":feats
            | null feats -> do
                putStrLn $ errLabel "One or more features must be enabled."
                putStrLn $ hintLabel "For information on 'set', pass 'help' as an argument to LayeredHaskell-make."
            | otherwise -> do
                writeFile features (unlines feats)
                putStrLn $ infoLabel "Enabled '" ++ unwords feats ++ "'."
        -- Unrecognized input
        _  -> do
            putStrLn $ warnLabel "One or more command-line arguments were given, but they are not recognized."
            putStrLn $ warnLabel "Proceeding as if there were none..."
            names <- getNames
            superimpose names dir
            putStrLn $ hintLabel "For a list of all commands, pass 'help' as an argument to LayeredHaskell-make."
    where
        getNames = do
            args <- readFile features
            return (filter (not . null) (lines args))

-- | Prints a succinct description for every command in @LayeredHaskell-make@.
help :: IO ()
help = do
    version
    putStrLn "help       : A list of all commands"
    putStrLn "ls <arg>   : A list of..."
    putStrLn "    ...    = none  :  ...features"
    putStrLn "    ...    = a     :  ...all features"
    putStrLn "    ...    = c     :  ...all non-redundant feature combinations"
    putStrLn "    ...    = d     :  ...all feature dependencies and implications"
    putStrLn "set <args> : Enable a set of features"
    putStrLn "     ....  = Any feature combination from 'ls c', separated by spaces"
    putStrLn "show       : The set of currently enabled features"
    putStrLn "version    : The version"

-- | Prints the version of @LayeredHaskell-make@.
version :: IO ()
version = do
    putStrLn $ "LayeredHaskell-make version " ++ showVersion LH.version