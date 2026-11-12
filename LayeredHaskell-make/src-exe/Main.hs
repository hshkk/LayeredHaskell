module Main where

import System.Environment (getArgs)

import LH.Data.Feature    (superimpose, generate)
import LH.Text.Label      (warnLabel)

-- | The @LayeredHaskell-make@ executable facilitates superimposition.
--   If @info@ is passed in as a command-line argument, 
--   a list of all possible (non-redundant) feature combinations is printed.
--   See 'generate'.
main :: IO ()
main = do
    args <- getArgs
    case args of
        []       -> do
            names <- getNames
            superimpose names dir
        ["info"] -> generate
        _        -> do
            putStrLn $ warnLabel "One or more command-line arguments were given, but they are not recognized."
            putStrLn $ warnLabel "Proceeding as if there were none..."
            names <- getNames
            superimpose names dir
    where
        getNames = do
            args <- readFile "LayeredHaskell-make/src/LH/Feature/Features"
            return (filter (not . null) (lines args))
        dir = "src/LH/Feature/"