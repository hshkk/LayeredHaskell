module Main (main) where

import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (withPrograms)
import Distribution.Simple.Program        (requireProgram, runProgram, simpleProgram)
import Distribution.Verbosity             (normal)

-- | Runs @LayeredHaskell-make@ before building the @LayeredHaskell@ executable
--   to generate the superimposed files.
main :: IO ()
main = defaultMainWithHooks simpleUserHooks
    { buildHook = \pd lbi hooks flags -> do
        (make, _) <- requireProgram normal (simpleProgram "LayeredHaskell-make") (withPrograms lbi)
        runProgram normal make []
        buildHook simpleUserHooks pd lbi hooks flags
    }