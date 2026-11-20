-- A script for building Layered Haskell binaries from source.
--
-- For each layer (see the definition of `layers' in this file), this script carries out three things.
--
--     1. Tells a source file generation tool (LayeredHaskell-make) which features to enable.
--     2. Builds a LayeredHaskell binary with those features.
--     3. Copies the resulting file into an output folder.
--
-- The output folder is named after the operating system, for example
-- `dist/LayeredHaskell-linux', and each binary is named after a layer,
-- for example `LH0' (or `LH0.exe' on Windows).
--
-- To run this script, execute the following command from the root directory.
--
-- > runghc scripts/Build.hs
--
-- Requirements: GHC 9.6 or newer; Cabal 3.4 or newer.
module Main (main) where

import Control.Monad    (forM_, unless, when)
import Data.Char        (isSpace)
import Data.List        (dropWhileEnd)
import System.Directory
import System.Exit
import System.FilePath  ((</>), (<.>))
import System.Info      (os)
import System.Process

-- [(layer name, features), ...]
--
-- TODO: These are placeholders. Later on, we will decide on what each layer
-- should provide.
layers :: [(String, [String])]
layers =
  [ ("LH0", ["B"])
  , ("LH1", ["B", "D"])
  , ("LH2", ["P", "B", "L"])
  ]

main :: IO ()
main = do
    -- Step 1: 
    -- Ask GHC which operating system the script is being executed on. We require the following.
    --     * A short name to be used in the name of the output folder.
    --     * The file extension that executables have on this system.
    --
    -- Note: `os' reports "mingw32" for Windows on both 32-bit and 64-bit architectures.
    (osName, ext) <- case os of
        "linux"     -> pure ("linux"    , ""   )
        "darwin"    -> pure ("osx"      , ""   )
        "mingw32"   -> pure ("win"      , "exe")
        -- The BSD family behaves like Linux and macOS for our purposes.
        "freebsd"   -> pure ("freebsd"  , ""   )
        "openbsd"   -> pure ("openbsd"  , ""   )
        "netbsd"    -> pure ("netbsd"   , ""   )
        "dragonfly" -> pure ("dragonfly", ""   )
        other       -> die (errLabel $ "Unsupported operating system: " ++ other)

    -- Step 2: 
    -- Create a directory afterwards, or clear it if a directory with the same name already exists.
    -- This prevents previously generated binaries from being retained.
    let folder = "dist" </> ("LayeredHaskell-" ++ osName)
    exists <- doesDirectoryExist folder
    when exists (removeDirectoryRecursive folder)
    createDirectoryIfMissing True folder

    -- Step 3:
    -- Then, build each layer in turn.
    forM_ layers $ \(name, features) -> do
        -- Step 3.1: 
        -- Enable the layer's features.
        -- This runs LayeredHaskell-make, which generates the source files for that combination of features.
        run "cabal" (["run", "LayeredHaskell-make", "--", "set"] ++ features)

        -- Step 3.2: 
        -- Build the LayeredHaskell binary with those features.
        run "cabal" ["build", "exe:LayeredHaskell"]

        -- Step 3.3: 
        -- Retrieve the location of the binary and copy it into the output folder.
        binary <- fmap trim (readProcess "cabal" ["list-bin", "exe:LayeredHaskell"] "")
        copyFile binary (folder </> (name <.> ext))

        -- Step 3.4: 
        -- Print a helpful message.
        putStrLn $ infoLabel ("Successfully generated binary: " ++ name <.> ext)
    
    -- Step 4:
    -- Finally, print a series of messages to indicate that the script has completed its execution.
    putStrLn $ infoLabel "Done!"
    putStrLn $ hintLabel ("Binaries were written to: " ++ folder)
    where
        -- Removes spaces and line breaks from both ends of a string.
        -- This is required since the output of `cabal list-bin' (Step 3) ends with a line break.
        trim = dropWhileEnd isSpace . dropWhile isSpace

-- Executes an external program and waits for it to finish.
-- If the program fails, the script immediately terminates and reports the failure code.
run :: String -> [String] -> IO ()
run cmd args = do
    (_, _, _, handle) <- createProcess (proc cmd args) { std_in = NoStream }
    code <- waitForProcess handle
    unless (code == ExitSuccess) (exitWith code)

-- Prepends a label to a string.
-- These definitions borrow directly from `LHMake.Text.Label' for the sake of consistency.
infoLabel, hintLabel, errLabel :: String -> String
infoLabel msg = "<INFO>  " ++ msg
hintLabel msg = "\ESC[1;34m<HINT>\ESC[0m  \ESC[34m" ++ msg ++ "\ESC[0m"
errLabel  msg = "\ESC[1;31m<ERROR>\ESC[0m \ESC[31m" ++ msg ++ "\ESC[0m"