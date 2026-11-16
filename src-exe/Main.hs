module Main where

import System.Environment (getArgs)
import System.Exit        (die)

import qualified Data.Text.IO as Text

import LH.Data.Error      (renderError)
import LH.Feature.Parser  (parseProgram)

-- | Currently takes a single filepath, parses it, and prints out the AST.
main :: IO ()
main = do
    files <- getArgs
    case files of
        [file] -> do
            src <- Text.readFile file
            case parseProgram src of
                Left err -> do
                    Text.putStrLn (renderError file src err)
                    die ""
                Right ast -> print ast
        _ -> die "1 filepath expected"