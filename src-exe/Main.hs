module Main where

import Prelude hiding (mod)

import Language.Haskell.Syntax
import Language.Haskell.Parser (parseModule, ParseResult(..))
import Language.Haskell.Pretty (prettyPrint)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath (takeBaseName)

import LH.Control.Superimposable ((-->))

main :: IO ()
main = do
    args <- getArgs
    case args of
        [leftFile, rightFile] -> do
            left  <- parseFile leftFile
            right <- parseFile rightFile
            let out = prettyPrint (left --> right)
            length out `seq` writeFile ("test/" ++ takeBaseName leftFile ++ takeBaseName rightFile ++ ".hs") out
        _ -> die "expected 2 command line arguments"

parseFile :: FilePath -> IO HsModule
parseFile file = do
    src <- readFile file
    case parseModule src of
        ParseOk mod -> return mod
        ParseFailed src' msg -> 
            die (file ++ "@" ++ show (srcLine src') ++ ":" ++ show (srcColumn src') ++ " - " ++ msg)