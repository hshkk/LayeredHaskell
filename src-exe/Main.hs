module Main where

import System.Environment (getArgs)
import System.Exit        (exitFailure)
import System.IO          (hPutStrLn, stderr)

import qualified Data.Text.IO as Text (readFile, hPutStrLn)

import LH.Text.Lexer        (scanTokensAsProgram)
import LH.Text.Parser.Monad (renderError)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> do
            src <- Text.readFile file
            case scanTokensAsProgram src of
                Left err -> do
                    Text.hPutStrLn stderr (renderError file src err)
                    exitFailure
                Right toks ->
                    mapM_ print toks
        _ -> do
            hPutStrLn stderr "1 argument (a filepath) expected"
            exitFailure