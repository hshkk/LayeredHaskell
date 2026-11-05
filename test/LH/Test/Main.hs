module Main (main) where

import Control.Monad (when)
import System.Exit   (exitFailure)

import Test.HUnit    (runTestTT, Counts(Counts))

import qualified LH.Test.Text.Lexer as Lexer
import qualified LH.Test.Text.Parser as Parser

main :: IO ()
main = do
    Counts _ _ le lf <- runTestTT Lexer.tests
    let lexerTests = le + lf > 0
    Counts _ _ pe pf <- runTestTT Parser.tests
    let parserTests = pe + pf > 0
    when (lexerTests && parserTests) exitFailure
