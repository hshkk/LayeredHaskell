module Main (main) where

import Control.Monad (when)
import System.Exit   (exitFailure)

import Test.HUnit    (runTestTT, Counts(Counts))

import qualified LH.Test.Text.Lexer as Lexer

main :: IO ()
main = do
    Counts _ _ e f <- runTestTT Lexer.tests
    when (e + f > 0) exitFailure