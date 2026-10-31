module Main (main) where

import Test.Hspec

import qualified LH.Test.Text.Lexer as Lexer

main :: IO ()
main = hspec $ do
    Lexer.spec