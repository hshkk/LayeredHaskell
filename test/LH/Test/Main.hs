module Main (main) where

import Test.Hspec

import qualified LH.Test.Text.Lexer as Lexer
import qualified LH.Test.Text.Parser as Parser

main :: IO ()
main = hspec $ do
    Lexer.spec
    Parser.spec