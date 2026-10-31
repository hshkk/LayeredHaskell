-------------------------------------------------------------------------------
-- This module provides a test suite for Layered Haskell's lexer.
{-# LANGUAGE OverloadedStrings #-}
module LH.Test.Text.Lexer (spec) where

import Control.Monad (forM_)
import Data.Text     (Text)

import Test.Hspec

import qualified Data.Text as Text (pack)

import LH.Text.Lexer
import LH.Text.Parser.Monad
import LH.Text.Token

-------------------------------------------------------------------------------
-- Helper functions for writing tests idiomatically

-- Tokenizes the entirety of the input, producing a (potential) list of tokens (not lexemes).
tokens :: Text -> Either LHsError [LHsToken]
tokens = fmap (map (\(LHsLexeme _ tok) -> tok)) . scanTokens

-- Tokenizes the entirety of the input, producing a (potential) list of positions (not lexemes).
positions :: Text -> Either LHsError [(Int, Int)]
positions = fmap (map (\(LHsLexeme (LHsPos l c) _) -> (l, c))) . scanTokens

-- Does the source produce an expected list of tokens?
lexesTo :: HasCallStack => Text -> [LHsToken] -> Spec
lexesTo src expected = it (show src) $ tokens src `shouldBe` Right expected

-- Does the source produce an expected list of positions?
hasPositions :: HasCallStack => Text -> [(Int, Int)] -> Spec
hasPositions src expected = it (show src) $ positions src `shouldBe` Right expected

-- Does the source produce an expected error?
failsWith :: HasCallStack => Text -> LHsError -> Spec
failsWith src err = it (show src) $ tokens src `shouldBe` Left err

-- Does the source produce a lexical error?
lexical :: HasCallStack => Text -> (Int, Int) -> Text -> Spec
lexical src (l, c) sym = src `failsWith` LexicalError (LHsPos l c) sym

-- Does the source produce an unclosed comment error?
unclosed :: HasCallStack => Text -> (Int, Int) -> Spec
unclosed src (l, c) = src `failsWith` UnclosedComment (LHsPos l c)

-- Is the EOF token in the place that we expect it to be in?
eofAt :: HasCallStack => Text -> Int -> (Int, Int) -> Spec
eofAt src n (l, c) =
    it (show src ++ ", token " ++ show n) $
        runLHsParser (scan n) src `shouldBe` Right (LHsLexeme (LHsPos l c) LHsTkEOF)
    where
        scan :: Int -> LHsParser LHsLexeme
        scan 1 = scanToken
        scan k = scanToken >> scan (k - 1)

-------------------------------------------------------------------------------
-- Tests

variables :: Spec
variables = describe "variables" $ do
    "x"         `lexesTo` [LHsTkVar "x"]
    "foo bar"   `lexesTo` [LHsTkVar "foo", LHsTkVar "bar"]
    "x1 x_2 x'" `lexesTo` [LHsTkVar "x1", LHsTkVar "x_2", LHsTkVar "x'"]
    "_x"        `lexesTo` [LHsTkVar "_x"]
    "_"         `lexesTo` [LHsTkVar "_"]
    "xY"        `lexesTo` [LHsTkVar "xY"]

constructors :: Spec
constructors = describe "constructors" $ do
    "X"            `lexesTo` [LHsTkCon "X"]
    "Just"         `lexesTo` [LHsTkCon "Just"]
    "Foo_1'"       `lexesTo` [LHsTkCon "Foo_1'"]
    "Nothing Just" `lexesTo` [LHsTkCon "Nothing", LHsTkCon "Just"]
    "xY Z"         `lexesTo` [LHsTkVar "xY", LHsTkCon "Z"]

integers :: Spec
integers = describe "integers" $ do
    -- Tests every integer from 1 to 100.
    forM_ [1..100] $ \n ->
        Text.pack (show n) `lexesTo` [LHsTkInt n]
    "0"   `lexesTo` [LHsTkInt 0]
    "00"  `lexesTo` [LHsTkInt 0]
    "000" `lexesTo` [LHsTkInt 0]

operators :: Spec
operators = describe "operators" $ do
    "+"   `lexesTo` [LHsTkPlus]
    "-"   `lexesTo` [LHsTkMinus]
    "*"   `lexesTo` [LHsTkTimes]
    "||"  `lexesTo` [LHsTkOr]
    "&&"  `lexesTo` [LHsTkAnd]
    "<"   `lexesTo` [LHsTkLt]
    ">"   `lexesTo` [LHsTkGt]
    "<="  `lexesTo` [LHsTkLte]
    ">="  `lexesTo` [LHsTkGte]
    "< =" `lexesTo` [LHsTkLt, LHsTkEq]
    "> =" `lexesTo` [LHsTkGt, LHsTkEq]
    "=="  `lexesTo` [LHsTkEqEq]
    "= =" `lexesTo` [LHsTkEq, LHsTkEq]
    "< ==" `lexesTo` [LHsTkLt, LHsTkEqEq]
    "<= =" `lexesTo` [LHsTkLte, LHsTkEq]
    "> ==" `lexesTo` [LHsTkGt, LHsTkEqEq]
    ">= =" `lexesTo` [LHsTkGte, LHsTkEq]
    ":"   `lexesTo` [LHsTkColon]
    "++"  `lexesTo` [LHsTkPlusPlus]
    "+ +" `lexesTo` [LHsTkPlus, LHsTkPlus]

lambdasAndApplications :: Spec
lambdasAndApplications = describe "lambdas and applications" $ do
    "\\x -> x"     `lexesTo` [LHsTkLam, LHsTkVar "x", LHsTkArrow, LHsTkVar "x"]
    "\\x->x"       `lexesTo` [LHsTkLam, LHsTkVar "x", LHsTkArrow, LHsTkVar "x"]
    "\\x->2"       `lexesTo` [LHsTkLam, LHsTkVar "x", LHsTkArrow, LHsTkInt 2]
    "\\f x -> f x" `lexesTo` [LHsTkLam, LHsTkVar "f", LHsTkVar "x", LHsTkArrow, LHsTkVar "f", LHsTkVar "x"]
    "\\(x)"        `lexesTo` [LHsTkLam, LHsTkLParen, LHsTkVar "x", LHsTkRParen]
    "\\_ -> Y"     `lexesTo` [LHsTkLam, LHsTkVar "_", LHsTkArrow, LHsTkCon "Y"]
    "(\\x -> x) y" `lexesTo` [LHsTkLParen, LHsTkLam, LHsTkVar "x", LHsTkArrow, LHsTkVar "x", LHsTkRParen, LHsTkVar "y"]
    "((x))"        `lexesTo` [LHsTkLParen, LHsTkLParen, LHsTkVar "x", LHsTkRParen, LHsTkRParen]
    "f(x)"         `lexesTo` [LHsTkVar "f", LHsTkLParen, LHsTkVar "x", LHsTkRParen]
    "x->y"         `lexesTo` [LHsTkVar "x", LHsTkArrow, LHsTkVar "y"]

whitespace :: Spec
whitespace = describe "whitespace" $ do
    ""                `lexesTo` []
    "  \t\n\r\n \f\v" `lexesTo` []
    "x\r\ny"          `lexesTo` [LHsTkVar "x", LHsTkVar "y"]

lineComments :: Spec
lineComments = describe "line comments" $ do
    "-- only"             `lexesTo` []
    "--"                  `lexesTo` []
    "x --"                `lexesTo` [LHsTkVar "x"]
    "x -- c\ny"           `lexesTo` [LHsTkVar "x", LHsTkVar "y"]
    "x ---- y\nz"         `lexesTo` [LHsTkVar "x", LHsTkVar "z"]
    "x--y\nz"             `lexesTo` [LHsTkVar "x", LHsTkVar "z"]
    "x -- > y\nz"         `lexesTo` [LHsTkVar "x", LHsTkVar "z"]
    "-- é λ\nx"           `lexesTo` [LHsTkVar "x"]
    "-- \\x -> @@ {- \nx" `lexesTo` [LHsTkVar "x"]

blockComments :: Spec
blockComments = describe "block comments" $ do
    "{- c -}"                 `lexesTo` []
    "x {- c -} y"             `lexesTo` [LHsTkVar "x", LHsTkVar "y"]
    "x{-c-}y"                 `lexesTo` [LHsTkVar "x", LHsTkVar "y"]
    "{- a {- b -} c -} x"     `lexesTo` [LHsTkVar "x"]
    "{- {- {- -} -} -} x"     `lexesTo` [LHsTkVar "x"]
    "{- a -} {- b -} x"       `lexesTo` [LHsTkVar "x"]
    "{--}x"                   `lexesTo` [LHsTkVar "x"]
    "{---}x"                  `lexesTo` [LHsTkVar "x"]
    "{-}-}x"                  `lexesTo` [LHsTkVar "x"]
    "{- -- -} x"              `lexesTo` [LHsTkVar "x"]
    "{- \\x -> @@ \"é\" -} x" `lexesTo` [LHsTkVar "x"]
    "{-# LANGUAGE X #-} x"    `lexesTo` [LHsTkVar "x"]
    "{- a\n b\n -} x"         `lexesTo` [LHsTkVar "x"]

tokenPositions :: Spec
tokenPositions = describe "token positions" $ do
    "x y"           `hasPositions` [(1, 1), (1, 3)]
    "x\n  y"        `hasPositions` [(1, 1), (2, 3)]
    "\\x -> x"      `hasPositions` [(1, 1), (1, 2), (1, 4), (1, 7)]
    "\tx"           `hasPositions` [(1, 2)]
    "x\r\ny"        `hasPositions` [(1, 1), (2, 1)]
    "-- é\nx"       `hasPositions` [(2, 1)]
    "{- a\n b -} x" `hasPositions` [(2, 7)]
    "{- é λ -} x"   `hasPositions` [(1, 11)]

endOfInput :: Spec
endOfInput = describe "end of input" $ do
    eofAt ""          1 (1, 1)
    eofAt "  \n "     1 (2, 2)
    eofAt "x y"       3 (1, 4)
    eofAt "x {- c -}" 2 (1, 10)
    eofAt ""          3 (1, 1)

lexicalErrors :: Spec
lexicalErrors = describe "lexical errors" $ do
    lexical "\"s\""        (1, 1) "\""
    lexical "[x]"          (1, 1) "["
    lexical "x, y"         (1, 2) ","
    lexical "é"            (1, 1) "é"
    lexical "a\n  é"       (2, 3) "é"
    lexical "{"            (1, 1) "{"
    lexical "}"            (1, 1) "}"
    lexical "x -} y"       (1, 4) "}"
    lexical "\\x.x"        (1, 3) "."
    lexical "M.x"          (1, 2) "."
    lexical "x @@ y"       (1, 3) "@@"
    lexical "@@x"          (1, 1) "@@"
    lexical "\\\\"         (1, 1) "\\\\"
    lexical "\\->"         (1, 1) "\\->"
    lexical "x -> y ->> z" (1, 8) "->>"
    lexical "x --> y"      (1, 3) "-->"
    lexical "--|"          (1, 1) "--|"
    lexical "|--"          (1, 1) "|--"
    lexical "x --+ y"      (1, 3) "--+"
    lexical "-- x\n--> y"  (2, 1) "-->"

unclosedComments :: Spec
unclosedComments = describe "unclosed comments" $ do
    unclosed "{-"            (1, 1)
    unclosed "{-}"           (1, 1)
    unclosed "x {- c"        (1, 3)
    unclosed "{- {- -}"      (1, 1)
    unclosed "{- {- c"       (1, 1)
    unclosed "{-{-}-}"       (1, 1)
    unclosed "{- a -} {- b"  (1, 9)
    unclosed "x\n  {- a\n b" (2, 3)

spec :: Spec
spec = describe "LH.Text.Lexer" $ do
    variables
    constructors
    integers
    operators
    lambdasAndApplications
    whitespace
    lineComments
    blockComments
    tokenPositions
    endOfInput
    lexicalErrors
    unclosedComments