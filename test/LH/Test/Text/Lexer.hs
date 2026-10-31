-------------------------------------------------------------------------------
-- This module provides a test suite for Layered Haskell's lexer.
{-# LANGUAGE OverloadedStrings #-}
module LH.Test.Text.Lexer (tests) where

import Data.Text (Text)

import Test.HUnit

import LH.Text.Lexer
import LH.Text.Parser.Monad
import LH.Text.Token

-------------------------------------------------------------------------------
-- Helper functions for writing idiomatic tests

-- Tokenizes the entirety of the input, producing a (potential) list of tokens (not lexemes).
tokens :: Text -> Either LHsError [LHsToken]
tokens = fmap (map (\(LHsLexeme _ tok) -> tok)) . scanTokens

-- Tokenizes the entirety of the input, producing a (potential) list of positions (not lexemes).
positions :: Text -> Either LHsError [(Int, Int)]
positions = fmap (map (\(LHsLexeme (LHsPos l c) _) -> (l, c))) . scanTokens

-- Does the source produce an expected list of tokens?
lexesTo :: Text -> [LHsToken] -> Test
lexesTo src expected = show src ~: Right expected ~=? tokens src

-- Does the source produce an expected list of positions?
hasPositions :: Text -> [(Int, Int)] -> Test
hasPositions src expected = show src ~: Right expected ~=? positions src

-- Does the source produce an expected error?
failsWith :: Text -> LHsError -> Test
failsWith src err = show src ~: Left err ~=? tokens src

-- Does the source produce a lexical error?
lexical :: Text -> (Int, Int) -> Text -> Test
lexical src (l, c) sym = src `failsWith` LexicalError (LHsPos l c) sym

-- Does the source produce an unclosed comment error?
unclosed :: Text -> (Int, Int) -> Test
unclosed src (l, c) = src `failsWith` UnclosedComment (LHsPos l c)

-- Is the EOF token in the place that we expect it to be in?
eofAt :: Text -> Int -> (Int, Int) -> Test
eofAt src n (l, c) = show src ~: Right (LHsLexeme (LHsPos l c) LHsTkEOF) ~=? runLHsParser (go n) src
    where
        go :: Int -> LHsParser LHsLexeme
        go 1 = scanToken
        go k = scanToken >> go (pred k)

-------------------------------------------------------------------------------
-- Tests

variables :: Test
variables = "variables" ~: test
    [ "x"         `lexesTo` [LHsTkVar "x"]
    , "foo bar"   `lexesTo` [LHsTkVar "foo", LHsTkVar "bar"]
    , "x1 x_2 x'" `lexesTo` [LHsTkVar "x1", LHsTkVar "x_2", LHsTkVar "x'"]
    , "_x"        `lexesTo` [LHsTkVar "_x"]
    , "_"         `lexesTo` [LHsTkVar "_"]
    , "xY"        `lexesTo` [LHsTkVar "xY"]
    ]

constructors :: Test
constructors = "constructors" ~: test
    [ "X"            `lexesTo` [LHsTkCon "X"]
    , "Just"         `lexesTo` [LHsTkCon "Just"]
    , "Foo_1'"       `lexesTo` [LHsTkCon "Foo_1'"]
    , "Nothing Just" `lexesTo` [LHsTkCon "Nothing", LHsTkCon "Just"]
    , "xY Z"         `lexesTo` [LHsTkVar "xY", LHsTkCon "Z"]
    ]

lambdasAndApplications :: Test
lambdasAndApplications = "lambdas and applications" ~: test
    [ "\\x -> x"     `lexesTo` [LHsTkLam, LHsTkVar "x", LHsTkArrow, LHsTkVar "x"]
    , "\\x->x"       `lexesTo` [LHsTkLam, LHsTkVar "x", LHsTkArrow, LHsTkVar "x"]
    , "\\f x -> f x" `lexesTo` [LHsTkLam, LHsTkVar "f", LHsTkVar "x", LHsTkArrow, LHsTkVar "f", LHsTkVar "x"]
    , "\\(x)"        `lexesTo` [LHsTkLam, LHsTkLParen, LHsTkVar "x", LHsTkRParen]
    , "\\_ -> Y"     `lexesTo` [LHsTkLam, LHsTkVar "_", LHsTkArrow, LHsTkCon "Y"]
    , "(\\x -> x) y" `lexesTo` [LHsTkLParen, LHsTkLam, LHsTkVar "x", LHsTkArrow, LHsTkVar "x", LHsTkRParen, LHsTkVar "y"]
    , "((x))"        `lexesTo` [LHsTkLParen, LHsTkLParen, LHsTkVar "x", LHsTkRParen, LHsTkRParen]
    , "f(x)"         `lexesTo` [LHsTkVar "f", LHsTkLParen, LHsTkVar "x", LHsTkRParen]
    , "x->y"         `lexesTo` [LHsTkVar "x", LHsTkArrow, LHsTkVar "y"]
    ]

whitespace :: Test
whitespace = "whitespace" ~: test
    [ ""                `lexesTo` []
    , "  \t\n\r\n \f\v" `lexesTo` []
    , "x\r\ny"          `lexesTo` [LHsTkVar "x", LHsTkVar "y"]
    ]

lineComments :: Test
lineComments = "line comments" ~: test
    [ "-- only"             `lexesTo` []
    , "--"                  `lexesTo` []
    , "x --"                `lexesTo` [LHsTkVar "x"]
    , "x -- c\ny"           `lexesTo` [LHsTkVar "x", LHsTkVar "y"]
    , "x ---- y\nz"         `lexesTo` [LHsTkVar "x", LHsTkVar "z"]
    , "x--y\nz"             `lexesTo` [LHsTkVar "x", LHsTkVar "z"]
    , "x -- > y\nz"         `lexesTo` [LHsTkVar "x", LHsTkVar "z"]
    , "-- é λ\nx"           `lexesTo` [LHsTkVar "x"]
    , "-- \\x -> @@ {- \nx" `lexesTo` [LHsTkVar "x"]
    ]

blockComments :: Test
blockComments = "block comments" ~: test
    [ "{- c -}"                 `lexesTo` []
    , "x {- c -} y"             `lexesTo` [LHsTkVar "x", LHsTkVar "y"]
    , "x{-c-}y"                 `lexesTo` [LHsTkVar "x", LHsTkVar "y"]
    , "{- a {- b -} c -} x"     `lexesTo` [LHsTkVar "x"]
    , "{- {- {- -} -} -} x"     `lexesTo` [LHsTkVar "x"]
    , "{- a -} {- b -} x"       `lexesTo` [LHsTkVar "x"]
    , "{--}x"                   `lexesTo` [LHsTkVar "x"]
    , "{---}x"                  `lexesTo` [LHsTkVar "x"]
    , "{-}-}x"                  `lexesTo` [LHsTkVar "x"]
    , "{- -- -} x"              `lexesTo` [LHsTkVar "x"]
    , "{- \\x -> @@ \"é\" -} x" `lexesTo` [LHsTkVar "x"]
    , "{-# LANGUAGE X #-} x"    `lexesTo` [LHsTkVar "x"]
    , "{- a\n b\n -} x"         `lexesTo` [LHsTkVar "x"]
    ]

tokenPositions :: Test
tokenPositions = "token positions" ~: test
    [ "x y"           `hasPositions` [(1, 1), (1, 3)]
    , "x\n  y"        `hasPositions` [(1, 1), (2, 3)]
    , "\\x -> x"      `hasPositions` [(1, 1), (1, 2), (1, 4), (1, 7)]
    , "\tx"           `hasPositions` [(1, 2)]
    , "x\r\ny"        `hasPositions` [(1, 1), (2, 1)]
    , "-- é\nx"       `hasPositions` [(2, 1)]
    , "{- a\n b -} x" `hasPositions` [(2, 7)]
    , "{- é λ -} x"   `hasPositions` [(1, 11)]
    ]

endOfInput :: Test
endOfInput = "end of input" ~: test
    [ eofAt ""          1 (1, 1)
    , eofAt "  \n "     1 (2, 2)
    , eofAt "x y"       3 (1, 4)
    , eofAt "x {- c -}" 2 (1, 10)
    , eofAt ""          3 (1, 1)
    ]

lexicalErrors :: Test
lexicalErrors = "lexical errors" ~: test
    [ lexical "3"            (1, 1) "3"
    , lexical "x 3"          (1, 3) "3"
    , lexical "\"s\""        (1, 1) "\""
    , lexical "[x]"          (1, 1) "["
    , lexical "x, y"         (1, 2) ","
    , lexical "é"            (1, 1) "é"
    , lexical "a\n  é"       (2, 3) "é"
    , lexical "{"            (1, 1) "{"
    , lexical "}"            (1, 1) "}"
    , lexical "x -} y"       (1, 3) "-"
    , lexical "\\x.x"        (1, 3) "."
    , lexical "M.x"          (1, 2) "."
    , lexical "x - y"        (1, 3) "-"
    , lexical "x @@ y"       (1, 3) "@@"
    , lexical "@@x"          (1, 1) "@@"
    , lexical "\\\\"         (1, 1) "\\\\"
    , lexical "\\->"         (1, 1) "\\->"
    , lexical "x -> y ->> z" (1, 8) "->>"
    , lexical "x --> y"      (1, 3) "-->"
    , lexical "--|"          (1, 1) "--|"
    , lexical "|--"          (1, 1) "|--"
    , lexical "x --+ y"      (1, 3) "--+"
    , lexical "-- x\n--> y"  (2, 1) "-->"
    , lexical "3 @@"         (1, 1) "3"
    ]

unclosedComments :: Test
unclosedComments = "unclosed comments" ~: test
    [ unclosed "{-"            (1, 1)
    , unclosed "{-}"           (1, 1)
    , unclosed "x {- c"        (1, 3)
    , unclosed "{- {- -}"      (1, 1)
    , unclosed "{- {- c"       (1, 1)
    , unclosed "{-{-}-}"       (1, 1)
    , unclosed "{- a -} {- b"  (1, 9)
    , unclosed "x\n  {- a\n b" (2, 3)
    ]

tests :: Test
tests = test
    [ variables
    , constructors
    , lambdasAndApplications
    , whitespace
    , lineComments
    , blockComments
    , tokenPositions
    , endOfInput
    , lexicalErrors
    , unclosedComments
    ]