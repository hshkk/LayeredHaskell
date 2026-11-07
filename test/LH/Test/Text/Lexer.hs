-------------------------------------------------------------------------------
-- This module provides a test suite for Layered Haskell's lexer.
{-# LANGUAGE OverloadedStrings #-}
module LH.Test.Text.Lexer (spec) where

import Control.Monad (forM_)
import Data.Char     (isUpper)
import Data.Text     (Text)

import Test.Hspec

import qualified Data.Text as Text

import LH.Text.Lexer
import LH.Text.Parser.Monad
import LH.Text.Token

-------------------------------------------------------------------------------
-- Helper functions for writing general tests idiomatically

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
-- Helper functions for writing layout tests idiomatically

-- Tokenizes the entirety of the input as a program, producing a (potential) list of tokens (not lexemes).
tokensAsProgram :: Text -> Either LHsError [LHsToken]
tokensAsProgram = fmap (map (\(LHsLexeme _ tok) -> tok)) . scanTokensAsProgram
 
-- Pretty-prints tokens including layout.
-- For instance, "{ x = a ; y = b }".
pprTokens :: Text -> [LHsToken]
pprTokens = map pprToken . Text.words
    where
        pprToken "{"    = LHsTkVOpen
        pprToken ";"    = LHsTkVSemi
        pprToken "}"    = LHsTkVClose
        pprToken "="    = LHsTkEq
        pprToken "->"   = LHsTkArrow
        pprToken "|"    = LHsTkPipe
        pprToken "::"   = LHsTkColonColon
        pprToken "let"  = LHsTkLet
        pprToken "in"   = LHsTkIn
        pprToken "case" = LHsTkCase
        pprToken "of"   = LHsTkOf
        pprToken "data" = LHsTkData
        pprToken "type" = LHsTkType
        pprToken name
            | isUpper (Text.head name) = LHsTkCon name
            | otherwise                = LHsTkVar name
 
-- Does the source produce an expected list of tokens (including virtual ones)?
layoutsTo :: HasCallStack => Text -> Text -> Spec
layoutsTo src expected = it (show src) $ 
    tokensAsProgram src `shouldBe` Right (pprTokens expected)

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
    -- Tests every integer from 1 to 25.
    forM_ [1..25] (\n -> Text.pack (show n) `lexesTo` [LHsTkInt n])
    "0"   `lexesTo` [LHsTkInt 0]
    "00"  `lexesTo` [LHsTkInt 0]
    "000" `lexesTo` [LHsTkInt 0]

operators :: Spec
operators = describe "operators" $ do
    "+"    `lexesTo` [LHsTkPlus]
    "-"    `lexesTo` [LHsTkMinus]
    "*"    `lexesTo` [LHsTkTimes]
    "||"   `lexesTo` [LHsTkOr]
    "&&"   `lexesTo` [LHsTkAnd]
    "<"    `lexesTo` [LHsTkLt]
    ">"    `lexesTo` [LHsTkGt]
    "<="   `lexesTo` [LHsTkLte]
    ">="   `lexesTo` [LHsTkGte]
    "< ="  `lexesTo` [LHsTkLt, LHsTkEq]
    "> ="  `lexesTo` [LHsTkGt, LHsTkEq]
    "=="   `lexesTo` [LHsTkEqEq]
    "/="   `lexesTo` [LHsTkNotEq]
    "= ="  `lexesTo` [LHsTkEq, LHsTkEq]
    "< ==" `lexesTo` [LHsTkLt, LHsTkEqEq]
    "<= =" `lexesTo` [LHsTkLte, LHsTkEq]
    "> ==" `lexesTo` [LHsTkGt, LHsTkEqEq]
    ">= =" `lexesTo` [LHsTkGte, LHsTkEq]
    ":"    `lexesTo` [LHsTkColon]
    "++"   `lexesTo` [LHsTkPlusPlus]
    "+ +"  `lexesTo` [LHsTkPlus, LHsTkPlus]

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

uppermostBlock :: Spec
uppermostBlock = describe "uppermost layout block" $ do
    ""                  `layoutsTo` "{ }"
    "x = a"             `layoutsTo` "{ x = a }"
    "x = a\ny = b"      `layoutsTo` "{ x = a ; y = b }"
    "x = a\ny = b\n"    `layoutsTo` "{ x = a ; y = b }"
    "x = a\n  b\ny = c" `layoutsTo` "{ x = a b ; y = c }"
    "  x = a\n  y = b"  `layoutsTo` "{ x = a ; y = b }"
    "  x = a\ny = b"    `layoutsTo` "{ x = a } y = b"
    "type X = A"        `layoutsTo` "{ type X = A }"

letBlocks :: Spec
letBlocks = describe "let blocks" $ do
    "letx = in_"                             `layoutsTo` "{ letx = in_ }"
    "x = let\n  a = b\n  c = d\n"            `layoutsTo` "{ x = let { a = b ; c = d } }"
    "x = let a = b\n        c = d\n    in a" `layoutsTo` "{ x = let { a = b ; c = d } in a }"
    "a = let b = let c = d\ne = f"           `layoutsTo` "{ a = let { b = let { c = d } } ; e = f }"
    "x = let\ny = z"                         `layoutsTo` "{ x = let { } ; y = z }"
    "x = let"                                `layoutsTo` "{ x = let { } }"
    -- Note: closing the layout block here is the parser's responsibility, not the lexer.
    "x = let a = b in a"                     `layoutsTo` "{ x = let { a = b in a } }"

layoutAndComments :: Spec
layoutAndComments = describe "layout and comments" $ do
    "x = let {- c -} a = b"                     `layoutsTo` "{ x = let { a = b } }"
    "x = let\n  a = b\n-- c\n  d = e"           `layoutsTo` "{ x = let { a = b ; d = e } }"
    "x = let\n  {- c -} a = b\n          d = e" `layoutsTo` "{ x = let { a = b ; d = e } }"
    "x = let\n  a = b\n  {- c\n-}d = e"         `layoutsTo` "{ x = let { a = b ; d = e } }"

layoutErrors :: Spec
layoutErrors = describe "layout errors" $ do
    lexical "x = a\n  -->"  (2, 3) "-->"
    lexical "x = let --| a" (1, 9) "--|"
    lexical "x = let\n  {"  (2, 3) "{"
    unclosed "x = let {- a" (1, 9)
    unclosed "x = a\n{- b"  (2, 1)

equations :: Spec
equations = describe "equations" $ do
    "f x = x\ng y = y" `layoutsTo` "{ f x = x ; g y = y }"
    "f A = a\nf B = b" `layoutsTo` "{ f A = a ; f B = b }"
    "f x =\n  x"       `layoutsTo` "{ f x = x }"
    "f x\n  = x"       `layoutsTo` "{ f x = x }"
    -- Note: rejecting the following equations is the parser's responsibility, not the lexer.
    "x =\na"           `layoutsTo` "{ x = ; a }"
    "x\n= a"           `layoutsTo` "{ x ; = a }"

dataDeclarations :: Spec
dataDeclarations = describe "datatype declarations" $ do
    "data T = A | B"             `layoutsTo` "{ data T = A | B }"
    "data T a = A | B a"         `layoutsTo` "{ data T a = A | B a }"
    "data T\n  = A\n  | B"       `layoutsTo` "{ data T = A | B }"
    "data T -- c\n  {- d -} = A" `layoutsTo` "{ data T = A }"
    "data T = A\ndata U = B"     `layoutsTo` "{ data T = A ; data U = B }"
    "data T = A\nf = a"          `layoutsTo` "{ data T = A ; f = a }"
    -- Note: rejecting the following datatype declaration is the parser's responsibility, not the lexer.
    "data T = A\n| B"            `layoutsTo` "{ data T = A ; | B }"
 
caseExpressions :: Spec
caseExpressions = describe "case expressions" $ do
    "f = case x of A -> a"                       `layoutsTo` "{ f = case x of { A -> a } }"
    "f = case x of\n  A -> a\n  B -> b"          `layoutsTo` "{ f = case x of { A -> a ; B -> b } }"
    "f = case x of A -> a\n              B -> b" `layoutsTo` "{ f = case x of { A -> a ; B -> b } }"
    "f = case x of\n  A ->\n    a\n  B -> b"     `layoutsTo` "{ f = case x of { A -> a ; B -> b } }"
    "f = case x of\n  A -> a\ng = b"             `layoutsTo` "{ f = case x of { A -> a } ; g = b }"
    "f = case\n  x of\n  A -> a"                 `layoutsTo` "{ f = case x of { A -> a } }"
    "f = case x of\n  A -> case y of\n    C -> c\n    D -> d\n  B -> b"
        `layoutsTo` "{ f = case x of { A -> case y of { C -> c ; D -> d } ; B -> b } }"
    "f = case x of"                              `layoutsTo` "{ f = case x of { } }"
    "f = case x of\ng = y"                       `layoutsTo` "{ f = case x of { } ; g = y }"
    "x = let a = case b of C -> d in a"          `layoutsTo` "{ x = let { a = case b of { C -> d in a } } }"

typeSignatures :: Spec
typeSignatures = describe "type signatures" $ do
    "f :: Int -> Int"    `lexesTo`   [LHsTkVar "f", LHsTkColonColon, LHsTkCon "Int", LHsTkArrow, LHsTkCon "Int"]
    "id :: a -> a"       `lexesTo`   [LHsTkVar "id", LHsTkColonColon, LHsTkVar "a", LHsTkArrow, LHsTkVar "a"]
    "f :: Int\n  -> Int" `layoutsTo` "{ f :: Int -> Int }"
    -- Note: rejecting the following type signatures is the parser's responsibility, not the lexer.
    "f\n:: Int -> Int"   `layoutsTo` "{ f ; :: Int -> Int }"
    "f ::\nInt -> Int"   `layoutsTo` "{ f :: ; Int -> Int }"

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
    uppermostBlock
    letBlocks
    layoutAndComments
    layoutErrors
    equations
    dataDeclarations
    caseExpressions
    typeSignatures