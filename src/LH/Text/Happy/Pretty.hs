{- |
Module      : LH.Text.Happy.Pretty
Description : A pretty-printer for Happy ASTs.
Copyright   : (c) Hangil Kim, 2026
                  Kairon Johnson, 2026
                  Leonidas Sallos, 2026
                  Oscar Abelanet, 2026
License     : MIT
Maintainer  : kimhang@oregonstate.edu
Stability   : experimental

This module provides a basic pretty-printer for Happy abstract syntax trees.
-}
module LH.Text.Happy.Pretty (
    -- * Pretty-printer
    pprHappy
    ) where

import Data.List (intercalate)
import Data.Maybe (maybeToList)
import Happy.Frontend.AbsSyn
import Happy.Grammar.ExpressionWithHole (ExpressionWithHole(..))

-- | @happy-lib@ exposes a function for pretty-printing abstract syntax, but it doesn't include any Haskell.
--  Since we need to generate /complete/ Happy files, we define our own Happy pretty-printer.
pprHappy :: BookendedAbsSyn -> String
pprHappy (BookendedAbsSyn options header (AbsSyn directives rules) footer) =
    concat
        [ maybe "" (\o -> "{-# " ++ o ++ " #-}\n\n") options
          -- Currently, the OverloadedStrings pragma is manually added in.
        , maybe "" (\h -> braces ("{-# LANGUAGE OverloadedStrings #-}\n" ++ h) ++ "\n\n") header
        , unlines (map pprDirective directives)
        , "\n%%\n\n"
        , intercalate "\n" (map pprRule rules)
        , maybe "" (\f -> "\n" ++ braces f ++ "\n") footer
        ]

-------------------------------------------------------------------------------
-- Directives

pprDirective :: Directive String -> String
pprDirective dir = case dir of
    TokenType ty ->
        "%tokentype " ++ braces ty
    TokenSpec specs ->
        "%token\n" ++ intercalate "\n" [ "  " ++ name ++ " " ++ braces (pprTokenSpec spec) | (name, spec) <- specs ]
    TokenName name start isPartial ->
        (if isPartial then "%partial " else "%name ") ++ unwords (name : maybeToList start)
    TokenLexer lexer eof ->
        "%lexer " ++ braces lexer ++ " " ++ braces eof
    TokenImportedIdentity ->
        "%importedidentity"
    TokenMonad st monad bind ret ->
        pprMonad st monad bind ret
    TokenNonassoc ids -> unwords ("%nonassoc" : ids)
    TokenRight ids    -> unwords ("%right" : ids)
    TokenLeft ids     -> unwords ("%left" : ids)
    TokenExpect n ->
        "%expect " ++ show n
    TokenError handler msg ->
        unwords ("%error" : map braces (handler : maybeToList msg))
    TokenErrorExpected ->
        "%error.expected"
    TokenErrorHandlerType ty ->
        "%errorhandlertype " ++ ty
    TokenAttributetype ty ->
        "%attributetype " ++ braces ty
    TokenAttribute name ty ->
        "%attribute " ++ name ++ " " ++ braces ty
 
pprMonad :: String -> String -> String -> String -> String
pprMonad st monad bind ret =
    "%monad " ++ unwords (map braces (stateType ++ [monad] ++ bindAndReturn))
    where
        stateType
            | st == "()" = []
            | otherwise  = [st]
        bindAndReturn
            | (bind, ret) == ("Happy_Prelude.>>=", "Happy_Prelude.return") = []
            | otherwise = [bind, ret]

pprTokenSpec :: TokenSpec -> String
pprTokenSpec (TokenFixed code) = code
pprTokenSpec (TokenWithValue (ExpressionWithHole before after)) =
    concatMap escape before ++ "$$" ++ after
    where
        escape '$' = "\\$"
        escape c   = [c]

pprPrec :: Prec -> [String]
pprPrec PrecNone   = []
pprPrec PrecShift  = ["%shift"]
pprPrec (PrecId x) = ["%prec", x]

-------------------------------------------------------------------------------
-- Rules
 
pprRule :: Rule String -> String
pprRule (Rule name params prods ruleType) =
    unlines (heading : zipWith (\lead prod -> "  " ++ lead ++ " " ++ pprProd prod) (":" : repeat "|") prods)
    where
        heading = name ++ pprParams params ++ maybe "" (\t -> " :: " ++ braces t) ruleType
 
pprParams :: [String] -> String
pprParams [] = ""
pprParams params = "(" ++ intercalate ", " params ++ ")"

pprProd :: Prod String -> String
pprProd (Prod terms code _ prec) = unwords (map pprTerm terms ++ pprPrec prec ++ [braces code])

pprTerm :: Term -> String
pprTerm (App name [])   = name
pprTerm (App name args) = name ++ "(" ++ intercalate ", " (map pprTerm args) ++ ")"

-- | Wraps braces around a string. 
--   Happy doesn't keep the curly braces around code segments, so we need to add them back in.
braces :: String -> String
braces s = "{" ++ s ++ "}"