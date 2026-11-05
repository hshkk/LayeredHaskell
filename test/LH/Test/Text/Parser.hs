-------------------------------------------------------------------------------
-- This module provides a test suite for Layered Haskell's lexer.
{-# LANGUAGE OverloadedStrings #-}
module LH.Test.Text.Parser (tests) where

import Data.Text (Text)

import Test.HUnit

import LH.Text.Parser.AST
import LH.Text.Parser.Parser

-------------------------------------------------------------------------------
-- Helper functions for writing tests

parsesTo :: Text -> Program -> Test
parsesTo src expected = show src ~: Right expected ~=? parse src

-------------------------------------------------------------------------------
-- Tests

variables :: Test
variables = "variables" ~: test
    [ "x"         `parsesTo` Program [ExprDecl (AtomExpr (AVar "x"))]
    , "foo bar"   `parsesTo` Program [ExprDecl (CompExpr (AtomExpr (AVar "foo"))
                                                                   (AVar "bar"))]
    ]

tests :: Test
tests = test
    [ variables
    ]
