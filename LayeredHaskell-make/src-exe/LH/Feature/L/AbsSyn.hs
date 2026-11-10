{- |
Module      : LH.Feature.AbsSyn
Description : The abstract syntax for L.
Copyright   : (c) Hangil Kim, 2026
                  Kairon Johnson, 2026
                  Leonidas Sallos, 2026
                  Oscar Abelanet, 2026
License     : MIT
Maintainer  : kimhang@oregonstate.edu
Stability   : experimental

This module provides abstract syntax definitions for L.
-}
module LH.Feature.AbsSyn (
    -- ** Expressions
    LHsExpr (..),
    -- ** Patterns
    LHsPat (..),
    -- ** Types
    LHsType (..)
    ) where

import Data.Sequence (Seq)

data LHsExpr
    = LHsList (Seq LHsExpr)      -- ^ A list of expressions.
    | LHsTuple (Seq LHsExpr)     -- ^ A tuple of expressions.
    | LHsListChain (Seq LHsExpr) -- ^ A chaining of expressions to a list.
    | LHsConcat LHsExpr LHsExpr  -- ^ A list concat expression (TC must verify lists).

data LHsPat
    = LHsPList (Seq LHsPat)      -- ^ A list of patterns.
    | LHsPTuple (Seq LHsPat)     -- ^ A tuple of patterns.
    | LHsPListChain (Seq LHsPat) -- ^ A chaining of patterns from a list.

data LHsType
    = LHsTyList LHsType        -- ^ A list type.
    | LHsTyTuple (Seq LHsType) -- ^ A tuple type.

