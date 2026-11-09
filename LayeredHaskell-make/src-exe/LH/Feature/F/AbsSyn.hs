{- |
Module      : LH.Feature.AbsSyn
Description : The abstract syntax for F.
Copyright   : (c) Hangil Kim, 2026
                  Kairon Johnson, 2026
                  Leonidas Sallos, 2026
                  Oscar Abelanet, 2026
License     : MIT
Maintainer  : kimhang@oregonstate.edu
Stability   : experimental

This module provides abstract syntax definitions for F.
-}
module LH.Feature.AbsSyn (
    -- ** Expressions and values
    LHsExpr (..),
    -- ** Patterns
    LHsPat (..),
    ) where

import Data.Sequence (Seq)

-- | An expression.
data LHsExpr
    = LHsLam (Seq LHsPat) LHsExpr   -- ^ A lambda expression.
    deriving (Eq, Show)

-- | A pattern.
data LHsPat
    = LHsPVar Id            -- ^ A variable pattern entry.
    | LHsPCon Id            -- ^ A constructor pattern entry.
    | LHsPWild              -- ^ A wildcard pattern entry.
    | LHsPPats (Seq LHsPat) -- ^ A sublist pattern entry.
    deriving (Eq, Show)
