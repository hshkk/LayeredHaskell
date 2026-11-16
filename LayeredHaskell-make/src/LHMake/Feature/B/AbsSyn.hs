{- |
Module      : LH.Feature.AbsSyn
Description : The abstract syntax for B.
Copyright   : (c) Hangil Kim, 2026
                  Kairon Johnson, 2026
                  Leonidas Sallos, 2026
                  Oscar Abelanet, 2026
License     : MIT
Maintainer  : kimhang@oregonstate.edu
Stability   : experimental

This module provides abstract syntax definitions for B.
-}
module LH.Feature.AbsSyn (
    -- * Expressions and values
    LHsExpr (..), LHsVal (..),
    -- ** Binary operators
    LHsBin (..),
    -- * Types
    LHsType (..),
    -- ** Patterns
    LHsPat (..)
    ) where

import LH.Data.SrcLoc (L)

-- | An expression.
data LHsExpr
    = LHsInt  Int                                 -- ^ An integer.
    | LHsBin  LHsBin (L LHsExpr) (L LHsExpr)      -- ^ A binary operation.
    | LHsPre  LHsBin                              -- ^ A binary operator written in prefix form.
    | LHsNeg  (L LHsExpr)                         -- ^ Negation.
    | LHsIf   (L LHsExpr) (L LHsExpr) (L LHsExpr) -- ^ An @if@ expression.
    -- Note that we do not need a dedicated Boolean representation, since LHsCon provides this already.
    deriving (Eq, Show)

-- | A binary operator (except for function and list operators).
data LHsBin
    -- Arithmetic operators
    = LHsAdd
    | LHsSub
    | LHsMul
    -- Logical operators
    | LHsOr
    | LHsAnd
    -- Relational operators
    | LHsLt
    | LHsGt
    | LHsLte
    | LHsGte
    -- Equality
    | LHsEq
    | LHsNeq
    deriving (Eq, Show)

-- | A value.
data LHsVal
    = LHsVInt Int                      -- ^ An integer.
    deriving (Eq, Show)

-- | A type.
data LHsType
    = LHsTyCon Id (Seq (L LHsType))    -- ^ A type constructor.
    deriving (Eq, Show)

{-# WARNING LHsType "B has no accessible patterns. The following is only used when F is enabled." #-}
data LHsPat
    = LHsPInt Int                      -- ^ A literal integer pattern.
    | LHsPCon Id (Seq (L LHsPat))      -- ^ A pattern constructor.
    deriving (Eq, Show)
