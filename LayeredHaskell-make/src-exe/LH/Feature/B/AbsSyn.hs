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
    LHsType (..)
    ) where

-- | An expression.
data LHsExpr
    = LHsInt  Int                     -- ^ An integer.
    | LHsBin  LHsBin LHsExpr LHsExpr  -- ^ A binary operation.
    | LHsNeg  LHsExpr                 -- ^ Negation.
    | LHsIf   LHsExpr LHsExpr LHsExpr -- ^ An @if@ expression.
    -- Note that we do not need a dedicated Boolean representation, since LHsCon provides this already.
    deriving (Eq, Show)

-- | A binary operator (except for list operators).
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
    deriving (Eq, Show)

-- | A value.
data LHsVal
    = LHsVInt Int                  -- ^ An integer.
    deriving (Eq, Show)

-- | A type.
data LHsType
    = LHsTyCon Id                  -- ^ A nullary type constructor.
    deriving (Eq, Show)