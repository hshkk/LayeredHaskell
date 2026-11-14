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
    -- ** Binary operators
    LHsBin (..),
    -- ** Patterns
    LHsPat (..),
    -- ** Types
    LHsType (..),
    ) where

import Data.Sequence (Seq)

-- | An expression.
data LHsExpr
    = LHsLam (Seq LHsPat) LHsExpr     -- ^ A lambda abstraction.
    | LHsBin  LHsBin LHsExpr LHsExpr  -- ^ A binary operation.
    deriving (Eq, Show)

-- | A binary operator.
data LHsBin
    = LHsFunApp                     -- ^ Function application.
    | LHsFunComp                    -- ^ Function composition.
    deriving (Eq, Show)

-- | A pattern.
data LHsPat
    = LHsPVar Id                    -- ^ A pattern variable.
    | LHsPWild                      -- ^ The wildcard pattern.
    deriving (Eq, Show)

-- | A type.
data LHsType
    = LHsTyFn LHsType LHsType       -- ^ A function type.
    deriving (Eq, Show)
