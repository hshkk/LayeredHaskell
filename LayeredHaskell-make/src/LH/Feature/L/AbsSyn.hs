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
    = LHsList (Seq LHsExpr)          -- ^ A list expression.
    | LHsTuple (Seq LHsExpr)         -- ^ A tuple expression.
    | LHsListCons LHsExpr LHsExpr    -- ^ The cons operation on lists.
    | LHsListAppend LHsExpr LHsExpr  -- ^ The append operation on lists.
    deriving (Eq, Show)

data LHsPat
    = LHsPList (Seq LHsPat)          -- ^ A list pattern.
    | LHsPTuple (Seq LHsPat)         -- ^ A tuple pattern.
    | LHsPListCons LHsPat LHsPat     -- ^ The cons pattern on lists.
    deriving (Eq, Show)

data LHsType
    = LHsTyList LHsType              -- ^ The list type.
    | LHsTyTuple (Seq LHsType)       -- ^ A tuple type.
    deriving (Eq, Show)

