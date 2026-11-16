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
    -- ** Binary operators
    LHsBin (..),
    -- ** Patterns
    LHsPat (..),
    -- ** Types
    LHsType (..)
    ) where

import Data.Sequence  (Seq)

import LH.Data.SrcLoc (L)

data LHsExpr
    = LHsList (Seq (L LHsExpr))     -- ^ A list expression.
    | LHsTuple (Seq (L LHsExpr))    -- ^ A tuple expression.
    deriving (Eq, Show)

-- | A binary operator. 
--
--   Note that binary operations in LHsExpr are provided by F, which L depends on.
data LHsBin
    = LHsListCons
    | LHsListAppend
    | LHsListIndex
    deriving (Eq, Show)

data LHsPat
    = LHsPList (Seq (L LHsPat))          -- ^ A list pattern.
    | LHsPTuple (Seq (L LHsPat))         -- ^ A tuple pattern.
    | LHsPListCons (L LHsPat) (L LHsPat) -- ^ The cons pattern on lists.
    deriving (Eq, Show)

data LHsType
    = LHsTyList (L LHsType)              -- ^ The list type.
    | LHsTyTuple (Seq (L LHsType))       -- ^ A tuple type.
    deriving (Eq, Show)

