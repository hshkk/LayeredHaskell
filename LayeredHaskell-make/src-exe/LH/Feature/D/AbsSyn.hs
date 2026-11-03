{- |
Module      : LH.Feature.AbsSyn
Description : The abstract syntax for D.
Copyright   : (c) Hangil Kim, 2026
                  Kairon Johnson, 2026
                  Leonidas Sallos, 2026
                  Oscar Abelanet, 2026
License     : MIT
Maintainer  : kimhang@oregonstate.edu
Stability   : experimental

This module provides abstract syntax definitions for D.
-}
module LH.Feature.AbsSyn (
    -- * Declarations
    LHsDecl (..),
    -- ** Types
    LHsType (..),
    ) where

import Data.Sequence (Seq)

-- | A declaration.
data LHsDecl
    = LHsData Id (Seq (Id, Seq Id))  -- ^ A datatype declaration.
    deriving (Eq, Show)

-- | A type.
data LHsType
    = LHsTyCon Id                    -- ^ A nullary type constructor.
    deriving (Eq, Show)