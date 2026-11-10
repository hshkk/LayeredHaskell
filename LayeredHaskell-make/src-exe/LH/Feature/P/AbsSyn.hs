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
    -- ** Types
    LHsType (..),
    ) where

-- | A type.
data LHsType
    = LHsTyVar Id  -- ^ A type variable.
