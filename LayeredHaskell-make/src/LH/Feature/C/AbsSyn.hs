{- |
Module      : LH.Feature.AbsSyn
Description : The abstract syntax for C.
Copyright   : (c) Hangil Kim, 2026
                  Kairon Johnson, 2026
                  Leonidas Sallos, 2026
                  Oscar Abelanet, 2026
License     : MIT
Maintainer  : kimhang@oregonstate.edu
Stability   : experimental

This module provides abstract syntax definitions for C.
-}
module LH.Feature.AbsSyn (
    -- * Expressions and values
    LHsExpr (..),
    -- ** Case alternatives
    LHsAlt
    ) where

import Data.Sequence (Seq)

data LHsExpr
    = LHsCase LHsExpr (Seq LHsAlt) -- ^ A case expression.

type LHsAlt = (LHsPat, LHsExpr)