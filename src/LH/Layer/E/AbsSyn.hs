{- |
Module      : LH.Layer.E.AbsSyn
Description : The abstract syntax for E.
Copyright   : (c) Hangil Kim, 2026
                  Kairon Johnson, 2026
                  Leonidas Sallos, 2026
                  Oscar Abelanet, 2026
License     : MIT
Maintainer  : kimhang@oregonstate.edu
Stability   : experimental

This module provides abstract syntax definitions for E.
-}
module LH.Layer.E.AbsSyn (
    -- * Syntax
    Id, LHsDecl (..), LHsExpr (..), LHsType
    ) where

import Data.Sequence (Seq)
import Data.Text     (Text)

-- | An identifier.
type Id = Text

-- | A declaration.
data LHsDecl
    = LHsEqn Id LHsExpr               -- ^ An equation.
    | LHsSig Id LHsType               -- ^ A type signature.

-- | An expression.
data LHsExpr
    = LHsVar Id                       -- ^ A variable.
    | LHsCon Id                       -- ^ A constructor.
    | LHsApp LHsExpr LHsExpr          -- ^ An application.
    | LHsLet (Seq LHsDecl) LHsExpr    -- ^ A @let@ expression.

{-# WARNING LHsType "E has no types. It must be accompanied by B or D." #-}
-- | A type.
data LHsType