{- |
Module      : LH.Feature.AbsSyn
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
module LH.Feature.AbsSyn (
    -- * Identifiers
    Id, 
    -- * Declarations
    LHsDecl (..), 
    -- ** Expressions and values
    LHsExpr (..), LHsVal (..),
    -- ** Types
    LHsType,
    -- ** Environments
    LHsEnv
    ) where

import Data.Map      (Map)
import Data.Sequence (Seq)
import Data.Text     (Text)

-- | An identifier.
type Id = Text

-- | A declaration.
data LHsDecl
    = LHsEqn Id LHsExpr            -- ^ An equation.
    | LHsSig Id LHsType            -- ^ A type signature.
    deriving (Eq, Show)

-- | An expression.
data LHsExpr
    = LHsVar Id                    -- ^ A variable.
    | LHsCon Id                    -- ^ A constructor.
    | LHsApp LHsExpr LHsExpr       -- ^ An application.
    | LHsLet (Seq LHsDecl) LHsExpr -- ^ A @let@ expression.
    deriving (Eq, Show)

-- | A value.
data LHsVal
    = LHsVCon Id (Seq LHsVal)      -- ^ A constructor.
    | LHsClos LHsEnv LHsExpr       -- ^ A closure (a pair of an environment and an expression), implementing lexical scoping.
    deriving (Eq, Show)

-- {-# WARNING LHsType "E has no types. It must be accompanied by B or D." #-}
-- -- | A type.
-- data LHsType

-- | An environment of variable-value bindings.
type LHsEnv = Map Id LHsVal
