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
    = LHsData Id (Seq Id) (Seq (Id, Seq LHsType))  
                                     -- ^ A datatype declaration.
                                     --   
                                     --   Type variables are not present in @D@,
                                     --   but we include it as the second argument to 
                                     --   avoid rewriting it again for @P@.
                                     --   In @D@, the sequence will /always/ be empty, 
                                     --   a property guaranteed by the parser.
    deriving (Eq, Show)

-- | A type.
data LHsType
    = LHsTyCon Id                    -- ^ A nullary type constructor.
    deriving (Eq, Show)