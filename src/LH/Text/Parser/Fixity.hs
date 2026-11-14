{- |
Module      : LH.Text.Parser.Fixity
Description : Operator fixities.
Copyright   : (c) Hangil Kim, 2026
                  Kairon Johnson, 2026
                  Leonidas Sallos, 2026
                  Oscar Abelanet, 2026
License     : MIT
Maintainer  : kimhang@oregonstate.edu
Stability   : experimental

This module provides definitions for operator fixities, which are represented by associativity and precedence.

Note that user-defined infix operators are not supported by Layered Haskell.
The use of operator fixities here is only to resolve operator precedences across features.
-}
module LH.Text.Parser.Fixity (
    -- * Precedence
    Prec, 
    -- * Associativity
    Assoc (..), 
    -- * Fixities
    Fixity
    ) where

-- | Operator precedence is expressed as an integer from 0 to 9.
type Prec = Int

-- | Operator associativity is expressed simply in terms of left ('L'), right ('R'), and none ('N').
data Assoc 
    = N    -- ^ Non-associativity.
    | L    -- ^ Left associativity.
    | R    -- ^ Right associativity.
    deriving (Eq, Ord, Show)

-- | An operator fixity is represented by associativity and precedence.
--  
--   In Layered Haskell, fixities follow the exact same definitions provided by the Haskell standard library.
--
--- ==== __Fixities__
--
-- > TODO
type Fixity = (Assoc, Prec)