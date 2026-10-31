{- |
Module      : LH.Text.Token
Description : Tokens and source positions.
Copyright   : (c) Hangil Kim, 2026
                  Kairon Johnson, 2026
                  Leonidas Sallos, 2026
                  Oscar Abelanet, 2026
License     : MIT
Maintainer  : kimhang@oregonstate.edu
Stability   : experimental

This module provides definitions for tokens and source positions.
-}
module LH.Text.Token (
    -- * Source positions
    LHsPos (..), 
    -- * Tokens
    LHsToken (..), 
    -- ** Lexemes
    LHsLexeme (..)
    ) where

import Data.Text (Text)

-- | A position in the source.
data LHsPos = LHsPos 
    { posLine :: !Int -- ^ A line.
    , posCol  :: !Int -- ^ A column.
    }
    deriving (Eq, Ord, Show)

-- | A token.
data LHsToken
    = LHsTkVar    !Text    -- ^ A variable name.
    | LHsTkCon    !Text    -- ^ A constructor name.
    | LHsTkLam             -- ^ A backslash @\@, as used in a lambda abstraction.
    | LHsTkArrow           -- ^ An arrow @->@.
    | LHsTkLParen          -- ^ A left parenthesis @(@.
    | LHsTkRParen          -- ^ A right parenthesis @)@.
    | LHsTkEOF             -- ^ The token indicating end-of-file (EOF).
    deriving (Eq, Show)

-- | A token paired with its position in the source.
data LHsLexeme = LHsLexeme !LHsPos !LHsToken
    deriving (Eq, Show)