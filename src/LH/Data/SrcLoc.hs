{- |
Module      : LH.Data.SrcLoc
Description : Source positions and spans.
Copyright   : (c) Hangil Kim, 2026
                  Kairon Johnson, 2026
                  Leonidas Sallos, 2026
                  Oscar Abelanet, 2026
License     : MIT
Maintainer  : kimhang@oregonstate.edu
Stability   : experimental

This module provides definitions for locations in the source.
'LHsPos' indicates a single position, whereas 'LHsSpan' indicates a set of positions.
-}
module LH.Data.SrcLoc (
    -- * Source positions
    LHsPos (..), 
    -- * Source spans
    LHsSpan (..),
    -- ** Lexemes
    LHsLexeme (..),
    -- ** Joining spans
    Spannable (..), (><), (><*),
    -- * Located values
    L (..)
    ) where

import Data.Foldable (foldl')
import Data.Sequence (Seq)

import LH.Text.Token (LHsToken)

-------------------------------------------------------------------------------
-- Source positions

-- | A position in the source.
data LHsPos = LHsPos 
    { posLine :: !Int      -- ^ A line.
    , posCol  :: !Int      -- ^ A column.
    }
    deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- Source spans

-- | A set of contiguous positions in the source.
--   A span can simply be described as the interval \([a,b)\), where \(a\) and \(b\) have type 'LHsPos'.
data LHsSpan = LHsSpan
    { spanStart :: !LHsPos    -- ^ The start, inclusive.
    , spanEnd   :: !LHsPos    -- ^ The end, exclusive.
    }
    deriving (Eq, Show)

-- | A token paired with its span in the source.
data LHsLexeme = LHsLexeme !LHsSpan !LHsToken
    deriving (Eq, Show)

-- | A 'Spannable' type can be converted into a span via 'toSpan'.
class Spannable a where
    -- | Converts a value into a span.
    toSpan :: a -> LHsSpan

instance Spannable LHsSpan where
    toSpan = id

instance Spannable (L a) where
    toSpan = loc

instance Spannable LHsLexeme where
    toSpan (LHsLexeme sp _) = sp

infixl 3 ><

-- | Produces a span starting at the first argument and ending at the second argument.
--
--   \([a,b)\mathbin{\texttt{><}}[c,d)=[a,d)\)
(><) :: (Spannable a, Spannable b) => a -> b -> LHsSpan
a >< b = LHsSpan (spanStart (toSpan a)) (spanEnd (toSpan b))

infixl 4 ><*

-- | Produces a span starting at the first argument all the way to the end of the last element of the second argument.
--   
--   @'(><*)'@ can be interpreted as applying @'(><)'@ zero or more times.
--
--   \([a,b)\mathbin{\texttt{><}^*}\texttt{[}[c_1,d_1)\texttt{,}\ldots\texttt{,}[c_n,d_n)\texttt{]}=[a,b)\mathbin{\texttt{><}}[c_1,d_1)\mathbin{\texttt{><}}\ldots\mathbin{\texttt{><}}[c_n,d_n)=[a,d_n)\)
(><*) :: (Spannable a, Spannable b) => a -> Seq b -> LHsSpan
(><*) a = foldl' (><) (toSpan a)

-------------------------------------------------------------------------------
-- Located values

-- | A value paired with its span in the source.
--   'L' allows any language construct to be paired with a span, e.g., declarations, expressions, types, and patterns.
--   Error messages can then reference 'loc' for pointing to appropriate places in the source.
data L a = L 
    { loc :: LHsSpan    -- ^ A span in the source.
    , un  :: a          -- ^ A value.
    }

-- | @x :: 'L' a == y :: 'L' a@ \(\Longleftrightarrow\) @'un' x == 'un' y@.
instance Eq a => Eq (L a) where
    x == y = un x == un y

-- | @'compare' (x :: 'L' a) (y :: 'L' a)@ \(\Longleftrightarrow\) @'compare' ('un' x) ('un' y)@.
instance Ord a => Ord (L a) where
    compare x y = compare (un x) (un y)

-- | @'show' (x :: 'L' a) = 'show' ('un' x)@.
instance Show a => Show (L a) where
    showsPrec d = showsPrec d . un