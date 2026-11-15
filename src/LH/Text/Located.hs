{- |
Module      : LH.Text.Located
Description : Located values.
Copyright   : (c) Hangil Kim, 2026
                  Kairon Johnson, 2026
                  Leonidas Sallos, 2026
                  Oscar Abelanet, 2026
License     : MIT
Maintainer  : kimhang@oregonstate.edu
Stability   : experimental

This module provides definitions for located values, which are values paired with their spans in the source.
Several operations for joining spans are also defined, namely @'(><)'@ and @'(><*)'@.
-}
module LH.Text.Located (
    -- * Located values
    L (..),
    -- * Joining spans
    Spannable (..), (><), (><*)
    ) where

import Data.Foldable (foldl')
import Data.Sequence (Seq)

import LH.Text.Token (LHsSpan (..), LHsLexeme (..))

-- | A value paired with its span in the source.
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
--   @'(><*)'@ can be interpreted as applying @'(><)'@ zero or more times.
--
--   \([a,b)\mathbin{\texttt{><}^*}\{[c_1,d_1),\ldots,[c_n,d_n)\}=[a,b)\mathbin{\texttt{><}}[c_1,d_1)\mathbin{\texttt{><}}\ldots\mathbin{\texttt{><}}[c_n,d_n)=[a,d_n)\)
(><*) :: (Spannable a, Spannable b) => a -> Seq b -> LHsSpan
(><*) a = foldl' (><) (toSpan a)
