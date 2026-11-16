{- |
Module      : LH.Data.Error
Description : A generic interface for errors emitted by Layered Haskell.
Copyright   : (c) Hangil Kim, 2026
                  Kairon Johnson, 2026
                  Leonidas Sallos, 2026
                  Oscar Abelanet, 2026
License     : MIT
Maintainer  : kimhang@oregonstate.edu
Stability   : experimental

This module provides an interface for errors emitted by Layered Haskell,
and provides a few formatting helpers to aid in the construction of error messages.
-}
{-# LANGUAGE OverloadedStrings #-}
module LH.Data.Error (
    -- * Errors
    LHsError (..),
    -- ** Reporting multiple errors
    LHsMaxError (..),
    -- ** Formatting errors
    underline, (+>)
    ) where

import Data.Text  (Text)

import Errata
import Errata.Styles

import qualified Data.Text      as Text
import qualified Data.Text.Lazy as LazyText

import LH.Data.SrcLoc (LHsPos (..), LHsSpan (..))

-- | A type representing an error is obligated to implement two functions: 'renderError' and 'buildError'.
-- 
--   'renderError' displays an error as 'Text', whereas 'buildError' constructs the error message itself.
--   Note that 'renderError' has a default definition that will be sufficient in the vast majority of cases.
class LHsError a where
    -- | Renders an error as a message referencing the source text.
    renderError :: FilePath  -- ^ The filepath of the source text. 
                -> Text      -- ^ The source text itself.
                -> a         -- ^ An error.
                -> Text      -- ^ The resulting message.
    renderError file src err = 
        LazyText.toStrict (prettyErrors src [buildError file err])
    -- | Builds the message corresponding to an error.
    buildError  :: FilePath  -- ^ The filepath of the source text. 
                -> a         -- ^ An error.
                -> Errata    -- ^ A pretty-printable representation of the error.
                             --   This is where the error message itself should be written,
                             --   in addition to places in the source that it may need to point to.

-- | Multiple simultaneous errors are all printed.
-- 
--   'renderError' prints the errors separately, while 'buildError' merges them all into one.
instance LHsError a => LHsError [a] where
    renderError file src errs = 
        LazyText.toStrict (prettyErrors src (map (buildError file) errs))
    buildError file errs = 
        Errata 
            (Just (Text.intercalate "\n" [header | err <- built, Just header <- [errataHeader err]]))
            (concatMap errataBlocks built) 
            Nothing
      where built = map (buildError file) errs

-- | A type for representing a number of errors simultaneously, but only up to a maximum.
newtype LHsMaxError a = LHsMaxError (Int, [a])

-- | Multiple simultaneous errors /up to a maximum/ are printed, 
--   then elided with the message @"...and @\(m-n\)@ more errors"@ 
--   if the number of errors \(m\) exceeds the maximum \(n\).
-- 
--   'renderError' prints the errors separately, while 'buildError' merges them all into one.
instance LHsError a => LHsError (LHsMaxError a) where
    renderError file src (LHsMaxError (n, errs)) =
        let m = length errs in
        renderError file src (take n errs) <> 
            if m > n 
            then "\n...and " <> Text.pack (show (m - n)) <> " more errors" 
            else ""
    buildError file (LHsMaxError (n, errs)) =
        let m = length errs in
        (buildError file (take n errs)) 
            { errataBody = 
                if m > n 
                then Just $ "\n...and " <> Text.pack (show (m - n)) <> " more errors"
                else Nothing 
            }

-- | Underlines a __single-line__ span alongside a label.
underline :: FilePath    -- ^ The filepath of the source text.
          -> LHsSpan     -- ^ The source span to underline.
          -> Text        -- ^ A label.
          -> Block
underline file (LHsSpan (LHsPos l c) (LHsPos _ c')) label =
    blockSimple 
        fancyRedStyle 
        fancyRedPointer 
        file 
        Nothing
        (l, c, c', Just label) 
        Nothing

infix 5 +>

-- | Constructs a span of a given length starting at a given position.
(+>) :: LHsPos -> Int -> LHsSpan
pos +> len = LHsSpan pos (pos { posCol = posCol pos + len })