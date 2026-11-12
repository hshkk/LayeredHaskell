{- |
Module      : LH.Text.Label
Description : Labeling terminal output.
Copyright   : (c) Hangil Kim, 2026
                  Kairon Johnson, 2026
                  Leonidas Sallos, 2026
                  Oscar Abelanet, 2026
License     : MIT
Maintainer  : kimhang@oregonstate.edu
Stability   : experimental

This module provides definitions for prepending labels to strings.
-}
module LH.Text.Label (
    -- * Labeling
    infoLabel, 
    -- ** Warnings and errors
    warnLabel, errLabel
    ) where

-- | Prepends an @"\<INFO\>"@ label to a string.
infoLabel :: String -> String
infoLabel msg = "<INFO>  " ++ msg

-- | Prepends a yellow @"\<WARN\>"@ label to a string.
warnLabel :: String -> String
warnLabel msg = "\ESC[33m<WARN>\ESC[0m  " ++ msg

-- | Prepends a bold red @"\<ERROR\>"@ label to a string.
errLabel :: String -> String
errLabel msg = "\ESC[1;31m<ERROR>\ESC[0m " ++ msg