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
    infoLabel, hintLabel,
    -- ** Warnings and errors
    warnLabel, errLabel
    ) where

-- | Prepends an @"\<INFO\>"@ label to a string.
infoLabel :: String -> String
infoLabel msg = "<INFO>  " ++ msg

-- | Prepends a bold @"\<HINT\>"@ label to a string, and colors everything blue.
hintLabel :: String -> String
hintLabel msg = "\ESC[1;34m<HINT>\ESC[0m  \ESC[34m" ++ msg ++ "\ESC[0m"

-- | Prepends a bold @"\<WARN\>"@ label to a string, and colors everything yellow.
warnLabel :: String -> String
warnLabel msg = "\ESC[1;33m<WARN>\ESC[0m  \ESC[33m" ++ msg ++ "\ESC[0m"

-- | Prepends a bold @"\<ERROR\>"@ label to a string, and colors everything red.
errLabel :: String -> String
errLabel msg = "\ESC[1;31m<ERROR>\ESC[0m \ESC[31m" ++ msg ++ "\ESC[0m"