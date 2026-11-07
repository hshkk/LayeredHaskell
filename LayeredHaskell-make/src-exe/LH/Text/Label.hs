module LH.Text.Label (
    -- * Labeling
    infoLabel, warnLabel, errLabel
    ) where

-- | Prepends an @"<INFO>"@ label to a string.
infoLabel :: String -> String
infoLabel msg = "<INFO>  " ++ msg

-- | Prepends a yellow @"<WARN>"@ label to a string.
warnLabel :: String -> String
warnLabel msg = "\ESC[33m<WARN>\ESC[0m  " ++ msg

-- | Prepends a bold red @"<ERROR>"@ label to a string.
errLabel :: String -> String
errLabel msg = "\ESC[1;31m<ERROR>\ESC[0m " ++ msg