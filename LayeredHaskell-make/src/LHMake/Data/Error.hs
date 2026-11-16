{- |
Module      : LHMake.Data.Error
Description : Errors emitted by @LayeredHaskell-make@.
Copyright   : (c) Hangil Kim, 2026
                  Kairon Johnson, 2026
                  Leonidas Sallos, 2026
                  Oscar Abelanet, 2026
License     : MIT
Maintainer  : kimhang@oregonstate.edu
Stability   : experimental

This module provides a datatype representation for errors emitted by @LayeredHaskell-make@.
-}
module LHMake.Data.Error (
    -- * Errors
    LHsMakeError
    ) where

-- | An error, which we represent simply as a 'String'.
type LHsMakeError a = Either String a