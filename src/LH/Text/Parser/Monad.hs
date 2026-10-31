{- |
Module      : LH.Text.Parser.Monad
Description : The parser monad.
Copyright   : (c) Hangil Kim, 2026
                  Kairon Johnson, 2026
                  Leonidas Sallos, 2026
                  Oscar Abelanet, 2026
License     : MIT
Maintainer  : kimhang@oregonstate.edu
Stability   : experimental

This module provides the parser monad for Layered Haskell.
Consequently, it includes definitions for a custom Alex wrapper and a representation of errors and state.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LH.Text.Parser.Monad where

import Control.Monad.Except       (MonadError)
import Control.Monad.State.Strict 
    (MonadState, gets, modify,
     StateT, evalStateT)

import Data.Char (ord)
import Data.Text (Text)
import Data.Word (Word8)

import qualified Data.ByteString    as Byte
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text

import LH.Text.Token (LHsPos (..))

-------------------------------------------------------------------------------
-- * A wrapper for Alex
-- (i.e., a set of definitions that customize how the lexer Alex generates will 
--  interact with input and output.)

-- | A representation of the current state of the input stream being tokenized.
data AlexInput = AlexInput
    { aiPos   :: !LHsPos
    , aiPrev  :: !Char
    , aiBytes :: [Word8]
    , aiText  :: !Text
    }

-- | Reads the next character from the input and advances the input state.
nextChar :: AlexInput -> Maybe (Char, AlexInput)
nextChar (AlexInput p _ _ t) = do
    (c, t') <- Text.uncons t
    Just (c, AlexInput (advance c p) c [] t')
    where
        -- | Advances the source position after consuming a character.
        advance :: Char -> LHsPos -> LHsPos
        advance '\n' (LHsPos l _) = LHsPos (succ l) 1
        advance _    (LHsPos l c) = LHsPos l (succ c)

-- | Encodes a character into its UTF-8 byte representation.
utf8 :: Char -> [Word8]
utf8 c
    -- ASCII characters are converted directly into a single byte.
    | c < '\x80' = [fromIntegral (ord c)]
    -- All other characters are encoded using a UTF-8 encoder.
    | otherwise  = Byte.unpack (Text.encodeUtf8 (Text.singleton c))

-- | Fetches the next byte from the input and returns an updated input.
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte a@AlexInput { aiBytes = b:bs } = Just (b, a { aiBytes = bs })
alexGetByte a = do
    (c, a') <- nextChar a
    b:bs    <- return (utf8 c)
    return (b, a' { aiBytes = bs })

-- | Returns the character previously consumed from the input.
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = aiPrev

-------------------------------------------------------------------------------
-- * Parser monad
-- Definitions for a parser monad with basic error handling 

-- | A parser monad.
newtype LHsParser a = LHsParser (StateT LHsState (Either LHsError) a)
    deriving (Functor, Applicative, Monad, MonadState LHsState, MonadError LHsError)

-- | Runs a 'LHsParser' against an input 'Text'.
runLHsParser :: LHsParser a -> Text -> Either LHsError a
runLHsParser (LHsParser m) src = 
    evalStateT m (LHsState (AlexInput (LHsPos 1 1) '\n' [] src) [])

-- ** Parser state

-- | The parser state.
data LHsState = LHsState
    { stInput    :: !AlexInput    -- ^ The current input.
    , stComments :: [LHsPos]      -- ^ A stack of the positions of open multi-line comments (@{-@).
    }

-- | Retrieves the current input. 
getInput :: LHsParser AlexInput
getInput = gets stInput

-- | Sets the current input. 
setInput :: AlexInput -> LHsParser ()
setInput inp = modify (\st -> st { stInput = inp })

-- ** Errors

-- | An error.
data LHsError
    = LexicalError    !LHsPos !Text -- ^ A lexical error emitted by Alex.
    | UnclosedComment !LHsPos       -- ^ An unclosed multi-line comment error emitted by Alex.
    deriving (Eq, Show)