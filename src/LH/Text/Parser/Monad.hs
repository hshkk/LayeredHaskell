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
{-# LANGUAGE OverloadedStrings #-}
module LH.Text.Parser.Monad (
    -- * A wrapper for Alex
    AlexInput (..), alexGetByte, alexInputPrevChar,
    -- * Parser monad
    LHsParser (..), runLHsParser,
    -- ** Parser state
    LHsState (..), getInput, setInput,
    -- ** Errors
    LHsError (..), renderError
    ) where

import Control.Monad.Except       (MonadError)
import Control.Monad.State.Strict 
    (MonadState, gets, modify,
     StateT, evalStateT)

import Data.Char (ord)
import Data.Text (Text)
import Data.Word (Word8)

import Errata
import Errata.Styles

import qualified Data.ByteString    as Byte
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy     as LazyText

import LH.Text.Token (LHsPos (..))

-------------------------------------------------------------------------------
-- Definitions for an Alex wrapper
-- (i.e., a set of definitions that customize how the lexer Alex generates will 
--  interact with input and output.)

-- | A representation of the current state of the input stream being tokenized.
data AlexInput = AlexInput
    { aiPos   :: !LHsPos   -- ^ The current position in the source.
    , aiPrev  :: !Char     -- ^ The previous character read (used only in the definition of 'alexInputPrevChar').
    , aiBytes :: [Word8]   -- ^ The remaining input represented as a sequence of bytes.
    , aiText  :: !Text     -- ^ The remaining input represented as 'Text'.
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

-- | Returns the character previously read from the input.
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = aiPrev

-------------------------------------------------------------------------------
-- Definitions for a parser monad with basic error handling 

-- | A parser monad.
newtype LHsParser a = LHsParser (StateT LHsState (Either LHsError) a)
    deriving (Functor, Applicative, Monad, MonadState LHsState, MonadError LHsError)

-- | Runs a 'LHsParser' against an input 'Text'.
runLHsParser :: LHsParser a -> Text -> Either LHsError a
runLHsParser (LHsParser m) src = 
    evalStateT m (LHsState (AlexInput (LHsPos 1 1) '\n' [] src) [])

-------------------------------------------------------------------------------
-- Parser state

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

-------------------------------------------------------------------------------
-- Errors

-- | An error.
data LHsError
    = LexicalError    !LHsPos !Text -- ^ A lexical error emitted by Alex.
    | UnclosedComment !LHsPos       -- ^ An unclosed multi-line comment error emitted by Alex.
    deriving (Eq, Show)

-- | Renders a 'LHsError' as a message referencing the source text.
--
--   Note that for 'renderError' to produce expected output, 
--   the 'Text' argument must be the same source text that was given to the lexer (namely via 'LH.Text.Lexer.scanTokens'),
--   otherwise the positions will not line up.
renderError :: FilePath   -- ^ The filepath of the source text. 
            -> Text       -- ^ The source text itself.
            -> LHsError   -- ^ An error.
            -> Text
renderError file src err = LazyText.toStrict (prettyErrors src [toErrata file err])
 
-- | Builds the message corresponding to a 'LHsError'.
toErrata :: FilePath -> LHsError -> Errata
toErrata file (LexicalError pos bad) =
    errataSimple
        (Just ("Lexical error: unexpected `" <> bad <> "`"))
        (underline file pos (Text.length bad) "this is not a valid token")
        Nothing
toErrata file (UnclosedComment pos) =
    errataSimple
        (Just "Unclosed comment")
        (underline file pos 2 "this comment is never closed")
        Nothing
 
-- | Underlines a number of characters on a single line with a label, starting at a given position.
underline :: FilePath   -- ^ The filepath of the source text.
          -> LHsPos     -- ^ The starting position.
          -> Int        -- ^ The number of characters to underline from that position.
          -> Text       -- ^ A label.
          -> Block
underline file (LHsPos l c) len label =
    blockSimple fancyRedStyle fancyRedPointer file Nothing
        (l, c, c + len, Just label) Nothing