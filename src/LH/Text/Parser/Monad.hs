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
    -- * The parser monad
    LHsParser (..), runLHsParser,
    -- ** State
    LHsState (..), getInput, setInput,
    -- ** Startcodes
    LHsStcd (..), getStcd, pushStcd, popStcd,
    -- ** Layout
    getInnermostLayout, pushLayout, popLayout,
    -- ** Errors
    LHsError (..), renderError, buildError
    ) where

import Control.Monad.Except       (MonadError)
import Control.Monad.State.Strict 
    (MonadState, gets, modify,
     StateT, evalStateT)

import Data.Char  (ord)
import Data.Maybe (listToMaybe)
import Data.Text  (Text)
import Data.Word  (Word8)

import Errata
import Errata.Styles

import qualified Data.ByteString    as Byte
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy     as LazyText

import LH.Text.Token (LHsPos (..), LHsToken (..))

-------------------------------------------------------------------------------
-- Definitions for an Alex wrapper
-- (i.e., a set of definitions that customize how the lexer Alex generates will 
--  interact with input and output.)

-- | A representation of the current state of the input stream being tokenized.
data AlexInput = AlexInput
    { aiPos   :: !LHsPos    -- ^ The current position in the source.
    , aiPrev  :: !Char      -- ^ The previous character read.
    , aiBytes :: [Word8]    -- ^ The remaining input represented as a sequence of bytes.
    , aiText  :: !Text      -- ^ The remaining input represented as 'Text'.
    }

-- | Reads the next character from the input and advances the input state.
nextChar :: AlexInput -> Maybe (Char, AlexInput)
nextChar (AlexInput pos _ _ txt) = do
    (c, txt') <- Text.uncons txt
    Just (c, AlexInput (advance c pos) c [] txt')
    where
        -- | Advances the source position after consuming a character.
        advance :: Char -> LHsPos -> LHsPos
        advance '\n' (LHsPos l _) = LHsPos (1 + l) 1
        advance _    (LHsPos l c) = LHsPos l (1 + c)

-- | Encodes a character into its UTF-8 byte representation.
utf8 :: Char -> [Word8]
utf8 c
    -- ASCII characters are converted directly into a single byte.
    | c < '\x80' = [fromIntegral (ord c)]
    -- All other characters are encoded using a UTF-8 encoder.
    | otherwise  = Byte.unpack (Text.encodeUtf8 (Text.singleton c))

-- | Fetches the next byte from the input and returns an updated input.
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte inp@AlexInput { aiBytes = b:bs } = Just (b, inp { aiBytes = bs })
alexGetByte inp = do
    (c, inp') <- nextChar inp
    b:bs      <- return (utf8 c)
    return (b, inp' { aiBytes = bs })

-- | Returns the character previously read from the input.
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = aiPrev

-------------------------------------------------------------------------------
-- Definitions for a parser monad with basic error handling 

-- | A parser monad with state ('LHsState') and errors ('LHsError').
newtype LHsParser a = LHsParser (StateT LHsState (Either LHsError) a)
    deriving (Functor, Applicative, Monad, MonadState LHsState, MonadError LHsError)

-- | Runs a 'LHsParser' against an input 'Text'.
runLHsParser :: LHsParser a -> Text -> Either LHsError a
runLHsParser (LHsParser m) src = 
    evalStateT m (LHsState (AlexInput initPos initPrev initBytes src) initStack initStack)
    where
        initPos   = LHsPos 1 1
        initPrev  = '\n'
        initBytes = []
        initStack = []

-------------------------------------------------------------------------------
-- Parser state

-- | The parser state, which keeps track of the current input and two stacks.
data LHsState = LHsState
    { stInput  :: !AlexInput    -- ^ The current input.
    , stStcds  :: [LHsStcd]     -- ^ A stack of startcodes. When empty, the lexer is in the default startcode @0@.
    , stLayout :: [Int]         -- ^ A stack of the columns of open layout blocks.
    }

-- | Retrieves the current input. 
getInput :: LHsParser AlexInput
getInput = gets stInput

-- | Sets the current input. 
setInput :: AlexInput -> LHsParser ()
setInput inp = modify (\st -> st { stInput = inp })

-------------------------------------------------------------------------------
-- Startcodes

-- | A startcode paired with the position at which it was pushed onto the stack.
data LHsStcd = LHsStcd !Int !LHsPos

-- | Retrieves the current startcode. 
getStcd :: LHsParser Int
getStcd = gets (maybe 0 (\(LHsStcd code _) -> code) . listToMaybe . stStcds)

-- | Pushes a startcode onto the stack.
pushStcd :: Int -> LHsPos -> LHsParser ()
pushStcd code pos = modify (\st -> st { stStcds = LHsStcd code pos : stStcds st })

-- | Pops the current startcode from the stack.
popStcd :: LHsParser ()
popStcd = modify (\st -> st { stStcds = drop 1 (stStcds st) })

-------------------------------------------------------------------------------
-- Layout

-- | Retrieves the column of the innermost open layout block, if present.
getInnermostLayout :: LHsParser (Maybe Int)
getInnermostLayout = gets (listToMaybe . stLayout)

-- | Pushes a column of an open layout block onto the stack.
pushLayout :: Int -> LHsParser ()
pushLayout c = modify (\st -> st { stLayout = c : stLayout st })

-- | Pops a column of an open layout block from the stack.
popLayout :: LHsParser ()
popLayout = modify (\st -> st { stLayout = drop 1 (stLayout st) })

-------------------------------------------------------------------------------
-- Errors

-- | An error.
data LHsError
    = LexicalError    !LHsPos !Text             -- ^ A lexical error emitted by Alex.
    | UnclosedComment !LHsPos                   -- ^ An unclosed multi-line comment error emitted by Alex.
    | ParseError      !LHsPos !LHsPos !LHsToken -- ^ A parse error emitted by Happy. 
                                                --   The first two arguments indicate where an unexpected token 
                                                --   starts and ends in the source.
    deriving (Eq, Show)

-- | Renders a 'LHsError' as a message referencing the source text.
--
--   Note that for 'renderError' to produce expected output, 
--   the 'Text' argument must be the /same/ source text that was given to the lexer.
renderError :: FilePath    -- ^ The filepath of the source text. 
            -> Text        -- ^ The source text itself.
            -> LHsError    -- ^ An error.
            -> Text
renderError file src err = LazyText.toStrict (prettyErrors src [buildError file err])
 
-- | Builds the message corresponding to a 'LHsError'.
buildError :: FilePath    -- ^ The filepath of the source text. 
           -> LHsError    -- ^ An error.
           -> Errata      -- ^ A pretty-printable representation of the error.
                          --   This is where the error message itself should be written,
                          --   in addition to places in the source that it may need to point to.
buildError file (LexicalError pos bad) =
    errataSimple
        (Just ("Lexical error: unexpected `" <> bad <> "`"))
        (underline file pos (Text.length bad) "this is not a valid token")
        Nothing
buildError file (UnclosedComment pos) =
    errataSimple
        (Just "Unclosed comment")
        (underline file pos 2 "this comment is never closed")
        Nothing
buildError file (ParseError start end bad) =
    errataSimple
        (Just "Parse error")
        (underline file start len label)
        Nothing
    where
        len   = max 1 (posCol end - posCol start)
        label = case bad of
            LHsTkVSemi  -> "this starts a new declaration, but the previous one is unfinished"
            LHsTkVClose -> "the block ends here, but something before it is unfinished"
            _           -> "this was not expected here"

-- | Underlines a number of characters on a single line with a label, starting at a given position.
underline :: FilePath    -- ^ The filepath of the source text.
          -> LHsPos      -- ^ The starting position.
          -> Int         -- ^ The number of characters to underline from that position.
          -> Text        -- ^ A label.
          -> Block
underline file (LHsPos l c) len label =
    blockSimple fancyRedStyle fancyRedPointer file Nothing
        (l, c, c + len, Just label) Nothing