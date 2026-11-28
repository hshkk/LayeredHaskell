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
    LHsState (..), getInput, setInput, updateWithToken,
    -- ** Startcodes
    LHsStcd (..), getStcd, pushStcd, popStcd,
    -- ** Layout
    getInnermostLayout, pushLayout, popLayout,
    -- ** Brackets
    LHsBracket (..), LHsBracketType (..), openBracketType, closeBracketType, pprOpenBracketType,
    -- ** Errors
    LHsTextError (..)
    ) where

import Control.Monad.Except (MonadError)
import Control.Monad.State.Strict 
    (MonadState, gets, modify,
     StateT, evalStateT)

import Data.Char  (isAscii, ord)
import Data.Maybe (listToMaybe)
import Data.Text  (Text)
import Data.Word  (Word8)

import Errata

import qualified Data.ByteString    as Byte
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text

import LH.Data.Error
import LH.Data.SrcLoc (LHsPos (..), LHsSpan (..))
import LH.Text.Token  (LHsToken (..), isVirtual, isKeyword, isOperator)

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

-- | A parser monad with state ('LHsState') and errors ('LHsTextError').
newtype LHsParser a = LHsParser (StateT LHsState (Either LHsTextError) a)
    deriving (Functor, Applicative, Monad, MonadState LHsState, MonadError LHsTextError)

-- | Runs a 'LHsParser' against an input 'Text'.
runLHsParser :: LHsParser a -> Text -> Either LHsTextError a
runLHsParser (LHsParser m) src = 
    evalStateT m (LHsState (AlexInput initPos initPrev initBytes src) initStack initStack initStack Nothing)
    where
        initPos   = LHsPos 1 1
        initPrev  = '\n'
        initBytes = []
        initStack = []

-------------------------------------------------------------------------------
-- Parser state

-- | The parser state, which keeps track of the current input and two stacks.
data LHsState = LHsState
    { stInput  :: !AlexInput    
        -- ^ The current input.
    , stStcds  :: [LHsStcd]     
        -- ^ A stack of startcodes. When empty, the lexer is in the default startcode @0@.
    , stLayout :: [Int]         
        -- ^ A stack of the columns of open layout blocks.
    , stBrack  :: [LHsBracket]
        -- ^ A stack of unclosed brackets. 
        --
        --   'stBrack' is used for reporting better error messages related to unclosed brackets.
    , stNVirt  :: !(Maybe LHsSpan)
        -- ^ The span of the last seen non-virtual token.
        --
        --   'stNVirt' is used for reporting better error messages related to incomplete definitions.
    }

-- | Retrieves the current input. 
getInput :: LHsParser AlexInput
getInput = gets stInput

-- | Sets the current input. 
setInput :: AlexInput -> LHsParser ()
setInput inp = modify (\st -> st { stInput = inp })

-- | Updates the state after the lexer produces a token.
--
--   'updateWithToken' exists to update 'stNVirt' and 'stBrack', 
--   since these are the fields of 'LHsState' that might be updated after a new token is produced.
--   For instance, if an incoming 'LH.Text.Token.LHsRParen' closes the innermost entry in 'stBrack' 
--   (which should be 'LH.Text.Token.LHsLParen'), 
--   then 'updateWithToken' pops 'LH.Text.Token.LHsLParen' from the stack.
updateWithToken :: LHsSpan -> LHsToken -> LHsParser ()
updateWithToken sp tok = modify $ \st ->
    -- Update stNVirt if tok is non-virtual.
    let st' = if isVirtual tok then st else st { stNVirt = Just sp }
    -- Update the stack of open brackets.
    in case (openBracketType tok, closeBracketType tok, stBrack st') of
        -- If tok opens a new bracket, push it onto the stack.
        (Just bty, _, brs) ->
            st' { stBrack = LHsBracket bty sp : brs }
        -- If tok closes a bracket at the front of the stack, pop it.
        (_, Just bty, LHsBracket bty' _ : brs') | bty == bty' ->
            st' { stBrack = brs' }
        _ -> st'

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
-- Brackets

-- | A bracket type.
data LHsBracketType 
    = BParen    -- ^ A parenthesis.
    | BBrack    -- ^ A square bracket.
    deriving Eq

-- | Pretty-prints the opening bracket corresponding to a 'LHsBracketType'.
pprOpenBracketType :: LHsBracketType -> Text
pprOpenBracketType BParen = "("
pprOpenBracketType BBrack = "["

-- | A bracket paired with the position at which it was pushed onto the stack.
data LHsBracket = LHsBracket !LHsBracketType !LHsSpan

-- | Identifies which type of bracket a token opens, if any.
--
-- ==== __Example__
--
-- >>> openBracketType LHsTkLParen
-- Just BParen
openBracketType :: LHsToken -> Maybe LHsBracketType
openBracketType LHsTkLParen = Just BParen
openBracketType LHsTkLBrack = Just BBrack
openBracketType _           = Nothing

-- | Identifies which type of bracket a token closes, if any.
--
-- ==== __Example__
--
-- >>> closeBracketType LHsTkRParen
-- Just BParen
closeBracketType :: LHsToken -> Maybe LHsBracketType
closeBracketType LHsTkRParen = Just BParen
closeBracketType LHsTkRBrack = Just BBrack
closeBracketType _           = Nothing

-------------------------------------------------------------------------------
-- Errors

-- | A text-related error emitted by the lexer or parser.
data LHsTextError
    = LexicalError    !LHsPos !Text         -- ^ A lexical error emitted by Alex.
    | UnclosedComment !LHsPos               -- ^ An unclosed multi-line comment error emitted by Alex.
    | ParseError      !LHsSpan !LHsToken !(Maybe (LHsSpan, Text))
        -- ^ A parse error emitted by Happy. 
        --
        --   The first argument indicates where an unexpected token starts and ends in the source.
        --   The (optional) third argument is a previous place in the source alongside a label.
    deriving (Eq, Show)

-- | The error code prefix for text-related errors.
errorCodePrefix :: Text
errorCodePrefix = "T"

-- | Creates an error code for text-related errors.
--
--   Lexical errors (00-09)
--       00: Generic lexical error
--       01: Unclosed comment
--   Parse errors   (10-19)
--       10: Generic syntax error
--       11: Unavailable keyword
--       12: Unavailable binary operator
--       13: Syntax error at a virtual token
errorCodeWith :: LHsTextError -> Text
errorCodeWith (LexicalError _ _)  = errorCode errorCodePrefix "00"
errorCodeWith (UnclosedComment _) = errorCode errorCodePrefix "01"
errorCodeWith (ParseError _ bad _)
    | isKeyword  bad = errorCode errorCodePrefix "11"
    | isOperator bad = errorCode errorCodePrefix "12"
    | isVirtual  bad = errorCode errorCodePrefix "13"
    | otherwise      = errorCode errorCodePrefix "10"

instance LHsError LHsTextError where
    buildError file err@(LexicalError pos bad) =
        errataSimple
            (Just $ errorCodeWith err <+> "Unexpected" <+> quotes bad)
            (underline file (pos +> Text.length bad) "not sure how to interpret this symbol")
            unicodeHint
            where
                unicodeHint
                    | Text.any (not . isAscii) bad =
                        Just $ possibleFix $ 
                            "use only ASCII characters in program text\n"
                            <> reason "outside of comments, Layered Haskell does not support Unicode symbols"
                    | otherwise = Nothing
    buildError file err@(UnclosedComment pos) =
        errataSimple
            (Just $ errorCodeWith err <+> "Unclosed comment")
            (underline file (pos +> 2) "this comment is never closed")
            commentHint
        where
            commentHint = Just $ possibleFix $
                "add" <+> quotes "-}" <+> "somewhere after" <+> quotes "{-" <+> "to close the comment"
    buildError file err@(ParseError (LHsSpan start end) bad prev) =
        errataSimple
            (Just $ errorCodeWith err <+> "Syntax error")
            (underlines file main label prev)
            Nothing
        where
            -- Virtual tokens have an empty span, so we underline at least one character.
            main = LHsSpan start (end { posCol = max (posCol end) (posCol start + 1) })
            
            label = case (bad, prev) of
                (LHsTkVSemi,  Nothing) -> "a new definition starts here, but the previous one is incomplete"
                (LHsTkVSemi,  Just _)  -> "a new definition starts here, but the previous one is incomplete"
                (LHsTkVClose, _)       -> "something above this is incomplete"
                _                      -> "this was not expected here"
