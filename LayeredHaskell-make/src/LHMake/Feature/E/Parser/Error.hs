{-# LANGUAGE OverloadedStrings #-}
module LH.Feature.Parser.Error (
    -- * Errors
    LHsTextError (..)
    ) where

import Data.Char (isAscii)
import Data.Text (Text)

import Errata

import qualified Data.Text as Text

import LH.Data.Error
import LH.Data.SrcLoc (LHsPos (..), LHsSpan (..))
import LH.Text.Token  (LHsToken (..), isVirtual, isKeyword, isOperator)

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
                (_, _)
                    | isKeyword bad    -> "unavailable keyword"
                _                      -> "this was not expected here"
