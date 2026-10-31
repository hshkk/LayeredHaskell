{
{- |
Module      : LH.Text.Lexer
Description : The lexer.
Copyright   : (c) Hangil Kim, 2026
                  Kairon Johnson, 2026
                  Leonidas Sallos, 2026
                  Oscar Abelanet, 2026
License     : MIT
Maintainer  : kimhang@oregonstate.edu
Stability   : experimental

This module provides the lexer for Layered Haskell.
Currently, the lexer handles the λ-calculus including constructors.
-}
module LH.Text.Lexer (
    -- * Lexical tokenization
    scanToken, scanTokens
    ) where

import Control.Monad.Except       (throwError)
import Control.Monad.State.Strict (get, modify)

import Data.Text (Text)

import qualified Data.Text as Text

import LH.Text.Parser.Monad
import LH.Text.Token
}

%encoding "latin1"

-------------------------------------------------------------------------------
-- Character set macros

$digit     = 0-9
$upper     = A-Z
$lower     = [a-z \_]
$idchar    = [A-Z a-z 0-9 \_ \']
$symbol    = [\! \# \$ \% \& \* \+ \. \/ \< \= \> \? \@ \\ \^ \| \- \~ \:]
$nonsymbol = . # $symbol

-------------------------------------------------------------------------------
-- Rules

tokens :-

-- "0":       The default state of the lexer.
-- "comment": Entered when a multi-line comment starts, exited only when all open multi-line comments end.

<0> {
    $white+                   ;                     -- Whitespace is ignored entirely.
    "--" \-* ($nonsymbol .*)? ;                     -- The content of single-line comments is also ignored entirely.
    \\                        { token LHsTkLam    }
    "->"                      { token LHsTkArrow  }
    "("                       { token LHsTkLParen }
    ")"                       { token LHsTkRParen }
    $lower $idchar*           { named LHsTkVar    }
    $upper $idchar*           { named LHsTkCon    }
    $symbol+                  { \pos sym -> throwError (LexicalError pos sym) }
}

<0, comment> "{-"             { openComment       }

<comment> {
    "-}"                      { closeComment      }
    .|\n                      ;                     -- The content of multi-line comments is ignored entirely.
}

{
-------------------------------------------------------------------------------
-- Actions

-- | An action performed by the lexer.
type Action = LHsPos -> Text -> LHsParser (Maybe LHsToken)

-- | Lifts a 'LHsToken' into an 'Action'.
token :: LHsToken -> Action
token tok _ _ = return (Just tok)

-- | Lifts a function taking an identifier 
--   (namely, the constructors 'LHsTkVar' and 'LHsTkCon') into an 'Action'.
named :: (Text -> LHsToken) -> Action
named f _ name = return (Just (f name))

-- | Opens a multi-line comment, pushing a position onto the stack of the positions of open multi-line comments.
openComment :: Action
openComment pos _ = Nothing <$ modify (\st -> st { stComments = pos:stComments st })

-- | Closes a multi-line comment, popping a position from the stack of the positions of open multi-line comments.
closeComment :: Action
closeComment _ _ = Nothing <$ modify (\st -> st { stComments = drop 1 (stComments st) })

-------------------------------------------------------------------------------
-- Lexical tokenization

-- | Scans a /single/ 'LHsToken' from the input, producing a 'LHsLexeme'.
scanToken :: LHsParser LHsLexeme
scanToken = do
    LHsState inp com <- get
    let pos  = aiPos inp
        stcd = if null com then 0 else comment
    case alexScan inp stcd of
        AlexEOF
            | null com  -> return (LHsLexeme pos LHsTkEOF)
            | otherwise -> throwError (UnclosedComment (last com))
        AlexError _            -> throwError (LexicalError pos (Text.take 1 (aiText inp)))
        AlexSkip inp' _        -> setInput inp' >> scanToken
        AlexToken inp' len act -> do
            setInput inp'
            act pos (Text.take len (aiText inp)) >>= maybe scanToken (return . LHsLexeme pos)

-- | Tokenizes the /entirety/ of the input, producing a (potential) list of 'LHsLexeme's.
scanTokens :: Text -> Either LHsError [LHsLexeme]
scanTokens = runLHsParser scan
    where
        scan = do
            lxm <- scanToken
            case lxm of
                LHsLexeme _ LHsTkEOF -> return []
                _ -> fmap (lxm:) scan
}