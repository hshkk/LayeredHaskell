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
-}
module LH.Text.Lexer (
    -- * Lexical tokenization
    scanToken, scanTokens, scanTokensAsProgram,
    -- ** Stack initialization
    beginProgram
    ) where

import Control.Monad.Except       (throwError)
import Control.Monad.State.Strict (gets)

import Data.Text (Text)

import qualified Data.Text as Text
import qualified Data.Text.Read as Text (decimal)

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
$blank     = $white # \n

-------------------------------------------------------------------------------
-- Regular expression macros

-- A single-line comment.
@comment   = "--" \-* ($nonsymbol .*)?
-- @noncomment represents strings that begin with -- but are not considered comments.
-- For instance, --> is treated as an operator in Haskell, so Layered Haskell should
-- not consider it as a single-line comment.
@noncomment = "--" \-* ($symbol # \-) $symbol*

-------------------------------------------------------------------------------
-- Rules

tokens :-

-------------------------------------------------------------------------------
-- Startcodes
-------------------------------------------------------------------------------
-- 
-- 0:       The default state of the lexer.
-- comment: Entered when a multi-line comment starts, exited only when all open multi-line comments end.
-- layout:  Entered immediately after a layout keyword.
-- empty:   Entered immediately after a layout block turned out to be empty.
-- newline: Entered after a line break. The next token is checked against the offside rule.
--
-- Note that Alex internally represents these as integers.

-------------------------------------------------------------------------------
-- Whitespace and comments

<0, layout, newline> {
    $blank+  ;
    @comment ;
}

<0>               \n               { beginNewline }
<layout, newline> \n               ;
<layout, newline> @noncomment      { lexicalError } -- In 0, the $symbol+ rule covers this case.

<0, layout, newline, comment> "{-" { beginComment }

<comment> {
    "-}"                           { endComment   }
    .|\n                           ;                -- The content of multi-line comments is ignored entirely.
}

-------------------------------------------------------------------------------
-- Standard rules

<0> {
    ---------------------------------------------------------------------------
    -- Symbols
    \\                        { token LHsTkLam        }
    "="                       { token LHsTkEq         }
    "|"                       { token LHsTkPipe       }
    "::"                      { token LHsTkColonColon }
    "("                       { token LHsTkLParen     }
    ")"                       { token LHsTkRParen     }
    "["                       { token LHsTkLBrack     }
    "]"                       { token LHsTkRBrack     }
    ","                       { token LHsTkComma      }
    ---------------------------------------------------------------------------
    -- Keywords
    "let"                     { layoutKeyword LHsTkLet }
    "in"                      { token LHsTkIn          }
    "case"                    { token LHsTkCase        }
    "of"                      { layoutKeyword LHsTkOf  }
    "data"                    { token LHsTkData        }
    "type"                    { token LHsTkType        }
    ---------------------------------------------------------------------------
    -- Operators
    "->"                      { token LHsTkArrow    }
    -- Arithmetic operators
    "+"                       { token LHsTkPlus     }
    "-"                       { token LHsTkMinus    }
    "*"                       { token LHsTkTimes    }
    -- Logical operators
    "||"                      { token LHsTkOr       }
    "&&"                      { token LHsTkAnd      }
    -- Relational operators
    "<"                       { token LHsTkLt       }
    ">"                       { token LHsTkGt       }
    "<="                      { token LHsTkLte      }
    ">="                      { token LHsTkGte      }
    -- Equality
    "=="                      { token LHsTkEqEq     }
    -- List operators
    ":"                       { token LHsTkColon    }
    "++"                      { token LHsTkPlusPlus }
    ---------------------------------------------------------------------------
    -- Literals
    $digit+                   { numeric LHsTkInt }
    ---------------------------------------------------------------------------
    -- Identifiers
    $lower $idchar*           { named LHsTkVar }
    $upper $idchar*           { named LHsTkCon }
    $symbol+                  { lexicalError   }
}

-------------------------------------------------------------------------------
-- Layout

-- The following rules don't match on any input.
-- They are automatically applied once the current startcode matches.
<layout>  ()                { beginBlock    }
<empty>   ()                { endEmptyBlock }
<newline> () / { notAtEnd } { offsideRule   }

{
-------------------------------------------------------------------------------
-- Actions

-- | An action performed by the lexer.
--   Some actions may not necessarily produce a token 
--   (such as opening/closing a comment), so 'Action' produces a 'Maybe' 'LHsToken'.
type Action = LHsPos -> Text -> LHsParser (Maybe LHsToken)

-- | Lifts a 'LHsToken' into an 'Action'.
token :: LHsToken -> Action
token tok _ _ = return (Just tok)

-- | Lifts a function taking an identifier 
--   (namely, the constructors 'LHsTkVar' and 'LHsTkCon') into an 'Action'.
named :: (Text -> LHsToken) -> Action
named f _ name = return (Just (f name))

-- | Lifts a function taking an integer 
--   (namely, the constructor 'LHsTkInt') into an 'Action'.
numeric :: (Int -> LHsToken) -> Action
numeric f pos name = case Text.decimal name of
    Right (num, _) -> return (Just (f num))
    -- The following case should never occur by definition of the $digit rule.
    _ -> throwError (LexicalError pos name)

-- | Rejects a string that doesn't form a token, emitting a 'LexicalError'.
lexicalError :: Action
lexicalError pos sym = throwError (LexicalError pos sym)

-- | Opens a multi-line comment, pushing the @comment@ startcode onto the stack.
beginComment :: Action
beginComment pos _ = Nothing <$ pushStcd comment pos

-- | Closes a multi-line comment, popping a startcode from the stack.
endComment :: Action
endComment _ _ = Nothing <$ popStcd

-- | Pushes the @newline@ startcode onto the stack.
beginNewline :: Action
beginNewline pos _ = Nothing <$ pushStcd newline pos

-- | Produces a layout keyword, pushing the @layout@ startcode onto the stack.
layoutKeyword :: LHsToken -> Action
layoutKeyword tok pos _ = Just tok <$ pushStcd layout pos

-- | Opens a layout block at the column of the token immediately succeeding a layout keyword.
--   The block is considered empty when that token isn't indented further than the enclosing block.
beginBlock :: Action
beginBlock pos@(LHsPos _ c) _ = do
    popStcd
    encl <- getInnermostLayout
    if maybe True (c >) encl
    then pushLayout c
    else pushStcd empty pos
    return (Just LHsTkVOpen)

-- | Closes an empty layout block and pushes the @newline@ startcode onto the stack.
--   This way, the token that follows is checked against the offside rule.
endEmptyBlock :: Action
endEmptyBlock pos _ = do
    popStcd
    pushStcd newline pos
    return (Just LHsTkVClose)

-- | Compares the column of the first token on a line with the innermost layout block.
offsideRule :: Action
offsideRule (LHsPos _ c) _ = do
    encl <- getInnermostLayout
    case fmap (compare c) encl of
        -- If the column is indented the same, we emit a virtual semicolon.
        -- > let x = 5
        -- >     y = 2 in x+y
        -- > --  ^ These declarations are in the same column.
        Just EQ -> Just LHsTkVSemi  <$ popStcd
        -- If the column is less indented, emit a virtual closing brace, and apply the rule again.
        -- > a = let x = 5 
        -- >       y = 10 in x+y
        -- --      ^ This declaration is less indented, so the layout block terminates at x = 5.
        -- --        (Note that this thus isn't a valid Haskell program.)
        Just LT -> Just LHsTkVClose <$ popLayout
        -- Otherwise, we continue as normal.
        _       -> Nothing <$ popStcd

-- | A predicate that holds provided that there is input remaining.
notAtEnd :: () -> AlexInput -> Int -> AlexInput -> Bool
notAtEnd _ _ _ inp = not (Text.null (aiText inp))

-------------------------------------------------------------------------------
-- Lexical tokenization

-- | Scans a /single/ 'LHsToken' from the input, producing a 'LHsLexeme'.
scanToken :: LHsParser LHsLexeme
scanToken = do
    inp <- getInput
    stcd <- getStcd
    let pos  = aiPos inp
    case alexScan inp stcd of
        AlexEOF                -> eof pos
        AlexError _            -> throwError (LexicalError pos (Text.take 1 (aiText inp)))
        AlexSkip inp' _        -> setInput inp' >> scanToken
        AlexToken inp' len act -> do
            setInput inp'
            act pos (Text.take len (aiText inp)) >>= maybe scanToken (return . LHsLexeme pos)
    where
        -- | Handles the end of the input. We either have an 'UnclosedComment' error,
        --   or every layout block still open is closed before we produce the end-of-file token.
        eof :: LHsPos -> LHsParser LHsLexeme
        eof pos = do
            st <- gets (\st -> ([p | LHsStcd c p <- stStcds st, c == comment], stLayout st))
            case st of
                (posns@(_:_), _) -> throwError (UnclosedComment (last posns))
                ([], _:_) -> LHsLexeme pos LHsTkVClose <$ popLayout
                ([], [])  -> return (LHsLexeme pos LHsTkEOF)

-- | Tokenizes the /entirety/ of the input, producing a (potential) list of 'LHsLexeme's.
--   If the input is a program, one should use 'scanTokensAsProgram' instead.
scanTokens :: Text -> Either LHsError [LHsLexeme]
scanTokens = runLHsParser scan
    where
        scan = do
            lxm <- scanToken
            case lxm of
                LHsLexeme _ LHsTkEOF -> return []
                _ -> fmap (lxm:) scan

{-# WARNING beginProgram "This should be invoked /before/ lexing a program. Otherwise, layout rules won't be applied to top-level declarations. See 'scanTokensAsProgram'." #-}
-- | Immediately pushes the @layout@ startcode onto the stack, indicating the beginning of a program.
beginProgram :: LHsParser ()
beginProgram = pushStcd layout (LHsPos 1 1)

-- | Tokenizes the /entirety/ of the input as a program, producing a (potential) list of 'LHsLexeme's.
--   See 'beginProgram'.
scanTokensAsProgram :: Text -> Either LHsError [LHsLexeme]
scanTokensAsProgram = runLHsParser (beginProgram >> scan)
    where
        scan = do
            lxm <- scanToken
            case lxm of
                LHsLexeme _ LHsTkEOF -> return []
                _ -> fmap (lxm:) scan
}