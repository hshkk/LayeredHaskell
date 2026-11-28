{
{- |
Module      : LH.Feature.Parser
Description : The parser for E.
Copyright   : (c) Hangil Kim, 2026
                  Kairon Johnson, 2026
                  Leonidas Sallos, 2026
                  Oscar Abelanet, 2026
License     : MIT
Maintainer  : kimhang@oregonstate.edu
Stability   : experimental

This module provides a parser for Layered Haskell programs with E enabled.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK hide #-}
module LH.Feature.Parser (
    -- * Parsing
    parseProgram
    ) where

import Control.Monad.Except (throwError)
import Control.Monad.State.Strict (get)

import Data.Sequence (Seq, (|>), (<|))
import Data.Text     (Text)

import qualified Data.Sequence as Seq

import LH.Data.Error (quotes, (<+>))
import LH.Data.SrcLoc
import LH.Feature.AbsSyn
import LH.Feature.Parser.Error
import LH.Text.Lexer
import LH.Text.Parser.Monad
import LH.Text.Token
}

%name program Block

%tokentype { LHsLexeme }
%monad     { LHsParser }
%lexer     { lexer } { LHsLexeme _ LHsTkEOF }
%error     { parseError }

-- There must be no shift/reduce conflicts. 
%expect 0

%token
    var   { LHsLexeme _ (LHsTkVar _)    }
    con   { LHsLexeme _ (LHsTkCon _)    }
    'let' { LHsLexeme _ LHsTkLet        }
    'in'  { LHsLexeme _ LHsTkIn         }
    '='   { LHsLexeme _ LHsTkEq         }
    '::'  { LHsLexeme _ LHsTkColonColon }
    '('   { LHsLexeme _ LHsTkLParen     }
    ')'   { LHsLexeme _ LHsTkRParen     }
    OPEN  { LHsLexeme _ LHsTkVOpen      }
    SEMI  { LHsLexeme _ LHsTkVSemi      }
    CLOSE { LHsLexeme _ LHsTkVClose     }

%right 'in'

%%

-------------------------------------------------------------------------------
-- Rules

-- A block of declarations, which also serves as a program.
Block :: { Seq (L LHsDecl) }
    : OPEN Decls End { $2 }

-- The end of a block.
-- Outside of a virtual closing brace, a parse error can also end a block!
End :: { () }
    : CLOSE { () }
    | error {% popLayout }

-------------------------------------------------------------------------------
-- Declarations

-- A sequence of declarations (separated by virtual semicolons).
Decls :: { Seq (L LHsDecl) }
    : {- ε -}         { Seq.empty }
    | Decl            { Seq.singleton $1 }
    | Decls SEMI      { $1 }
    | Decls SEMI Decl { $1 |> $3 }

-- A single declaration.
Decl :: { L LHsDecl }
    : var Pats '=' Expr { L ($1><$4) (LHsEqn (ident $1) $2 $4) }
    | var '::' Type     { L ($1><$3) (LHsSig (ident $1) $3) }

-------------------------------------------------------------------------------
-- Expressions

Expr :: { L LHsExpr }
    : 'let' Block 'in' Expr { L ($1><$4) (LHsLet $2 $4) }
    | App                   { $1 }

App :: { L LHsExpr }
    : App Atom              { L ($1><$2) (LHsApp $1 $2) }
    | Atom                  { $1 }

Atom :: { L LHsExpr }
    : var                   { L (toSpan $1) (LHsVar (ident $1)) }
    | con                   { L (toSpan $1) (LHsCon (ident $1)) }
    | '(' Expr ')'          { L ($1><$3) (un $2) }

-------------------------------------------------------------------------------
-- Patterns

-- A sequence of patterns (separated by juxtaposition).
-- E contains no patterns, so this rule is empty.
Pats :: { Seq (L LHsPat) }
    : {- ε -}               { Seq.empty }

Pat :: { L LHsPat }
    : PatCon                { $1 }

PatCon :: { L LHsPat }
    : PatAtom               { $1 }

PatApp :: { L (Seq (L LHsPat)) }
    : PatAtom Pats          { L ($1 ><* $2) ($1 <| $2) }

PatAtom :: { L LHsPat }
    : '(' Pat ')'           { L ($1 >< $3) (un $2) }

-------------------------------------------------------------------------------
-- Types
--
-- The following rules are intentionally non-terminating, since E has no meaningful types.

Type :: { L LHsType }
    : TyApp                 { $1 }

TyApp :: { L LHsType }
    : TyAtom                { $1 }

TyAtom :: { L LHsType }
    : '(' Type ')'          { L ($1 >< $3) (un $2) }

{
-- | Gives the parser the next token. 
lexer :: (LHsLexeme -> LHsParser a) -> LHsParser a
lexer f = scanToken >>= f

-- | Parses a program, producing a sequence of declarations ('LHsDecl').
parseProgram :: Text -> Either LHsTextError (Seq (L LHsDecl))
parseProgram = runLHsParser (beginProgram >> program)

-- | Reports a token that the parser did not expect.
parseError :: LHsLexeme -> LHsParser a
parseError (LHsLexeme sp tok) = do
    st <- get
    let brs = stBrack st
    let nv  = stNVirt st
    throwError (ParseError sp tok (prev tok brs nv))
    where
        -- Finds a previous place in the source worth pointing at, and adds a label.
        prev :: LHsToken -> [LHsBracket] -> Maybe LHsSpan -> Maybe (LHsSpan, Text)
        prev tok brs nv
            | not (isVirtual tok)          = Nothing
            -- If a bracket is unclosed...
            | LHsBracket bty sp : _ <- brs = 
                Just (sp, "this" <+> quotes (pprOpenBracketType bty) <+> "is never closed")
            -- If a definition is incomplete...
            | isVirtual tok, Just sp <- nv = Just (sp, "this definition is incomplete from here")
            | otherwise                    = Nothing

-- | Parses an identifier.
ident :: LHsLexeme -> Id
ident (LHsLexeme sp (LHsTkVar x)) = L sp x
ident (LHsLexeme sp (LHsTkCon c)) = L sp c
-- The below case will never happen.
ident _ = undefined
}
