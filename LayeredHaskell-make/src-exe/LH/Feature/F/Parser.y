{
{- |
Module      : LH.Feature.Parser
Description : The parser for F.
Copyright   : (c) Hangil Kim, 2026
                  Kairon Johnson, 2026
                  Leonidas Sallos, 2026
                  Oscar Abelanet, 2026
License     : MIT
Maintainer  : kimhang@oregonstate.edu
Stability   : experimental

This module provides a parser for Layered Haskell programs with F enabled.
-}
module LH.Feature.Parser (
    -- * Parsing
    parseProgram
    ) where

import Data.Sequence (Seq, (<|), (|>))

import LH.Feature.AbsSyn
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
    '\\' { LHsLexeme _ LHsTkLam }
    '->' { LHsLexeme _ LHsTkArrow }
    '_'  { LHsLexeme _ LHsTkUnderscore }

%left LAM

%%

-------------------------------------------------------------------------------
-- Rules

-------------------------------------------------------------------------------
-- Declarations

-- A sequence of patterns (separated by juxtaposition).
Pats :: { Seq LHsPat }
     : PatAtom Pats { $1 <| $2 }

-------------------------------------------------------------------------------
-- Expressions

Expr :: { LHsExpr }
     : '\\' Pat Pats '->' Expr %prec LAM { LHsLam ($2 <| $3) $5 }

-------------------------------------------------------------------------------
-- Patterns

Pat :: { LHsPat }
    : PatAtom      { $1 }

PatAtom :: { LHsPat }
    : var          { LHsPVar $1 }
    | '_'          { LHsPWild }
    | '(' Pat ')'  { $2 }

-------------------------------------------------------------------------------
-- Types

Type :: { LHsType }
    : TyAtom '->' Type { LHsTyFn $1 $3 }
