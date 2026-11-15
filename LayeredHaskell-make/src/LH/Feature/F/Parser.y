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
import LH.Text.Located
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
    '\\' { LHsLexeme _ LHsTkLam        }
    '->' { LHsLexeme _ LHsTkArrow      }
    '_'  { LHsLexeme _ LHsTkUnderscore }
    '$'  { LHsLexeme _ LHsTkDollar     }
    '.'  { LHsLexeme _ LHsTkDot        }
    '('  { LHsLexeme _ LHsTkLParen     }
    ')'  { LHsLexeme _ LHsTkRParen     }

%left  LAM
%right '$'
%right '.'

%%

-------------------------------------------------------------------------------
-- Rules

-------------------------------------------------------------------------------
-- Expressions

Expr :: { L LHsExpr }
     : '\\' PatApp '->' Expr %prec LAM { L ($1 >< $4) (LHsLam (un $2) $4) }
     | Expr '$' Expr                   { L ($1 >< $3) (LHsBin LHsFunApp  $1 $3) }
     | Expr '.' Expr                   { L ($1 >< $3) (LHsBin LHsFunComp $1 $3) }

Atom :: { L LHsExpr }
    : '(' Bin ')' { L ($1 >< $3) (LHsPre $2) }

Bin :: { LHsBin }
    : '$'         { LHsFunApp  }
    | '.'         { LHsFunComp }

-------------------------------------------------------------------------------
-- Patterns

-- A sequence of patterns (separated by juxtaposition).
Pats :: { Seq (L LHsPat) }
     : PatApp         { un $1 }

PatAtom :: { L LHsPat }
    : var             { L (toSpan $1) (LHsPVar (ident $1)) }
    | '_'             { L (toSpan $1) LHsPWild }

-------------------------------------------------------------------------------
-- Types

Type :: { L LHsType }
    : TyApp '->' Type { L ($1 >< $3) (LHsTyFn $1 $3) }

