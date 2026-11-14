{
{- |
Module      : LH.Feature.Parser
Description : The parser for L.
Copyright   : (c) Hangil Kim, 2026
                  Kairon Johnson, 2026
                  Leonidas Sallos, 2026
                  Oscar Abelanet, 2026
License     : MIT
Maintainer  : kimhang@oregonstate.edu
Stability   : experimental

This module provides a parser for Layered Haskell programs with L enabled.
-}
module LH.Feature.Parser (
    -- * Parsing
    parseProgram
    ) where

import Control.Monad.Except (throwError)

import Data.Sequence (Seq, (<|))

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
    '++' { LHsLexeme _ LHsTkPlusPlus }
    ':'  { LHsLexeme _ LHsTkColon }
    '['  { LHsLexeme _ LHsTkLBrack }
    ']'  { LHsLexeme _ LHsTkRBrack }
    ','  { LHsLexeme _ LHsTkComma }
    '!!' { LHsLexeme _ LHsTkExclExcl }

%right ':' '++'
%left  '!!'

%%

-------------------------------------------------------------------------------
-- Rules

-------------------------------------------------------------------------------
-- Expressions

Expr :: { LHsExpr }
    : '[' ListExpr ']'                        { LHsList $2 }
    | '(' Expr ',' Expr CommaExprs ')'        { LHsTuple ($2 <| ($4 <| $5)) }
    | Expr ':' Expr                           { LHsBin LHsCons $1 $3 }
    | Expr '++' Expr                          { LHsBin LHsAppend $1 $3 }
    | Expr '!!' Expr                          { LHsBin LHsIndex $1 $3 }

ListExpr :: { Seq LHsExpr }
    : Expr CommaExprs                         { $1 <| $2 }
    | {- ε -}                                 { Seq.empty }

CommaExprs :: { Seq LHsExpr }
    : ',' Expr CommaExprs                     { $2 <| $3 }
    | {- ε -}                                 { Seq.empty }

-------------------------------------------------------------------------------
-- Patterns

Pat :: { LHsPat }
    : PatAtom ':' Pat                         { LHsPListCons $1 $3 }

PatAtom :: { LHsPat }
    : '[' ListPat ']'                         { LHsPList $2 }
    | '(' Pat ',' Pat CommaPats ')'           { LHsPTuple ($2 <| ($4 <| $5)) }

ListPat :: { Seq LHsPat }
    : Pat CommaPats                           { $1 <| $2 }
    | {- ε -}                                 { Seq.empty }

CommaPats :: { Seq LHsPat }
    : ',' Pat CommaPats                       { $2 <| $3 }
    | {- ε -}                                 { Seq.empty }

-------------------------------------------------------------------------------
-- Types

TyAtom :: { LHsType }
    : '[' Type ']'                     { LHsTyList $2 }
    | '(' Type ',' Type CommaTys ')'   { LHsTyTuple ($2 <| ($4 <| $5)) }

CommaTys :: { Seq LHsType }
    : ',' Type CommaTys                { $2 <| $3 }
    | {- ε -}                          { Seq.empty }
