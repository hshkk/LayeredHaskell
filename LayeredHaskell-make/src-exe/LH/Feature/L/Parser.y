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
    '++' {LHsLexeme _ LHsTkPlusPlus }
    ':'  {LHsLexeme _ LHsTkColon }
    '['  {LHsLexeme _ LHsTkLBrack }
    ']'  {LHsLexeme _ LHsTkRBrack }
    ','  {LHsLexeme _ LHsTkComma }

%left '++'

%%

-------------------------------------------------------------------------------
-- Rules

-------------------------------------------------------------------------------
-- Declarations

-------------------------------------------------------------------------------
-- Expressions

Expr :: { LHsExpr }
    : '[' ListExpr ']'                   { LHsList $2 }
    | '(' Expr ',' Expr CommaSepExpr ')' { LHsTuple ($2 <| ($4 <| $5)) }
    | '(' Expr ':' Expr ChainExpr ')'    { LHsListChain ($2 <| ($4 <| $5)) }
    | Expr '++' Expr { LHsConcat $1 $3 }

ListExpr :: { Seq LHsExpr }
    : Expr CommaSepExpr { $1 <| $2 }
    | {- ε -}           { Seq.empty }

CommaSepExpr :: { Seq LHsExpr }
    : ',' Expr CommaSepExpr { $2 <| $3 }
    | {- ε -}               { Seq.empty }

ChainExpr :: { Seq LHsExpr }
    : ':' Expr ChainExpr { $2 <| $3 }
    | {- ε -}            { Seq.empty }

Pat :: { LHsPat }
    : '[' ListPat ']'                 { LHsPList $2 }
    | '(' Pat ',' Pat CommaSepPat ')' { LHsPTuple ($2 <| ($4 <| $5)) }
    | '(' Pat ':' Pat ChainPat ')'    { LHsPListChain ($2 <| ($4 <| $5)) }

ListPat :: { Seq LHsPat }
    : Pat CommaSepPat { $1 <| $2 }
    | {- ε -}         { Seq.empty }

CommaSepPat :: { Seq LHsPat }
    : ',' Pat CommaSepPat { $2 <| $3 }
    | {- ε -}             { Seq.empty }

ChainPat :: { Seq LHsPat }
    : ':' Pat ChainPat { $2 <| $3 }
    | {- ε -}          { Seq.empty }

-------------------------------------------------------------------------------
-- Types

TyAtom :: { LHsType }
    : '[' TyAtom ']'                       { LHsTyList $2 }
    | '(' TyAtom ',' TyAtom CommaSepTy ')' { LHsTyTuple ($2 <| ($4 <| $5)) }

CommaSepTy :: { Seq LHsType }
    : ',' TyAtom CommaSepTy { $2 <| $3 }
    | {- ε -}               { Seq.empty }
