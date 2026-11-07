{
{- |
Module      : LH.Feature.Parser
Description : The parser for D.
Copyright   : (c) Hangil Kim, 2026
                  Kairon Johnson, 2026
                  Leonidas Sallos, 2026
                  Oscar Abelanet, 2026
License     : MIT
Maintainer  : kimhang@oregonstate.edu
Stability   : experimental

This module provides a parser for Layered Haskell programs with D enabled.
-}
module LH.Feature.Parser (
    -- * Parsing
    parseProgram
    ) where

import Control.Monad.Except (throwError)

import Data.Sequence (Seq, (|>))

import qualified Data.Sequence as Seq

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
    con    { LHsLexeme _ (LHsTkCon $$) }
    'data' { LHsLexeme _ LHsTkData }
    '='    { LHsLexeme _ LHsTkEq }
    '|'    { LHsLexeme _ LHsTkPipe }
    '('    { LHsLexeme _ LHsTkLParen }
    ')'    { LHsLexeme _ LHsTkRParen }

%right 'in'

%%

-------------------------------------------------------------------------------
-- Rules

-------------------------------------------------------------------------------
-- Declarations

-- A sequence of constructor declarations (separated by pipes).
Cons :: { Seq (Id, Seq LHsType) }
    : {- ε -}         { Seq.empty }
    | Con             { Seq.singleton $1 }
    | Cons '|' Con    { $1 |> $3 }

-- A constructor declaration.
Con :: { (Id, Seq LHsType) }
    : con             { ($1, Seq.empty) }
    | Con TyAtom      { let (c, xs) = $1 in (c, xs |> $2) }
    -- ^ TODO: Add type applications to TyAtom in P.

-- A single declaration.
Decl :: { LHsDecl }
    : 'data' con TyVars '=' Cons { LHsData $2 $3 $5 }

-- Type variables. (D has none.)
TyVars :: { Seq Id }
    : {- ε -}         { Seq.empty }

-------------------------------------------------------------------------------
-- Types

TyAtom :: { LHsType }
    : con  { LHsTyCon $1 }
