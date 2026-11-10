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

-- A single declaration
Decl :: { LHsDecl }
    : var '::' Type     { LHsSig $1 $3 }
    | 'data' Con '=' Cons { LHsData $2 $4 }

-- A sequence of constructor declarations (separated by pipes).
Cons :: { Seq LHsType }
     : Con             { Seq.singleton $1 }
     | Cons '|' Con    { $1 |> $3 }

Con :: { LHsType }
    : con TyVars { LHsTyCon $1 $2 }

-------------------------------------------------------------------------------
-- Types

-- Type variables. (D has none.)
TyVars :: { Seq Id }
       : {- ε -}         { Seq.empty }

TyAtom :: { LHsType }
       : Con { $1 }
