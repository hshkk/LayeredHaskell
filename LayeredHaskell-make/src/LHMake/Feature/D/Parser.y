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

import LH.Data.SrcLoc
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
    con    { LHsLexeme _ (LHsTkCon _) }
    'data' { LHsLexeme _ LHsTkData    }
    'type' { LHsLexeme _ LHsTkType    }
    '='    { LHsLexeme _ LHsTkEq      }
    '|'    { LHsLexeme _ LHsTkPipe    }
    '('    { LHsLexeme _ LHsTkLParen  }
    ')'    { LHsLexeme _ LHsTkRParen  }

%right 'in'

%%

-------------------------------------------------------------------------------
-- Rules

-------------------------------------------------------------------------------
-- Declarations

-- A sequence of constructor declarations (separated by pipes).
Cons :: { Seq (L (Id, Seq (L LHsType))) }
    : {- ε -}         { Seq.empty }
    | Con             { Seq.singleton $1 }
    | Cons '|' Con    { $1 |> $3 }

-- A constructor declaration.
Con :: { L (Id, Seq (L LHsType)) }
    : con             { L (toSpan $1) (ident $1, Seq.empty) }
    | Con TyAtom      { L ($1 >< $2) (let (c, xs) = un $1 in (c, xs |> $2)) }

-- A single declaration.
Decl :: { L LHsDecl }
    : 'data' con TyVars '=' Cons { L ($1 >< $4 ><* $5) (LHsData (ident $2) $3 (fmap un $5)) }
    | 'type' con TyVars '=' Type { L ($1 >< $5) (LHsType (ident $2) $3 $5) }

-------------------------------------------------------------------------------
-- Patterns

Pat :: { L LHsPat }
    : con PatApp   { L ($1 >< $2) (LHsPCon (ident $1) (un $2)) }

PatAtom :: { L LHsPat }
    : con          { L (toSpan $1) (LHsPCon (ident $1) Seq.empty) }

-------------------------------------------------------------------------------
-- Types

-- Type variables. (D has none.)
TyVars :: { Seq Id }
    : {- ε -}      { Seq.empty }

TyAtom :: { L LHsType }
    : con          { L (toSpan $1) (LHsTyCon (ident $1) Seq.empty) }
