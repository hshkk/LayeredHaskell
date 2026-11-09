{
{- |
Module      : LH.Feature.Parser
Description : The parser for P.
Copyright   : (c) Hangil Kim, 2026
                  Kairon Johnson, 2026
                  Leonidas Sallos, 2026
                  Oscar Abelanet, 2026
License     : MIT
Maintainer  : kimhang@oregonstate.edu
Stability   : experimental

This module provides a parser for Layered Haskell programs with P enabled.
-}
module LH.Feature.Parser (
    -- * Parsing
    parseProgram
    ) where

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
    '->' { LHsLexeme _ LHsTkArrow }

%%

-------------------------------------------------------------------------------
-- Rules

-------------------------------------------------------------------------------
-- Declarations

-------------------------------------------------------------------------------
-- Types

-- Type variables.
TyVars :: { Seq Id }
       : var TyVars    { $1 <| $2 }

TyAtom :: { LHsType }
       : Con { $1 }
       | var { LHsTyPVar $1 }
