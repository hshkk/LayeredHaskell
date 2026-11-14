{
{- |
Module      : LH.Feature.Parser
Description : The parser for C.
Copyright   : (c) Hangil Kim, 2026
                  Kairon Johnson, 2026
                  Leonidas Sallos, 2026
                  Oscar Abelanet, 2026
License     : MIT
Maintainer  : kimhang@oregonstate.edu
Stability   : experimental

This module provides a parser for Layered Haskell programs with C enabled.
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
    'case' { LHsLexeme _ LHsTkCase }
    'of'   { LHsLexeme _ LHsTkOf }

%%

-------------------------------------------------------------------------------
-- Rules

-------------------------------------------------------------------------------
-- Expressions

Expr :: { LHsExpr }
     : 'case' Expr 'of' OPEN CaseAlt CaseAlts End { LHsCase $2 ($5 <| $6) }

CaseAlts :: { Seq (LHsPat, LHsExpr) }
         : SEMI CaseAlt CaseAlts                  { $2 <| $3 }
         | {- ε -}                                { Seq.empty }

CaseAlt :: { LHsAlt }
        : Pat '->' Expr                           { ($1, $3) }
