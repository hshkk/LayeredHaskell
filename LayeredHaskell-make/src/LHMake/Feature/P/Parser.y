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
}

%name program Block

%tokentype { LHsLexeme }
%monad     { LHsParser }
%lexer     { lexer } { LHsLexeme _ LHsTkEOF }
%error     { parseError }

-- There must be no shift/reduce conflicts. 
%expect 0

%token
    var    { LHsLexeme _ (LHsTkVar _) }

%%

-------------------------------------------------------------------------------
-- Types

TyApp :: { L LHsType }
       : con TyAtoms    { L ($1 >< $2) (LHsTyCon (ident $1) (un $2)) }

TyAtoms :: { L (Seq (L LHsType)) }
       : TyAtom         { L (toSpan $1) (Seq.singleton $1) }
       | TyAtoms TyAtom { L ($1 >< $2) (un $1 |> $2)       }

-- Type variables.
TyVars :: { Seq Id }
       : var TyVars     { ident $1 <| $2 }

TyAtom :: { L LHsType }
       : var            { L (toSpan $1) (LHsTyVar (ident $1)) }
