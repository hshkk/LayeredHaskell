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
    '++' { LHsLexeme _ LHsTkPlusPlus }
    ':'  { LHsLexeme _ LHsTkColon    }
    '['  { LHsLexeme _ LHsTkLBrack   }
    ']'  { LHsLexeme _ LHsTkRBrack   }
    ','  { LHsLexeme _ LHsTkComma    }
    '!!' { LHsLexeme _ LHsTkExclExcl }

%right ':' '++'
%left  '!!'

%%

-------------------------------------------------------------------------------
-- Rules

-------------------------------------------------------------------------------
-- Expressions

Expr :: { L LHsExpr }
    : '[' ListExpr ']'                        { L ($1 >< $3) (LHsList $2)                  }
    | '(' Expr ',' Expr CommaExprs ')'        { L ($1 >< $6) (LHsTuple ($2 <| ($4 <| $5))) }
    | Expr ':' Expr                           { L ($1 >< $3) (LHsBin LHsListCons $1 $3)    }
    | Expr '++' Expr                          { L ($1 >< $3) (LHsBin LHsListAppend $1 $3)  }
    | Expr '!!' Expr                          { L ($1 >< $3) (LHsBin LHsListIndex $1 $3)   }

Bin :: { LHsBin }
    : ':'                                     { LHsListCons   }
    | '++'                                    { LHsListAppend }
    | '!!'                                    { LHsListIndex  }

ListExpr :: { Seq (L LHsExpr) }
    : Expr CommaExprs                         { $1 <| $2 }
    | {- ε -}                                 { Seq.empty }

CommaExprs :: { Seq (L LHsExpr) }
    : ',' Expr CommaExprs                     { $2 <| $3 }
    | {- ε -}                                 { Seq.empty }

-------------------------------------------------------------------------------
-- Patterns

Pat :: { L LHsPat }
    : PatAtom ':' Pat                         { L ($1 >< $3) (LHsPListCons $1 $3) }

PatAtom :: { L LHsPat }
    : '[' ListPat ']'                         { L ($1 >< $3) (LHsPList $2) }
    | '(' Pat ',' Pat CommaPats ')'           { L ($1 >< $6) (LHsPTuple ($2 <| ($4 <| $5))) }

ListPat :: { Seq (L LHsPat) }
    : Pat CommaPats                           { $1 <| $2  }
    | {- ε -}                                 { Seq.empty }

CommaPats :: { Seq (L LHsPat) }
    : ',' Pat CommaPats                       { $2 <| $3  }
    | {- ε -}                                 { Seq.empty }

-------------------------------------------------------------------------------
-- Types

TyAtom :: { L LHsType }
    : '[' Type ']'                     { L ($1 >< $3) (LHsTyList $2) }
    | '(' Type ',' Type CommaTys ')'   { L ($1 >< $6) (LHsTyTuple ($2 <| ($4 <| $5))) }

CommaTys :: { Seq (L LHsType) }
    : ',' Type CommaTys                { $2 <| $3 }
    | {- ε -}                          { Seq.empty }
