{
{- |
Module      : LH.Feature.Parser
Description : The parser for B.
Copyright   : (c) Hangil Kim, 2026
                  Kairon Johnson, 2026
                  Leonidas Sallos, 2026
                  Oscar Abelanet, 2026
License     : MIT
Maintainer  : kimhang@oregonstate.edu
Stability   : experimental

This module provides a parser for Layered Haskell programs with B enabled.
-}
{-# LANGUAGE OverloadedStrings #-}
module LH.Feature.Parser (
    -- * Parsing
    parseProgram
    ) where

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
    int    { LHsLexeme _ (LHsTkInt $$)     }
    '+'    { LHsLexeme _ LHsTkPlus         }
    '-'    { LHsLexeme _ LHsTkMinus        }
    '*'    { LHsLexeme _ LHsTkTimes        }
    '||'   { LHsLexeme _ LHsTkOr           }
    '&&'   { LHsLexeme _ LHsTkAnd          }
    '<'    { LHsLexeme _ LHsTkLt           }
    '>'    { LHsLexeme _ LHsTkGt           }
    '<='   { LHsLexeme _ LHsTkLte          }
    '>='   { LHsLexeme _ LHsTkGte          }
    '=='   { LHsLexeme _ LHsTkEqEq         }
    '/='   { LHsLexeme _ LHsTkNotEq        }
    'if'   { LHsLexeme _ LHsTkIf           }
    'then' { LHsLexeme _ LHsTkThen         }
    'else' { LHsLexeme _ LHsTkElse         }
    'Int'  { LHsLexeme _ (LHsTkCon "Int")  }
    'Bool' { LHsLexeme _ (LHsTkCon "Bool") }

%right 'else'
%left '||'
%left '&&'
%nonassoc '==' '/='
%nonassoc '<' '>' '<=' '>='
%left '+' '-'
%left '*'
-- Negation takes the highest precedence.
%left NEG

%%

-------------------------------------------------------------------------------
-- Rules

-------------------------------------------------------------------------------
-- Expressions

Expr :: { LHsExpr }
    : 'if' Expr 'then' Expr 'else' Expr 
                            { LHsIf $2 $4 $6 }
    | Expr '+' Expr         { LHsBin LHsAdd $1 $3 }
    | Expr '-' Expr         { LHsBin LHsSub $1 $3 }
    | Expr '*' Expr         { LHsBin LHsMul $1 $3 }
    | Expr '||' Expr        { LHsBin LHsOr  $1 $3 }
    | Expr '&&' Expr        { LHsBin LHsAnd $1 $3 }
    | Expr '<' Expr         { LHsBin LHsLt  $1 $3 }
    | Expr '>' Expr         { LHsBin LHsGt  $1 $3 }
    | Expr '<=' Expr        { LHsBin LHsLte $1 $3 }
    | Expr '>=' Expr        { LHsBin LHsGte $1 $3 }
    | Expr '==' Expr        { LHsBin LHsEq  $1 $3 }
    | Expr '==' Expr        { LHsBin LHsNeq $1 $3 }
    | '-' Expr %prec NEG    { LHsNeg $2 }

Atom :: { LHsExpr }
    : int    { LHsInt $1 }

-------------------------------------------------------------------------------
-- Types

TyAtom :: { LHsType }
    : 'Int'  { LHsTyCon "Int"  }
    | 'Bool' { LHsTyCon "Bool" }
