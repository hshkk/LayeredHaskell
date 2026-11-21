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

import Data.Sequence (Seq)

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
    int     { LHsLexeme _ (LHsTkInt _) }
    '+'     { LHsLexeme _ LHsTkPlus    }
    '-'     { LHsLexeme _ LHsTkMinus   }
    '*'     { LHsLexeme _ LHsTkTimes   }
    '||'    { LHsLexeme _ LHsTkOr      }
    '&&'    { LHsLexeme _ LHsTkAnd     }
    '<'     { LHsLexeme _ LHsTkLt      }
    '>'     { LHsLexeme _ LHsTkGt      }
    '<='    { LHsLexeme _ LHsTkLte     }
    '>='    { LHsLexeme _ LHsTkGte     }
    '=='    { LHsLexeme _ LHsTkEqEq    }
    '/='    { LHsLexeme _ LHsTkNotEq   }
    'if'    { LHsLexeme _ LHsTkIf      }
    'then'  { LHsLexeme _ LHsTkThen    }
    'else'  { LHsLexeme _ LHsTkElse    }
    '('     { LHsLexeme _ LHsTkLParen  }
    ')'     { LHsLexeme _ LHsTkRParen  }

%right    'else'
%left     '||'
%left     '&&'
%nonassoc '==' '/='
%nonassoc '<' '>' '<=' '>='
%left     '+' '-'
%left     '*'
-- Negation takes the highest precedence.
%left NEG

%%

-------------------------------------------------------------------------------
-- Rules

-------------------------------------------------------------------------------
-- Expressions

Expr :: { L LHsExpr }
    : 'if' Expr 'then' Expr 'else' Expr 
                            { L ($1 >< $6) (LHsIf $2 $4 $6)      }
    | Expr '+' Expr         { L ($1 >< $3) (LHsBin LHsAdd $1 $3) }
    | Expr '-' Expr         { L ($1 >< $3) (LHsBin LHsSub $1 $3) }
    | Expr '*' Expr         { L ($1 >< $3) (LHsBin LHsMul $1 $3) }
    | Expr '||' Expr        { L ($1 >< $3) (LHsBin LHsOr  $1 $3) }
    | Expr '&&' Expr        { L ($1 >< $3) (LHsBin LHsAnd $1 $3) }
    | Expr '<' Expr         { L ($1 >< $3) (LHsBin LHsLt  $1 $3) }
    | Expr '>' Expr         { L ($1 >< $3) (LHsBin LHsGt  $1 $3) }
    | Expr '<=' Expr        { L ($1 >< $3) (LHsBin LHsLte $1 $3) }
    | Expr '>=' Expr        { L ($1 >< $3) (LHsBin LHsGte $1 $3) }
    | Expr '==' Expr        { L ($1 >< $3) (LHsBin LHsEq  $1 $3) }
    | Expr '/=' Expr        { L ($1 >< $3) (LHsBin LHsNeq $1 $3) }
    | '-' Expr %prec NEG    { L ($1 >< $2) (LHsNeg $2)           }

Atom :: { L LHsExpr }
    : int         { int $1 }

Bin :: { LHsBin }
    : '+'         { LHsAdd }
    | '-'         { LHsSub }
    | '*'         { LHsMul }
    | '||'        { LHsOr  }
    | '&&'        { LHsAnd }
    | '<'         { LHsLt  }
    | '>'         { LHsGt  }
    | '<='        { LHsLte }
    | '>='        { LHsGte }
    | '=='        { LHsEq  }
    | '/='        { LHsNeq }

-------------------------------------------------------------------------------
-- Patterns

PatCon :: { L LHsPat }
    : '-' int     { pneg $1 $2 }

PatAtom :: { L LHsPat }
    : int         { pint $1 }
    -- This rule is more general than it needs to be due to an
    -- unintended interaction with other features. 
    -- This shouldn't cause any issues, since B only defines True and False as valid pattern constructors.
    | con         { L (toSpan $1) (LHsPCon (ident $1) Seq.empty) }

-------------------------------------------------------------------------------
-- Types

TyAtom :: { L LHsType }
    -- This rule is more general than it needs to be due to an
    -- unintended interaction with other features. 
    -- This shouldn't cause any issues, since B only defines Int and Bool as valid type constructors.
    : con         { L (toSpan $1) (LHsTyCon (ident $1) Seq.empty) }

{
-- | Parses an integer.
int :: LHsLexeme -> L LHsExpr
int (LHsLexeme sp (LHsTkInt n)) = L sp (LHsInt n)
-- The below case will never happen.
int _ = undefined

-- | Parses an integer as a pattern.
pint :: LHsLexeme -> L LHsPat
pint (LHsLexeme sp (LHsTkInt n)) = L sp (LHsPInt n)
-- The below case will never happen.
pint _ = undefined

-- | Parses a negative integer as a pattern.
pneg :: LHsLexeme -> LHsLexeme -> L LHsPat
pneg (LHsLexeme sp1 _) (LHsLexeme sp2 (LHsTkInt n)) 
    = L (sp1 >< sp2) (LHsPInt (negate n))
-- The below case will never happen.
pneg _ _ = undefined
}