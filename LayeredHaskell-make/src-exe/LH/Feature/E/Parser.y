{
{- |
Module      : LH.Feature.Parser
Description : The parser for E.
Copyright   : (c) Hangil Kim, 2026
                  Kairon Johnson, 2026
                  Leonidas Sallos, 2026
                  Oscar Abelanet, 2026
License     : MIT
Maintainer  : kimhang@oregonstate.edu
Stability   : experimental

This module provides a parser for Layered Haskell programs with E enabled.
-}
module LH.Feature.Parser (
    -- * Parsing
    parseProgram
    ) where

import Control.Monad.Except (throwError)

import Data.Sequence (Seq, (|>))
import Data.Text     (Text)

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
    var   { LHsLexeme _ (LHsTkVar $$) }
    con   { LHsLexeme _ (LHsTkCon $$) }
    'let' { LHsLexeme _ LHsTkLet }
    'in'  { LHsLexeme _ LHsTkIn }
    '='   { LHsLexeme _ LHsTkEq }
    '::'  { LHsLexeme _ LHsTkColonColon }
    '('   { LHsLexeme _ LHsTkLParen }
    ')'   { LHsLexeme _ LHsTkRParen }
    OPEN  { LHsLexeme _ LHsTkVOpen }
    SEMI  { LHsLexeme _ LHsTkVSemi }
    CLOSE { LHsLexeme _ LHsTkVClose }

%right 'in'

%%

-------------------------------------------------------------------------------
-- Rules

-- A block of declarations, which also serves as a program.
Block :: { Seq LHsDecl }
    : OPEN Decls End { $2 }

-- The end of a block.
-- Outside of a virtual closing brace, a parse error can also end a block!
End :: { () }
    : CLOSE { () }
    | error {% popLayout }

-------------------------------------------------------------------------------
-- Declarations

-- A sequence of declarations (separated by virtual semicolons).
Decls :: { Seq LHsDecl }
    : {- ε -}         { Seq.empty }
    | Decl            { Seq.singleton $1 }
    | Decls SEMI      { $1 }
    | Decls SEMI Decl { $1 |> $3 }

-- A single declaration.
Decl :: { LHsDecl }
    : var Pats '=' Expr { LHsEqn $1 $2 $4 }
    | var '::' Type     { LHsSig $1 $3 }

-- A sequence of patterns (separated by juxtaposition).
-- E contains no patterns, so this rule is empty.
Pats :: { Seq LHsPat }
    : {- ε -}           { Seq.empty }

-------------------------------------------------------------------------------
-- Expressions

Expr :: { LHsExpr }
    : 'let' Block 'in' Expr { LHsLet $2 $4 }
    | App                   { $1 }

App :: { LHsExpr }
    : App Atom { LHsApp $1 $2 }
    | Atom     { $1 }

Atom :: { LHsExpr }
    : var          { LHsVar $1 }
    | con          { LHsCon $1 }
    | '(' Expr ')' { $2 }

-------------------------------------------------------------------------------
-- Types
--
-- The following rules are intentionally non-terminating, since E has no meaningful types.

Type :: { LHsType }
    : TyAtom { $1 }

TyAtom :: { LHsType }
    : '(' Type ')' { $2 }

{
-- | Gives the parser the next token. 
lexer :: (LHsLexeme -> LHsParser a) -> LHsParser a
lexer f = scanToken >>= f

-- | Parses a program, producing a sequence of declarations ('LHsDecl').
parseProgram :: Text -> Either LHsError (Seq LHsDecl)
parseProgram = runLHsParser (beginProgram >> program)

-- | Reports a token that the parser did not expect.
parseError :: LHsLexeme -> LHsParser a
parseError (LHsLexeme start tok) = do
    end <- fmap aiPos getInput
    throwError (ParseError start end tok)
}
