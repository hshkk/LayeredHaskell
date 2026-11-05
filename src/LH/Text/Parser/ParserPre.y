{
{- |
Module      : LH.Text.Parser.Parser
Description : The parser.
Copyright   : (c) Hangil Kim, 2026
                  Kairon Johnson, 2026
                  Leonidas Sallos, 2026
                  Oscar Abelanet, 2026
License     : MIT
Maintainer  : kimhang@oregonstate.edu
Stability   : experimental

This module provides the parser for Layered Haskell.
Currently, the parser handles the λ-calculus and constructors when E and F
are defined.
-}
module LH.Text.Parser.Parser(parse) where

import Data.Text (Text, pack)

import LH.Text.Token
import LH.Text.Parser.Monad (LHsError)
import LH.Text.Lexer        (scanTokens)
import LH.Text.Parser.AST

}

%name genParser
%tokentype  { LHsLexeme }
%monad      { Either LHsError }{ >>= }{ return }
%error      { parseError }

%token
    var  { LHsLexeme _ (LHsTkVar $$) }
    cons { LHsLexeme _ (LHsTkCon $$) }
    '\\'  { LHsLexeme _ LHsTkLam }
    "->" { LHsLexeme _ LHsTkArrow }
    '('  { LHsLexeme _ LHsTkLParen }
    ')'  { LHsLexeme _ LHsTkRParen }
    eof  { LHsLexeme _ LHsTkEOF }

%%

program :: { Program }
program : declList  { Program $1 }

declList :: { [Decl] }
declList : declList decl    { $1 ++ [$2] }
         | decl             { [$1] }

-- Temp for early language dev
decl :: { Decl }
decl : expr { ExprDecl $1 }

#ifdef E
expr :: { Expr }
expr : atom         { AtomExpr $1 }
     | expr atom    { CompExpr $1 $2 }
#ifdef F
     | '\\' atom "->" expr   { LamExpr $2 $4 }
#endif

atom :: { Atom }
atom : var          { AVar $1 }
     | cons         { ACons $1 }
     | '(' expr ')' { AScope $2 }
#endif

{
-- | Handles parse errors.
parseError = error ("Parse error\n")

-- | Parses the /entirety/ of the input into a potenital AST.
parse :: Text -> Either LHsError Program
parse s = scanTokens s >>= genParser
}
