{
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE OverloadedStrings #-}
module LH.Feature.Parser (parseProgram) where
import Data.Sequence (Seq, (<|), (|>))
import LH.Data.SrcLoc
import LH.Feature.AbsSyn
import LH.Text.Lexer
import LH.Text.Parser.Monad
import LH.Text.Token
import Control.Monad.Except (throwError)
import qualified Data.Sequence as Seq
import Data.Text (Text)
}

%name program Block
%tokentype {LHsLexeme}
%monad {LHsParser}
%lexer {lexer} {LHsLexeme _ LHsTkEOF}
%error {parseError}
%expect 0
%token
  int {LHsLexeme _ (LHsTkInt _)}
%token
  '+' {LHsLexeme _ LHsTkPlus}
%token
  '-' {LHsLexeme _ LHsTkMinus}
%token
  '*' {LHsLexeme _ LHsTkTimes}
%token
  '||' {LHsLexeme _ LHsTkOr}
%token
  '&&' {LHsLexeme _ LHsTkAnd}
%token
  '<' {LHsLexeme _ LHsTkLt}
%token
  '>' {LHsLexeme _ LHsTkGt}
%token
  '<=' {LHsLexeme _ LHsTkLte}
%token
  '>=' {LHsLexeme _ LHsTkGte}
%token
  '==' {LHsLexeme _ LHsTkEqEq}
%token
  '/=' {LHsLexeme _ LHsTkNotEq}
%token
  'if' {LHsLexeme _ LHsTkIf}
%token
  'then' {LHsLexeme _ LHsTkThen}
%token
  'else' {LHsLexeme _ LHsTkElse}
%token
  '(' {LHsLexeme _ LHsTkLParen}
%token
  ')' {LHsLexeme _ LHsTkRParen}
%token
  var {LHsLexeme _ (LHsTkVar _)}
%token
  con {LHsLexeme _ (LHsTkCon _)}
%token
  'data' {LHsLexeme _ LHsTkData}
%token
  'type' {LHsLexeme _ LHsTkType}
%token
  '=' {LHsLexeme _ LHsTkEq}
%token
  '|' {LHsLexeme _ LHsTkPipe}
%token
  'let' {LHsLexeme _ LHsTkLet}
%token
  'in' {LHsLexeme _ LHsTkIn}
%token
  '::' {LHsLexeme _ LHsTkColonColon}
%token
  OPEN {LHsLexeme _ LHsTkVOpen}
%token
  SEMI {LHsLexeme _ LHsTkVSemi}
%token
  CLOSE {LHsLexeme _ LHsTkVClose}
%right 'in'
%right 'else'
%left '||'
%left '&&'
%nonassoc '==' '/='
%nonassoc '<' '>' '<=' '>='
%left '+' '-'
%left '*'
%left NEG

%%

Expr :: {L LHsExpr}
  : 'if' Expr 'then' Expr 'else' Expr {L ($1 >< $6) (LHsIf $2 $4 $6)}
  | Expr '+' Expr {L ($1 >< $3) (LHsBin LHsAdd $1 $3)}
  | Expr '-' Expr {L ($1 >< $3) (LHsBin LHsSub $1 $3)}
  | Expr '*' Expr {L ($1 >< $3) (LHsBin LHsMul $1 $3)}
  | Expr '||' Expr {L ($1 >< $3) (LHsBin LHsOr  $1 $3)}
  | Expr '&&' Expr {L ($1 >< $3) (LHsBin LHsAnd $1 $3)}
  | Expr '<' Expr {L ($1 >< $3) (LHsBin LHsLt  $1 $3)}
  | Expr '>' Expr {L ($1 >< $3) (LHsBin LHsGt  $1 $3)}
  | Expr '<=' Expr {L ($1 >< $3) (LHsBin LHsLte $1 $3)}
  | Expr '>=' Expr {L ($1 >< $3) (LHsBin LHsGte $1 $3)}
  | Expr '==' Expr {L ($1 >< $3) (LHsBin LHsEq  $1 $3)}
  | Expr '/=' Expr {L ($1 >< $3) (LHsBin LHsNeq $1 $3)}
  | '-' Expr %prec NEG {L ($1 >< $2) (LHsNeg $2)}
  | 'let' Block 'in' Expr {L ($1><$4) (LHsLet $2 $4)}
  | App {$1}

Atom :: {L LHsExpr}
  : int {int $1}
  | '(' Bin ')' {L ($1 >< $3) (LHsPre $2)}
  | var {L (toSpan $1) (LHsVar (ident $1))}
  | con {L (toSpan $1) (LHsCon (ident $1))}
  | '(' Expr ')' {L ($1><$3) (un $2)}

Bin :: {LHsBin}
  : '+' {LHsAdd}
  | '-' {LHsSub}
  | '*' {LHsMul}
  | '||' {LHsOr}
  | '&&' {LHsAnd}
  | '<' {LHsLt}
  | '>' {LHsGt}
  | '<=' {LHsLte}
  | '>=' {LHsGte}
  | '==' {LHsEq}
  | '/=' {LHsNeq}

PatAtom :: {L LHsPat}
  : int {pint $1}
  | con {L (toSpan $1) (LHsPCon (ident $1) Seq.empty)}
  | '(' Pat ')' {L ($1 >< $3) (un $2)}

TyAtom :: {L LHsType}
  : con {L (toSpan $1) (LHsTyCon (ident $1) Seq.empty)}
  | var {L (toSpan $1) (LHsTyVar (ident $1))}
  | '(' Type ')' {L ($1 >< $3) (un $2)}

TyApp :: {L LHsType}
  : con TyAtoms {L ($1 >< $2) (LHsTyCon (ident $1) (un $2))}
  | TyAtom {$1}

TyAtoms :: {L (Seq (L LHsType))}
  : TyAtom {L (toSpan $1) (Seq.singleton $1)}
  | TyAtoms TyAtom {L ($1 >< $2) (un $1 |> $2)}

TyVars :: {Seq Id}
  : var TyVars {ident $1 <| $2}
  | {Seq.empty}

Cons :: {Seq (L (Id, Seq (L LHsType)))}
  : {Seq.empty}
  | Con {Seq.singleton $1}
  | Cons '|' Con {$1 |> $3}

Con :: {L (Id, Seq (L LHsType))}
  : con {L (toSpan $1) (ident $1, Seq.empty)}
  | Con TyAtom {L ($1 >< $2) (let (c, xs) = un $1 in (c, xs |> $2))}

Decl :: {L LHsDecl}
  : 'data' con TyVars '=' Cons {L ($1 >< $4 ><* $5) (LHsData (ident $2) $3 (fmap un $5))}
  | 'type' con TyVars '=' Type {L ($1 >< $5) (LHsType (ident $2) $3 $5)}
  | var Pats '=' Expr {L ($1><$4) (LHsEqn (ident $1) $2 $4)}
  | var '::' Type {L ($1><$3) (LHsSig (ident $1) $3)}

Pat :: {L LHsPat}
  : con PatApp {L ($1 >< $2) (LHsPCon (ident $1) (un $2))}
  | PatAtom {$1}

Block :: {Seq (L LHsDecl)}
  : OPEN Decls End {$2}

End :: {()}
  : CLOSE {()}
  | error {% popLayout}

Decls :: {Seq (L LHsDecl)}
  : {Seq.empty}
  | Decl {Seq.singleton $1}
  | Decls SEMI {$1}
  | Decls SEMI Decl {$1 |> $3}

App :: {L LHsExpr}
  : App Atom {L ($1><$2) (LHsApp $1 $2)}
  | Atom {$1}

Pats :: {Seq (L LHsPat)}
  : {Seq.empty}

PatApp :: {L (Seq (L LHsPat))}
  : PatAtom Pats {L ($1 ><* $2) ($1 <| $2)}

Type :: {L LHsType}
  : TyApp {$1}

{
 
int :: LHsLexeme -> L LHsExpr
int (LHsLexeme sp (LHsTkInt n)) = L sp (LHsInt n)
int _ = undefined
 
pint :: LHsLexeme -> L LHsPat
pint (LHsLexeme sp (LHsTkInt n)) = L sp (LHsPInt n)
pint _ = undefined
 
lexer :: (LHsLexeme -> LHsParser a) -> LHsParser a
lexer f = scanToken >>= f
 
parseProgram :: Text -> Either LHsTextError (Seq (L LHsDecl))
parseProgram = runLHsParser (beginProgram >> program)
 
parseError :: LHsLexeme -> LHsParser a
parseError (LHsLexeme sp tok)
  = do end <- fmap aiPos getInput
       throwError (ParseError sp tok)
 
ident :: LHsLexeme -> Id
ident (LHsLexeme sp (LHsTkVar x)) = L sp x
ident (LHsLexeme sp (LHsTkCon c)) = L sp c
ident _ = undefined

}
