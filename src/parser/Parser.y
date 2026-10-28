{
module Parser(parse) where

import Lexer(scan, Token(..))

#ifdef E
data Expr
    = BindExpr String Expr
    | AtomExpr Atom
    | AtomsExpr Expr Atom
    | LetExpr [Expr] Expr
#ifdef B
    | IfExpr Expr Expr Expr
    | OpExpr Op Expr Expr
    | NotExpr Expr
#endif
#ifdef F
    | FuncExpr String [Patt] Expr
#endif
#endif

#ifdef E
data Atom
    = AVar String
    | ACon String
    | AScope Expr
#ifdef B
    | ATypeInt
    | ATypeBool
    | AInt Int
    | ATrue
    | AFalse
#endif
#endif

#ifdef B
data Op
    = OpPlus
    | OpMinus
    | OpMult
    | OpEq
    | OpAnd
#endif

#ifdef F
data Patt
    = VarPat String
    | ConPat String
    | WildPat
    | ContextPat Patt
#endif

#ifdef F
newtype Program = Program [Decl]

data Decl
    = BindDecl Expr
    | FuncDecl Expr
#endif
}

%name genParser

%tokenType { Token }
%token
    varid   { TVarid $$ }
    conid   { TConid $$ }
    integer { TInt $$ }
    "let"   { TLet }
    "("     { TOParen }
    ")"     { TCParen }
    ";"     { TSemicolon }
    "="     { TAssign }
    "{"     { TOBrace }
    "}"     { TCBrace }
    "in"    { TIn }
    '_'     { TUnderscore }
    int     { TInt }
    bool    { TBool }
    true    { TTrue }
    false   { TFalse }
    "if"    { TIf }
    "then"  { TThen }
    "else"  { TElse }
    "not"   { TNot }
    '+'     { TPlus }
    '-'     { TMinus }
    '*'     { TMult }
    "=="    { TEq }
    "&&"    { TAnd }

%%

#ifdef E
binding :: { Expr }
binding : varid '=' expr    { BindExpr $1 $3 }

bindings :: { [Expr] }
bindings : binding ';' bindings { [$1] ++ $3 }
         | binding              { [$1] }

atom :: { Atom }
atom : varid        { AVar $1 }
     | conid        { ACon $1 }
     | '(' expr ')' { AScope $2 }

expr :: { Expr }
expr : atom                             { AtomExpr $1 }
     | expr atom                        { AtomsExpr $1 $2 }
     | "let" '{' bindings '}' "in" expr { LetExpr $3 $6 }
#endif

#ifdef B
basicType :: { Atom }
basicType : int     { ATypeInt }
          | bool    { ATypeBool }

basicAtom :: { Atom }
basicAtom : integer { AInt $1 }
          | true    { ATrue }
          | false   { AFalse }

basicExpr :: { Expr }
basicExpr : "if" expr "then" expr "else" expr   { IfExpr $2 $4 $6 }
          | expr basicOp expr                   { OpExpr $2 $1 $3 }
          | "not" atom                          { NotExpr $2 }

basicOp :: { Op }
basicOp : '+'   { OpPlus }
        | '-'   { OpMinus }
        | '*'   { OpMult }
        | "=="  { OpEq }
        | "&&"  { OpAnd }
#endif

#ifdef F
program :: { Program }
program : '{' declarations '}'  { Program $2 }

declarations :: { [Decl] }
declarations : declaration ';' declarations { [$1] ++ [$3] }
             | declaration                  { [$1] }

declaration :: { Decl }
declaration : binding       { BindDecl $1 }
            | functionDef   { FuncDecl $1 }

functionDef :: { Expr }
functionDef : varid patts '=' expr  { FuncExpr $1 $2 $4 }

patts :: { [Patt] }
patts : patt patts  { [$1] ++ $2 }
      | patt        { [$1] }

patt :: { Patt }
patt : varid        { VarPat $1 }
     | conid        { ConPat $1 }
     | '_'          { WildPat }
     | '(' patt ')' { ContextPat $2 }
#endif

#ifdef D
#endif

{
happyError :: [Token] -> a
happyError = error ("Parse error\n")

parse :: String -> Program
parse = genParser . scan
}
