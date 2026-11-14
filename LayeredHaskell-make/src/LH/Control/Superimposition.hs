{- |
Module      : LH.Control.Superimposition
Description : Superimposition of Haskell and Happy ASTs.
Copyright   : (c) Hangil Kim, 2026
                  Kairon Johnson, 2026
                  Leonidas Sallos, 2026
                  Oscar Abelanet, 2026
License     : MIT
Maintainer  : kimhang@oregonstate.edu
Stability   : experimental

This module provides a superimposition operator @'(-->)'@ on Haskell and Happy abstract syntax trees.
The following example illustrates its use on two Haskell files.

> -- A.hs
> module LH.Feature.A.Basic where
>
> data Expr = Lit Int
> 
> eval :: Expr -> Int
> eval (Lit n) = n
> 
> -- B.hs
> module LH.Feature.B.Basic where
>
> data Expr = Add Expr Expr
> 
> eval :: Expr -> Int
> eval (Add e1 e2) = eval e1 + eval e2
> 
> -- B.hs --> A.hs
> module LH.Feature.BA.Basic where
>
> data Expr = Lit Int | Add Expr Expr
> 
> eval :: Expr -> Int
> eval (Lit n) = n
> eval (Add e1 e2) = eval e1 + eval e2
-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module LH.Control.Superimposition (
    -- * Superimposition
    Superimposable (..), Identifiable (..),
    -- ** Identifiers
    DeclId (..), DirectiveId (..),
    -- ** Flattening lists
    flattenDecls, flattenTokens,
    -- ** Token precedences
    precs
    ) where

import Control.Applicative ((<|>))
import Data.Char           (isSpace)
import Data.List           (nub, partition)
import Data.Map            (Map)
import Data.Maybe          (catMaybes)

import qualified Data.Map as Map

import qualified Happy.Frontend.AbsSyn        as Y
import qualified Happy.Frontend.PrettyGrammar as Y
import qualified Language.Haskell.Parser      as H
import qualified Language.Haskell.Pretty      as H
import qualified Language.Haskell.Syntax      as H

infixl 6 -->

-- | A 'Superimposable' type allows merging of two values \(L\) and \(R\),
--   which range over anything from Haskell modules to Happy productions.
--   \(L\) and \(R\) are assumed to have the same 'Identifier', as defined by the 'Identifiable' typeclass.
--
--   By default, \(L\) overrides the contents of \(R\).
--   Specific instances replace this behavior with a merging semantics.
class Superimposable a where
    -- | Superimposes two values.
    --   Note that superimposition can fail for various reasons, such as when differing module names are encountered.
    (-->) :: a -> a -> Either String a
    x --> _ = Right x

-- | An 'Identifiable' type @a@ has an 'Identifier', which is a value of some type identifying a value of type @a@.
--   'Identifier' is defined on a case-by-case basis, since a type @a@ might need a different identification mechanism than another type @b@.
--   The 'identify' function then takes a value and produces its 'Identifier'.
--   This allows us to decide whether two values can be merged in a general way.
class Identifiable a where
    type Identifier a
    -- | Identifies a value.
    identify :: a -> Either String (Identifier a)

-- | Lists are superimposed element by element. 
--   Elements with the same identifier are superimposed, and elements that only the right list has are added at the end.
instance (Identifiable a, Eq (Identifier a), Superimposable a) => Superimposable [a] where
    xs --> ys = do
        xids   <- traverse identify xs
        yids   <- traverse identify ys
        merged <- sequence [maybe (Right x) (x -->) (lookup xid (zip yids ys)) | (x, xid) <- zip xs xids]
        let added = [y | (y, yid) <- zip ys yids, yid `notElem` xids]
        return (merged ++ added)

-------------------------------------------------------------------------------
-- Haskell
-------------------------------------------------------------------------------

instance Superimposable H.HsQName
instance Superimposable H.HsCName
instance Superimposable H.HsConDecl
instance Superimposable H.HsMatch

instance Identifiable H.HsQName where
    type Identifier H.HsQName = H.HsQName
    identify = Right

instance Identifiable H.HsCName where
    type Identifier H.HsCName = H.HsName
    identify (H.HsVarName name) = Right name
    identify (H.HsConName name) = Right name

instance Identifiable H.HsConDecl where
    type Identifier H.HsConDecl = H.HsName
    identify (H.HsConDecl _ name _) = Right name
    identify (H.HsRecDecl _ name _) = Right name

-- | A function is identified by its patterns.
instance Identifiable H.HsMatch where
    type Identifier H.HsMatch = [H.HsPat]
    identify (H.HsMatch _ _ pats _ _) = Right pats

-- Modules --------------------------------------------------------------------

-- | To superimpose two Haskell modules, they must have the same name.
instance Superimposable H.HsModule where
    H.HsModule loc mod1 exps1 imps1 decls1 --> H.HsModule _ mod2 exps2 imps2 decls2
        | mod1 == mod2 = do
            exps  <- sequenceA (liftA2 (-->) exps1 exps2)
            imps  <- imps1 --> imps2
            decls <- concatMap flattenDecls decls1 --> concatMap flattenDecls decls2
            return (H.HsModule loc mod1 exps imps decls)
        | otherwise = Left ("Cannot merge differing modules: " ++ H.prettyPrint mod1 ++ " and " ++ H.prettyPrint mod2)

-- | Splits a declaration holding several names into one declaration per name.
--   Variable bindings (such as @x = e@) are also rewritten as function bindings, since they get parsed as a pattern binding.
--
--   For instance, the Haskell type signature @f g :: A@ is flattened into @f :: A@ and @g :: A@.
flattenDecls :: H.HsDecl -> [H.HsDecl]
flattenDecls (H.HsTypeSig loc names ty)         = [H.HsTypeSig loc [name] ty | name <- names]
flattenDecls (H.HsInfixDecl loc assoc prec ops) = [H.HsInfixDecl loc assoc prec [op] | op <- ops]
flattenDecls (H.HsPatBind loc (H.HsPVar name) rhs wheres)
    = [H.HsFunBind [H.HsMatch loc name [] rhs wheres]]
flattenDecls decl = [decl]

-- Exports --------------------------------------------------------------------

instance Superimposable H.HsExportSpec where
    x@(H.HsEThingAll _) --> _                      = Right x
    _ --> y@(H.HsEThingAll _)                      = Right y
    H.HsEThingWith name xs --> H.HsEThingWith _ ys = H.HsEThingWith name <$> (xs --> ys)
    H.HsEAbs _ --> y@(H.HsEThingWith _ _)          = Right y
    x --> _                                        = Right x

instance Identifiable H.HsExportSpec where
    type Identifier H.HsExportSpec = Either H.HsQName H.Module  -- an entity, or a whole module
    identify (H.HsEVar name)            = Right (Left  name)
    identify (H.HsEAbs name)            = Right (Left  name)
    identify (H.HsEThingAll name)       = Right (Left  name)
    identify (H.HsEThingWith name _)    = Right (Left  name)
    identify (H.HsEModuleContents modl) = Right (Right modl)

-- Imports --------------------------------------------------------------------

instance Superimposable H.HsImportDecl where
    x --> y = do
        specs <- sequenceA (liftA2 (-->) (H.importSpecs x) (H.importSpecs y))
        return (x { H.importSpecs = specs })

instance Identifiable H.HsImportDecl where
    type Identifier H.HsImportDecl = (H.Module, Bool, Maybe H.Module)
    identify imp = Right (H.importModule imp, H.importQualified imp, H.importAs imp)

-- | A list of module imports.
--   @(False, xs)@ imports only @xs@, and @(True, xs)@ imports everything except @xs@.
type ImportList = (Bool, [H.HsImportSpec])

instance Superimposable ImportList where
    (False, xs) --> (False, ys) = fmap ((,) False) (xs --> ys)
    (True,  xs) --> (True,  ys) = do
        xids <- traverse identify xs
        yids <- traverse identify ys
        return (True, [x | (x, xid) <- zip xs xids, xid `elem` yids])
    (False, xs) --> (True,  ys) = do
        xids <- traverse identify xs
        yids <- traverse identify ys
        return (True, [y | (y, yid) <- zip ys yids, yid `notElem` xids])
    (True,  xs) --> (False, ys) = do
        xids <- traverse identify xs
        yids <- traverse identify ys
        return (True, [x | (x, xid) <- zip xs xids, xid `notElem` yids])

instance Superimposable H.HsImportSpec where
    x@(H.HsIThingAll _) --> _                      = Right x
    _ --> y@(H.HsIThingAll _)                      = Right y
    H.HsIThingWith name xs --> H.HsIThingWith _ ys = fmap (H.HsIThingWith name) (xs --> ys)
    H.HsIAbs _ --> y@(H.HsIThingWith _ _)          = Right y
    x --> _                                        = Right x

instance Identifiable H.HsImportSpec where
    type Identifier H.HsImportSpec = H.HsName
    identify (H.HsIVar name)         = Right name
    identify (H.HsIAbs name)         = Right name
    identify (H.HsIThingAll name)    = Right name
    identify (H.HsIThingWith name _) = Right name

-- Declarations ---------------------------------------------------------------

-- | Identifiers for Haskell declarations.
--
--   Only these declarations can be merged. Everything else (classes, instances, default
--   declarations, foreign declarations, pattern bindings) is rejected by 'identify'.
--   We could consider writing definitions for these too, but we don't have a use for them right now.
data DeclId
    = Value  H.HsName  -- ^ Functions and variables.
    | Sig    H.HsName  -- ^ Type signatures.
    | Type   H.HsName  -- ^ Type synonyms, datatype declarations, and @newtype@s.
    | Fixity H.HsName  -- ^ Fixity declarations.
    deriving Eq

instance Identifiable H.HsDecl where
    type Identifier H.HsDecl = DeclId
    identify (H.HsTypeDecl _ name _ _)                  = Right (Type name)
    identify (H.HsDataDecl _ _ name _ _ _)              = Right (Type name)
    identify (H.HsNewTypeDecl _ _ name _ _ _)           = Right (Type name)
    identify (H.HsFunBind (H.HsMatch _ name _ _ _ : _)) = Right (Value name)
    identify (H.HsTypeSig _ [name] _)                   = Right (Sig name)
    identify (H.HsInfixDecl _ _ _ [H.HsVarOp name])     = Right (Fixity name)
    identify (H.HsInfixDecl _ _ _ [H.HsConOp name])     = Right (Fixity name)
    identify decl = Left ("Cannot merge this declaration: "
                          ++ takeWhile (/= '\n') (dropWhile isSpace (H.prettyPrint decl)))

-- | Datatype declarations and functions are merged.
--   Type synonyms, @newtype@s, type signatures, and fixity declarations are overridden from the left.
instance Superimposable H.HsDecl where
    H.HsDataDecl loc ctx name vars cons1 derivs1 --> H.HsDataDecl _ _ _ _ cons2 derivs2 = do
        cons   <- cons1 --> cons2
        derivs <- derivs1 --> derivs2
        return (H.HsDataDecl loc ctx name vars cons derivs)
    H.HsFunBind ms1 --> H.HsFunBind ms2
        | length (nub [length pats | H.HsMatch _ _ pats _ _ <- ms1 ++ ms2]) == 1
        = fmap H.HsFunBind (ms1 --> ms2)
    x --> _ = Right x

-------------------------------------------------------------------------------
-- Happy
-------------------------------------------------------------------------------

instance Superimposable (Y.Prod String)

-- | A production is identified by its RHS.
instance Identifiable (Y.Prod String) where
    type Identifier (Y.Prod String) = [String]
    identify (Y.Prod terms _ _ _) = Right (map (Y.render . Y.ppTerm) terms)

instance Superimposable (Y.Rule String) where
    Y.Rule name params1 prods1 ty1 --> Y.Rule _ params2 prods2 ty2
        | params1 == params2 = do
            prods <- prods1 --> prods2
            return (Y.Rule name params1 prods (ty1 <|> ty2))
    rule --> _ = Right rule

instance Identifiable (Y.Rule String) where
    type Identifier (Y.Rule String) = String
    identify (Y.Rule name _ _ _) = Right name

-- | @%token@ directives are merged and @%name@ directives are overridden from the left.
--   Other differing directives are simply rejected.
instance Superimposable (Y.Directive String) where
    x@(Y.TokenSpec _)  --> _ = Right x
    x@(Y.TokenName {}) --> _ = Right x
    x --> y
        | x == y    = Right x
        | otherwise = Left ("Cannot merge differing directives: " ++ show x ++ " and " ++ show y)

-- | Identifiers for Happy directives.
data DirectiveId
    = Token     String  -- ^ A @%token@ directive, one per token.
    | Name      String  -- ^ The @%name@ directive.
    | Attribute String  -- ^ The @%attribute@ directive.
    | Prec      String  -- ^ The @%left@, @%right@, or @%nonassoc@ directive.
    | Other     String  -- ^ Any other directive.
    deriving Eq

instance Identifiable (Y.Directive String) where
    type Identifier (Y.Directive String) = DirectiveId
    identify (Y.TokenSpec [(name, _)]) = Right (Token name)
    identify (Y.TokenName _ _ True)    = Left "Cannot merge %partial directives"
    identify (Y.TokenName name _ _)    = Right (Name name)
    identify (Y.TokenAttribute name _) = Right (Attribute name)
    identify (Y.TokenLeft [name])      = Right (Prec name)
    identify (Y.TokenRight [name])     = Right (Prec name)
    identify (Y.TokenNonassoc [name])  = Right (Prec name)
    identify dir                       = Right (Other (takeWhile (/= ' ') (show dir)))

-- | Splits @x a b@ into @x a@ and @x b@, where @x@ ranges over @%token@, @%left@, @%right@, and @%nonassoc@.
--   Leaves all other directives alone.
flattenTokens :: [Y.Directive String] -> [Y.Directive String]
flattenTokens = (=<<) flattenToken
    where
        flattenToken (Y.TokenSpec specs)    = map (Y.TokenSpec . return) specs
        flattenToken (Y.TokenLeft toks)     = map (Y.TokenLeft . return) toks
        flattenToken (Y.TokenRight toks)    = map (Y.TokenRight . return) toks
        flattenToken (Y.TokenNonassoc toks) = map (Y.TokenNonassoc . return) toks
        flattenToken dir = return dir

-- | Precedences of tokens, where a higher number binds tighter than a lower one.
--   The (relative) precedences of tokens corresponding to operators borrow directly from standard Haskell.
--   Operator associativity is defined within the parser files themselves.
-- 
--   ==== __Precedences__
--
--   > LAM                0
--   > 'in'               1
--   > 'else'             2
--   > '$'                3
--   > '||'               4
--   > '&&'               5
--   > '==' '/='          6
--   > '<' '>' '<=' '>='  7
--   > ':' '++'           8
--   > '+' '-'            9
--   > '*'               10
--   > NEG               11
--   > '.' '!!'          12
precs :: Map String Int
precs = Map.fromList
    [ ("LAM",    0)
    , ("'in'",   1)
    , ("'else'", 2)
    , ("'$'",    3)
    , ("'||'",   4)
    , ("'&&'",   5)
    , ("'=='",   6), ("'/='",  6)
    , ("'<'",    7), ("'>'",   7), ("'<='",  7), ("'>='", 7)
    , ("':'",    8), ("'++'",  8)
    , ("'+'",    9), ("'-'",   9)
    , ("'*'",   10)
    , ("NEG",   11)
    , ("'.'",   12), ("'!!'", 12)
    ]

-- | Groups precedence directives by the precedence and associativity of the involved tokens.
--   If a token with an unknown precedence is detected, an error will be thrown.
groupPrecs :: [Y.Directive String] -> Either String [Y.Directive String]
groupPrecs prios = do
    tokPrecs <- mapM precOf toks
    return
        [ dir toksAtPrec
        | prec <- [0 .. maximum precs]
        , dir  <- [Y.TokenLeft, Y.TokenRight, Y.TokenNonassoc]
        , let toksAtPrec = [tok | (tok, p) <- tokPrecs, p == prec, dir [tok] `elem` prios]
        , not (null toksAtPrec) ]
    where
        toks       = concatMap Y.getPrioNames prios
        precOf tok = maybe (Left ("Unknown precedence for: " ++ tok)) (\p -> Right (tok, p)) (Map.lookup tok precs)

instance Superimposable (Y.AbsSyn String) where
    Y.AbsSyn dirs1 rules1 --> Y.AbsSyn dirs2 rules2 = do
        merged  <- flattenTokens dirs1 --> flattenTokens dirs2
        rules   <- rules1 --> rules2
        let (prios, others) = partition (`elem` Y.getPrios merged) merged
        grouped <- groupPrecs prios
        return (Y.AbsSyn (others ++ grouped) rules)

-- | Happy grammars are superimposed by first superimposing the header and footer as a Haskell module.
--   Then, each rule is superimposed.
instance Superimposable Y.BookendedAbsSyn where
    Y.BookendedAbsSyn opts1 head1 syn1 foot1 --> Y.BookendedAbsSyn opts2 head2 syn2 foot2 = do
        modl1 <- parse head1 foot1
        modl2 <- parse head2 foot2
        H.HsModule loc name exps imps decls <- modl1 --> modl2
        syn <- syn1 --> syn2
        return (Y.BookendedAbsSyn (opts1 <|> opts2)
                            -- The header contains the Haskell module declaration and imports.
                            (Just (H.prettyPrint (H.HsModule loc name exps imps [])))
                            syn
                            -- The footer contains Haskell declarations.
                            (Just (unlines (map H.prettyPrint decls))))
        where
            parse header footer = case H.parseModule (unlines (catMaybes [header, footer])) of
                H.ParseOk modl      -> Right modl
                H.ParseFailed _ msg -> Left ("Invalid Happy header or footer: " ++ msg)