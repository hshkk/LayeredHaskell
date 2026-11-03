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
module LH.Control.Superimposition where

import Control.Applicative ((<|>))
import Data.Char           (isSpace)
import Data.List           (find, nub, partition)
import Data.Maybe          (catMaybes)

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
    (-->) :: a -> a -> a
    x --> _ = x

-- | An 'Identifiable' type @a@ has an 'Identifier', which is a value of some type identifying a value of type @a@.
--   'Identifier' is defined on a case-by-case basis, since a type @a@ might need a different identification mechanism than another type @b@.
--   The 'identify' function then takes a value and produces its 'Identifier'.
--   This allows us to decide whether two values can be merged in a general way.
class Identifiable a where
    type Identifier a
    -- | Identifies a value.
    identify :: a -> Identifier a

-- | Lists are superimposed element by element. 
--   Elements with the same identifier are superimposed, and elements that only the right list has are added at the end.
instance (Identifiable a, Eq (Identifier a), Superimposable a) => Superimposable [a] where
    xs --> ys = foldr seq (merged ++ added) (map identify (xs ++ ys))
        where
            merged = [maybe x (x -->) (find ((== identify x) . identify) ys) | x <- xs]
            added  = [y | y <- ys, identify y `notElem` map identify xs]

-------------------------------------------------------------------------------
-- Haskell
-------------------------------------------------------------------------------

instance Superimposable H.HsQName
instance Superimposable H.HsCName
instance Superimposable H.HsConDecl
instance Superimposable H.HsMatch

instance Identifiable H.HsQName where
    type Identifier H.HsQName = H.HsQName
    identify = id

instance Identifiable H.HsCName where
    type Identifier H.HsCName = H.HsName
    identify (H.HsVarName name) = name
    identify (H.HsConName name) = name

instance Identifiable H.HsConDecl where
    type Identifier H.HsConDecl = H.HsName
    identify (H.HsConDecl _ name _) = name
    identify (H.HsRecDecl _ name _) = name

-- | A function is identified by its patterns.
instance Identifiable H.HsMatch where
    type Identifier H.HsMatch = [H.HsPat]
    identify (H.HsMatch _ _ pats _ _) = pats

-- Modules --------------------------------------------------------------------

-- | To superimpose two Haskell modules, they must have the same name.
instance Superimposable H.HsModule where
    H.HsModule loc mod1 exps1 imps1 decls1 --> H.HsModule _ mod2 exps2 imps2 decls2
        | mod1 == mod2
            = H.HsModule loc mod1
                        (liftA2 (-->) exps1 exps2)
                        (imps1 --> imps2)
                        (concatMap flattenDecls decls1 --> concatMap flattenDecls decls2)
        | otherwise = error ("Cannot merge differing modules: " ++ H.prettyPrint mod1 ++ " and " ++ H.prettyPrint mod2)

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
    x@(H.HsEThingAll _) --> _                      = x
    _ --> y@(H.HsEThingAll _)                      = y
    H.HsEThingWith name xs --> H.HsEThingWith _ ys = H.HsEThingWith name (xs --> ys)
    H.HsEAbs _ --> y@(H.HsEThingWith _ _)          = y
    x --> _                                        = x

instance Identifiable H.HsExportSpec where
    type Identifier H.HsExportSpec = Either H.HsQName H.Module  -- an entity, or a whole module
    identify (H.HsEVar name)         = Left name
    identify (H.HsEAbs name)         = Left name
    identify (H.HsEThingAll name)    = Left name
    identify (H.HsEThingWith name _) = Left name
    identify (H.HsEModuleContents m) = Right m

-- Imports --------------------------------------------------------------------

instance Superimposable H.HsImportDecl where
    x --> y = x { H.importSpecs = liftA2 (-->) (H.importSpecs x) (H.importSpecs y) }

instance Identifiable H.HsImportDecl where
    type Identifier H.HsImportDecl = (H.Module, Bool, Maybe H.Module)
    identify imp = (H.importModule imp, H.importQualified imp, H.importAs imp)

-- | A list of module imports.
--   @(False, xs)@ imports only @xs@, and @(True, xs)@ imports everything except @xs@.
type ImportList = (Bool, [H.HsImportSpec])

instance Superimposable ImportList where
    (False, xs) --> (False, ys) = (False, xs --> ys)
    (True,  xs) --> (True,  ys) = (True,  [x | x <- xs, identify x `elem`    map identify ys])
    (False, xs) --> (True,  ys) = (True,  [y | y <- ys, identify y `notElem` map identify xs])
    (True,  xs) --> (False, ys) = (True,  [x | x <- xs, identify x `notElem` map identify ys])

instance Superimposable H.HsImportSpec where
    x@(H.HsIThingAll _) --> _                      = x
    _ --> y@(H.HsIThingAll _)                      = y
    H.HsIThingWith name xs --> H.HsIThingWith _ ys = H.HsIThingWith name (xs --> ys)
    H.HsIAbs _ --> y@(H.HsIThingWith _ _)          = y
    x --> _                                        = x

instance Identifiable H.HsImportSpec where
    type Identifier H.HsImportSpec = H.HsName
    identify (H.HsIVar name)         = name
    identify (H.HsIAbs name)         = name
    identify (H.HsIThingAll name)    = name
    identify (H.HsIThingWith name _) = name

-- Declarations ---------------------------------------------------------------

-- | Identifiers for Haskell declarations.
--
--   Only these declarations can be merged. Everything else (classes, instances, default
--   declarations, foreign declarations, pattern bindings) is rejected by 'identify'.
--   We could consider writing definitions for these too, but we don't have a use for them right now.
data DeclId
    = ValueI  H.HsName  -- ^ Functions and variables.
    | SigI    H.HsName  -- ^ Type signatures.
    | TypeI   H.HsName  -- ^ Type synonyms, datatype declarations, and @newtype@s.
    | FixityI H.HsName  -- ^ Fixity declarations.
    deriving Eq

instance Identifiable H.HsDecl where
    type Identifier H.HsDecl = DeclId
    identify (H.HsTypeDecl _ name _ _)                  = TypeI name
    identify (H.HsDataDecl _ _ name _ _ _)              = TypeI name
    identify (H.HsNewTypeDecl _ _ name _ _ _)           = TypeI name
    identify (H.HsFunBind (H.HsMatch _ name _ _ _ : _)) = ValueI name
    identify (H.HsTypeSig _ [name] _)                   = SigI name
    identify (H.HsInfixDecl _ _ _ [H.HsVarOp name])     = FixityI name
    identify (H.HsInfixDecl _ _ _ [H.HsConOp name])     = FixityI name
    identify decl = error ("Cannot merge this declaration: "
                           ++ takeWhile (/= '\n') (dropWhile isSpace (H.prettyPrint decl)))

-- | Datatype declarations and functions are merged.
--   Type synonyms, @newtype@s, type signatures, and fixity declarations are overridden from the left.
instance Superimposable H.HsDecl where
    H.HsDataDecl loc ctx name vars cons1 derivs1 --> H.HsDataDecl _ _ _ _ cons2 derivs2
        = H.HsDataDecl loc ctx name vars (cons1 --> cons2) (derivs1 --> derivs2)
    H.HsFunBind ms1 --> H.HsFunBind ms2
        | length (nub [length pats | H.HsMatch _ _ pats _ _ <- ms1 ++ ms2]) == 1
        = H.HsFunBind (ms1 --> ms2)
    x --> _ = x

-------------------------------------------------------------------------------
-- Happy
-------------------------------------------------------------------------------

instance Superimposable (Y.Prod String)

-- | A production is identified by its RHS.
instance Identifiable (Y.Prod String) where
    type Identifier (Y.Prod String) = [String]
    identify (Y.Prod terms _ _ _) = map (Y.render . Y.ppTerm) terms

instance Superimposable (Y.Rule String) where
    Y.Rule name params1 prods1 ty1 --> Y.Rule _ params2 prods2 ty2
        | params1 == params2 = Y.Rule name params1 (prods1 --> prods2) (ty1 <|> ty2)
    rule --> _ = rule

instance Identifiable (Y.Rule String) where
    type Identifier (Y.Rule String) = String
    identify (Y.Rule name _ _ _) = name

-- | @%token@ directives are merged and @%name@ directives are overridden from the left.
--   Other differing directives are simply rejected.
instance Superimposable (Y.Directive String) where
    x@(Y.TokenSpec _)  --> _ = x
    x@(Y.TokenName {}) --> _ = x
    x --> y
        | x == y    = x
        | otherwise = error ("Cannot merge differing directives: " ++ show x ++ " and " ++ show y)

-- | Identifiers for Happy directives.
data DirectiveId
    = TokenI     String  -- ^ A @%token@ directive, one per token.
    | NameI      String  -- ^ The @%name@ directive.
    | AttributeI String  -- ^ The @%attribute@ directive.
    | OtherI     String  -- ^ Any other directive.
    deriving Eq

instance Identifiable (Y.Directive String) where
    type Identifier (Y.Directive String) = DirectiveId
    identify (Y.TokenSpec [(name, _)]) = TokenI name
    identify (Y.TokenName _ _ True)    = error "Cannot merge %partial directives"
    identify (Y.TokenName name _ _)    = NameI name
    identify (Y.TokenAttribute name _) = AttributeI name
    identify dir                       = OtherI (takeWhile (/= ' ') (show dir))

-- | Splits @%token a b@ into @%token a@ and @%token b@, but leaves all other directives alone.
flattenTokens :: [Y.Directive String] -> [Y.Directive String]
flattenTokens = (=<<) flattenToken
    where
        flattenToken (Y.TokenSpec specs) = map (Y.TokenSpec . return) specs
        flattenToken dir = return dir

instance Superimposable (Y.AbsSyn String) where
    Y.AbsSyn dirs1 rules1 --> Y.AbsSyn dirs2 rules2
        = Y.AbsSyn (lower ++ (flattenTokens dirs1 --> flattenTokens others2)) (rules1 --> rules2)
        where
            (prios2, others2) = partition (`elem` Y.getPrios dirs2) dirs2
            prios1 = concatMap Y.getPrioNames (Y.getPrios dirs1)
            lower  = 
                [ assoc toks
                | prio <- prios2
                , let toks  = filter (`notElem` prios1) (Y.getPrioNames prio)
                      assoc = case prio of Y.TokenLeft  _ -> Y.TokenLeft
                                           Y.TokenRight _ -> Y.TokenRight
                                           _              -> Y.TokenNonassoc
                , not (null toks) ]

-- | Happy grammars are superimposed by first superimposing the header and footer as a Haskell module.
--   Then, each rule is superimposed.
instance Superimposable Y.BookendedAbsSyn where
    Y.BookendedAbsSyn opts1 head1 syn1 foot1 --> Y.BookendedAbsSyn opts2 head2 syn2 foot2
        = Y.BookendedAbsSyn (opts1 <|> opts2)
                            -- The header contains the Haskell module declaration and imports.
                            (Just (H.prettyPrint (H.HsModule loc name exps imps [])))
                            (syn1 --> syn2)
                            -- The footer contains Haskell declarations.
                            (Just (unlines (map H.prettyPrint decls)))
        where
            H.HsModule loc name exps imps decls = parse head1 foot1 --> parse head2 foot2

            parse header footer = case H.parseModule (unlines (catMaybes [header, footer])) of
                H.ParseOk modl      -> modl
                H.ParseFailed _ msg -> error ("Invalid Happy header or footer: " ++ msg)