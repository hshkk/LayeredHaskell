{- |
Module      : LHMake.Control.Superimposition
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
module LHMake.Control.Superimposition (
    -- * Superimposition
    Superimposable (..), Identifiable (..),
    -- ** Parsing
    parseMode,
    -- ** Identifiers
    PragmaId (..), DeclId (..), DirectiveId (..),
    -- ** Flattening lists
    flattenPragmas, flattenDecls, flattenTokens,
    -- ** Token precedences
    precs, groupPrecs
    ) where

import Control.Applicative ((<|>))
import Data.Char           (isSpace)
import Data.Functor        (void)
import Data.List           (nub, partition)
import Data.Map            (Map)
import Data.Maybe          (catMaybes, fromMaybe)

import qualified Data.Map as Map

import qualified Happy.Frontend.AbsSyn        as Y
import qualified Happy.Frontend.PrettyGrammar as Y
import qualified Language.Haskell.Exts        as H

import LHMake.Data.Error (LHsMakeError)

infixl 6 -->

-- | A 'Superimposable' type allows merging of two values \(L\) and \(R\),
--   which range over anything from Haskell modules to Happy productions.
--   \(L\) and \(R\) are assumed to have the same 'Identifier', as defined by the 'Identifiable' typeclass.
--
--   By default, \(L\) overrides the contents of \(R\).
--   Specific instances replace this behavior with a merging semantics.
class Superimposable a where
    -- | Superimposes two values.
    --   
    --   Note that superimposition can fail for various reasons, such as when differing module names are encountered.
    (-->) :: a -> a -> LHsMakeError a
    x --> _ = Right x

-- | An 'Identifiable' type @a@ has an 'Identifier', which is a value of some type identifying a value of type @a@.
--   'Identifier' is defined on a case-by-case basis, since a type @a@ might need a different identification mechanism than another type @b@.
--   The 'identify' function then takes a value and produces its 'Identifier'.
--   This allows us to decide whether two values can be merged in a general way.
class Identifiable a where
    type Identifier a
    -- | Identifies a value.
    --   
    --   Note that identification can fail for various reasons, 
    --   most notably when superimposition has not been defined for a 
    --   given language construct out of a lack of necessity.
    identify :: a -> LHsMakeError (Identifier a)

-- | Lists are superimposed element by element. 
--   Elements with the same identifier are superimposed, 
--   and elements that only the right list has are added at the end.
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

-- | 'H.defaultParseMode' but with 'H.fixities' set to 'Nothing'.
--
--   Operator precedence resolution is disabled since superimposition does 
--   not merge expressions (e.g., RHSs of function clauses).
parseMode :: H.ParseMode
parseMode = H.defaultParseMode { H.fixities = Nothing }

-- All Haskell syntax trees here carry the annotation (), since the source
-- positions the parser records would otherwise make equal values compare as different.

instance Superimposable (H.CName ())
instance Superimposable (H.QualConDecl ())
instance Superimposable (H.InstRule ())
instance Superimposable (H.Match ())

instance Identifiable (H.CName ()) where
    type Identifier (H.CName ()) = H.Name ()
    identify (H.VarName _ name) = Right name
    identify (H.ConName _ name) = Right name

-- | A constructor is identified by its name.
instance Identifiable (H.QualConDecl ()) where
    type Identifier (H.QualConDecl ()) = H.Name ()
    identify (H.QualConDecl _ _ _ con) = case con of
        H.ConDecl _ name _        -> Right name
        H.InfixConDecl _ _ name _ -> Right name
        H.RecDecl _ name _        -> Right name

instance Identifiable (H.InstRule ()) where
    type Identifier (H.InstRule ()) = H.InstRule ()
    identify = Right

-- | Splits a function clause into its name and patterns.
--   An infix definition such as @x ++ y = e@ is treated like @(++) x y = e@.
clause :: H.Match l -> (H.Name l, [H.Pat l])
clause (H.Match _ name pats _ _)          = (name, pats)
clause (H.InfixMatch _ pat name pats _ _) = (name, pat : pats)

-- | A function is identified by its patterns.
instance Identifiable (H.Match ()) where
    type Identifier (H.Match ()) = [H.Pat ()]
    identify = Right . snd . clause

-- Pragmas --------------------------------------------------------------------

-- | Identifiers for module pragmas.
data PragmaId
    = Extension (H.Name ())             -- ^ A @LANGUAGE@ pragma.
    | Options   (Maybe H.Tool) [String] -- ^ An @OPTIONS@ pragma, such as @OPTIONS_GHC@.
    | Annotated (H.ModulePragma ())     -- ^ An @ANN@ pragma, identified by its contents.
    deriving Eq

-- | Splits a @LANGUAGE@ pragma holding several extensions into one pragma per extension.
--
--   For instance, @{-# LANGUAGE A, B #-}@ is flattened into @{-# LANGUAGE A #-}@ and @{-# LANGUAGE B #-}@.
--   All other pragmas are left as is.
flattenPragmas :: H.ModulePragma () -> [H.ModulePragma ()]
flattenPragmas (H.LanguagePragma _ names) = [H.LanguagePragma () [name] | name <- names]
flattenPragmas pragma                     = [pragma]

instance Superimposable (H.ModulePragma ())

instance Identifiable (H.ModulePragma ()) where
    type Identifier (H.ModulePragma ()) = PragmaId
    identify (H.LanguagePragma _ [name])  = Right (Extension name)
    identify (H.LanguagePragma _ _)       = Left "Cannot merge a LANGUAGE pragma that holds several extensions"
    identify (H.OptionsPragma _ tool opt) = Right (Options tool (words opt))
    identify pragma@(H.AnnModulePragma _ _) = Right (Annotated pragma)

-- Modules --------------------------------------------------------------------

-- | Splits a declaration holding several names into one declaration per name.
--   Variable bindings (such as @x = e@) are also rewritten as function bindings, since they get parsed as a pattern binding.
--
--   For instance, the Haskell type signature @f g :: A@ is flattened into @f :: A@ and @g :: A@.
--
--   Note that pragmas attached to declarations (such as @WARNING@) are dropped by 'flattenDecls'.
flattenDecls :: H.Decl () -> [H.Decl ()]
flattenDecls (H.TypeSig _ names ty)         = [H.TypeSig () [name] ty | name <- names]
flattenDecls (H.InfixDecl _ assoc prec ops) = [H.InfixDecl () assoc prec [op] | op <- ops]
flattenDecls (H.PatBind _ (H.PVar _ name) rhs wheres)
    = [H.FunBind () [H.Match () name [] rhs wheres]]
flattenDecls (H.InstDecl _ overlap rule decls)
    = [H.InstDecl () overlap rule (fmap (concatMap flattenInstDecl) decls)]
flattenDecls H.InlineSig {}        = []
flattenDecls H.InlineConlikeSig {} = []
flattenDecls H.SpecSig {}          = []
flattenDecls H.SpecInlineSig {}    = []
flattenDecls H.InstSig {}          = []
flattenDecls H.RulePragmaDecl {}   = []
flattenDecls H.DeprPragmaDecl {}   = []
flattenDecls H.WarnPragmaDecl {}   = []
flattenDecls H.AnnPragma {}        = []
flattenDecls H.MinimalPragma {}    = []
flattenDecls decl = [decl]

-- | Flattens the declarations inside of an instance declaration.
flattenInstDecl :: H.InstDecl () -> [H.InstDecl ()]
flattenInstDecl (H.InsDecl _ decl) = map (H.InsDecl ()) (flattenDecls decl)
flattenInstDecl decl               = [decl]

-- | To superimpose two Haskell modules, they must have the same name.
--   Then, their pragmas, exports, and imports are merged.
--   Pragmas and declarations are flattened before they are merged. See 'flattenPragmas' and 'flattenDecls'. 
instance Superimposable (H.Module ()) where
    H.Module _ head1 pragmas1 imps1 decls1 --> H.Module _ head2 pragmas2 imps2 decls2 = do
        hd      <- case (head1, head2) of
            (Just h1, Just h2) -> fmap Just (h1 --> h2)
            (Nothing, Nothing) -> Right Nothing
            _                  -> Left "Cannot merge a module that has a header with one that does not"
        pragmas <- concatMap flattenPragmas pragmas1 --> concatMap flattenPragmas pragmas2
        imps    <- imps1 --> imps2
        decls   <- concatMap flattenDecls decls1 --> concatMap flattenDecls decls2
        return (H.Module () hd pragmas imps decls)
    _ --> _ = Left "Cannot merge modules that are not plain Haskell modules"

-- | A module header holds the module name and its exports.
--   The names must be the same, and then the exports are merged.
instance Superimposable (H.ModuleHead ()) where
    H.ModuleHead _ mod1 warn1 exps1 --> H.ModuleHead _ mod2 _ exps2
        | mod1 == mod2 = do
            exps <- sequenceA (liftA2 (-->) exps1 exps2)
            return (H.ModuleHead () mod1 warn1 exps)
        | otherwise = Left ("Cannot merge differing modules: " ++ H.prettyPrint mod1 ++ " and " ++ H.prettyPrint mod2)

-- Exports --------------------------------------------------------------------

instance Superimposable (H.ExportSpecList ()) where
    H.ExportSpecList _ xs --> H.ExportSpecList _ ys = fmap (H.ExportSpecList ()) (xs --> ys)

instance Superimposable (H.ExportSpec ()) where
    x@(H.EThingWith _ (H.EWildcard {}) _ _) --> _ 
        = Right x
    _ --> y@(H.EThingWith _ (H.EWildcard {}) _ _) 
        = Right y
    H.EThingWith _ wild name xs --> H.EThingWith _ _ _ ys 
        = fmap (H.EThingWith () wild name) (xs --> ys)
    H.EAbs _ _ _ --> y@(H.EThingWith _ _ _ _)     
        = Right y
    x --> _                                       
        = Right x

instance Identifiable (H.ExportSpec ()) where
    -- An export spec is identified by an entity or a whole module.
    type Identifier (H.ExportSpec ()) = Either (H.QName ()) (H.ModuleName ())
    identify (H.EVar _ name)            = Right (Left  name)
    identify (H.EAbs _ _ name)          = Right (Left  name)
    identify (H.EThingWith _ _ name _)  = Right (Left  name)
    identify (H.EModuleContents _ modl) = Right (Right modl)

-- Imports --------------------------------------------------------------------

instance Superimposable (H.ImportDecl ()) where
    x --> y = do
        specs <- sequenceA (liftA2 (-->) (H.importSpecs x) (H.importSpecs y))
        return (x { H.importSpecs = specs })

instance Identifiable (H.ImportDecl ()) where
    type Identifier (H.ImportDecl ()) = (H.ModuleName (), Bool, Maybe (H.ModuleName ()))
    identify imp = Right (H.importModule imp, H.importQualified imp, H.importAs imp)

-- Note that @ImportSpecList _ False xs@ imports only @xs@, 
-- and @ImportSpecList _ True xs@ imports everything except @xs@.
instance Superimposable (H.ImportSpecList ()) where
    H.ImportSpecList _ False xs --> H.ImportSpecList _ False ys = fmap (H.ImportSpecList () False) (xs --> ys)
    H.ImportSpecList _ True  xs --> H.ImportSpecList _ True  ys = do
        xids <- traverse identify xs
        yids <- traverse identify ys
        return (H.ImportSpecList () True [x | (x, xid) <- zip xs xids, xid `elem` yids])
    H.ImportSpecList _ False xs --> H.ImportSpecList _ True  ys = do
        xids <- traverse identify xs
        yids <- traverse identify ys
        return (H.ImportSpecList () True [y | (y, yid) <- zip ys yids, yid `notElem` xids])
    H.ImportSpecList _ True  xs --> H.ImportSpecList _ False ys = do
        xids <- traverse identify xs
        yids <- traverse identify ys
        return (H.ImportSpecList () True [x | (x, xid) <- zip xs xids, xid `notElem` yids])

instance Superimposable (H.ImportSpec ()) where
    x@(H.IThingAll _ _) --> _                      
        = Right x
    _ --> y@(H.IThingAll _ _)                      
        = Right y
    H.IThingWith _ name xs --> H.IThingWith _ _ ys 
        = fmap (H.IThingWith () name) (xs --> ys)
    H.IAbs _ _ _ --> y@(H.IThingWith _ _ _)        
        = Right y
    x --> _                                       
        = Right x

instance Identifiable (H.ImportSpec ()) where
    type Identifier (H.ImportSpec ()) = H.Name ()
    identify (H.IVar _ name)         = Right name
    identify (H.IAbs _ _ name)       = Right name
    identify (H.IThingAll _ name)    = Right name
    identify (H.IThingWith _ name _) = Right name

-- Declarations ---------------------------------------------------------------

-- | Identifiers for Haskell declarations.
--
--   Only these declarations can be merged. Everything else (classes, default
--   declarations, foreign declarations, pattern bindings) is rejected by 'identify'.
data DeclId
    = Value    (H.Name ())  -- ^ Functions and variables.
    | Sig      (H.Name ())  -- ^ Type signatures.
    | Type     (H.Name ())  -- ^ Type synonyms, datatype declarations, and @newtype@s.
    | Fixity   (H.Name ())  -- ^ Fixity declarations.
    | Instance (H.InstHead ())
                            -- ^ Typeclass instance declarations.
    deriving Eq

-- | Finds the name that a declaration head (such as @T a b@) declares.
headName :: H.DeclHead l -> H.Name l
headName (H.DHead _ name)     = name
headName (H.DHInfix _ _ name) = name
headName (H.DHParen _ hd)     = headName hd
headName (H.DHApp _ hd _)     = headName hd

-- | Finds the instance head (the class and its types) of an instance rule.
instHead :: H.InstRule l -> H.InstHead l
instHead (H.IRule _ _ _ hd) = hd
instHead (H.IParen _ rule)  = instHead rule

-- | Only a subset of Haskell declarations have been given superimposition definitions.
--   These are as follows.
--
--       * Type synonyms
--       * Datatype declarations
--       * Newtype declarations
--       * Function bindings
--       * Type signatures
--       * Fixity declarations
--       * Typeclass instance declarations.
--
--   See 'DeclId'.
instance Identifiable (H.Decl ()) where
    type Identifier (H.Decl ()) = DeclId
    identify (H.TypeDecl _ hd _)                  = Right (Type (headName hd))
    identify (H.DataDecl _ _ _ hd _ _)            = Right (Type (headName hd))
    identify (H.FunBind _ (m : _))                = Right (Value (fst (clause m)))
    identify (H.TypeSig _ [name] _)               = Right (Sig name)
    identify (H.InfixDecl _ _ _ [H.VarOp _ name]) = Right (Fixity name)
    identify (H.InfixDecl _ _ _ [H.ConOp _ name]) = Right (Fixity name)
    identify (H.InstDecl _ _ rule _)              = Right (Instance (instHead rule))
    identify decl = Left ("Cannot merge this declaration: "
                          ++ takeWhile (/= '\n') (dropWhile isSpace (H.prettyPrint decl)))

-- | Datatype declarations, functions, and typeclass instance declarations are merged.
--   Type synonyms, @newtype@s, type signatures, and fixity declarations are overridden from the left.
instance Superimposable (H.Decl ()) where
    H.DataDecl _ dn@(H.DataType _) ctx hd cons1 derivs1 --> H.DataDecl _ (H.DataType _) _ _ cons2 derivs2 = do
        cons   <- cons1 --> cons2
        derivs <- derivs1 --> derivs2
        return (H.DataDecl () dn ctx hd cons derivs)
    H.FunBind _ ms1 --> H.FunBind _ ms2
        | length (nub [length (snd (clause m)) | m <- ms1 ++ ms2]) == 1
        = fmap (H.FunBind ()) (ms1 --> ms2)
    H.InstDecl _ overlap rule decls1 --> H.InstDecl _ _ _ decls2 = do
        decls <- fromMaybe [] decls1 --> fromMaybe [] decls2
        return (H.InstDecl () overlap rule (if null decls then Nothing else Just decls))
    x --> _ = Right x

instance Superimposable (H.InstDecl ()) where
    H.InsDecl _ x --> H.InsDecl _ y = fmap (H.InsDecl ()) (x --> y)
    x --> _ = Right x

instance Identifiable (H.InstDecl ()) where
    type Identifier (H.InstDecl ()) = DeclId
    identify (H.InsDecl _ decl) = identify decl
    identify decl = 
        Left ("Cannot merge this instance declaration: " ++ 
              takeWhile (/= '\n') (dropWhile isSpace (H.prettyPrint decl)))

-- | A @deriving@ clause is identified by its strategy (e.g., 'H.DerivStock').
--   Clauses with the same strategy are merged, and the classes they derive are merged by their names.
instance Superimposable (H.Deriving ()) where
    H.Deriving _ strat rules1 --> H.Deriving _ _ rules2 
        = fmap (H.Deriving () strat) (rules1 --> rules2)

instance Identifiable (H.Deriving ()) where
    type Identifier (H.Deriving ()) = Maybe (H.DerivStrategy ())
    identify (H.Deriving _ strat _) = Right strat

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
    | Prec      String  -- ^ A @%left@, @%right@, or @%nonassoc@ directive, one per token.
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

-- | Splits @x a b@ into @x a@ and @x b@, where @x@ ranges over the @%token@, @%left@, @%right@, and @%nonassoc@ directives.
--   All other directives are left as is.
flattenTokens :: [Y.Directive String] -> [Y.Directive String]
flattenTokens = (=<<) flattenToken
    where
        flattenToken (Y.TokenSpec specs)    = map (Y.TokenSpec . return) specs
        flattenToken (Y.TokenLeft toks)     = map (Y.TokenLeft . return) toks
        flattenToken (Y.TokenRight toks)    = map (Y.TokenRight . return) toks
        flattenToken (Y.TokenNonassoc toks) = map (Y.TokenNonassoc . return) toks
        flattenToken dir = return dir

-- | Precedences of tokens, where a higher number binds tighter than a lower one.
--   The (relative) precedences of tokens corresponding to operators borrow directly from Haskell.
--   Operator associativity is defined within the parser files themselves.
--
--   Note that the precedence resolution mechanism here is slightly weaker than precedence climbing. 
--   In Happy files, tokens with the same precedence but different associativity cannot 
--   share a precedence level. Consequently, superimposition must order such tokens 
--   sequentially in precedence lists. As a result, Layered Haskell accepts expressions that 
--   GHC would normally reject, like @x . y !! z@.
-- 
--   ==== __Precedences__
--
--   Tokens where superimposition diverges from GHC are marked with a @!@.
--   There is currently only one such precedence level.
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
--   > '.' '!!'          12 !
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

-- | Groups precedence directives by the (numeric) precedence and associativity of the 
--   involved tokens. If a token with an unknown precedence is encountered, an error is emitted.
--   
--   Note that the numeric precedences come from 'precs', while the associativities come from the parser files themselves.
--   Consequently, the order in which token precedence is specified within the parser files is ignored,
--   since 'groupPrecs' sorts tokens into the expected order anyway.
groupPrecs :: [Y.Directive String] -> LHsMakeError [Y.Directive String]
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

-- | Directives are first flattened (see 'flattenTokens'), then merged as much as possible.
--   Precedence directives are merged with respect to numeric precedences (see 'precs').
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
        modl1  <- parse head1 foot1
        modl2  <- parse head2 foot2
        merged <- modl1 --> modl2
        (shell, decls) <- case merged of
            H.Module _ hd pragmas imps ds -> Right (H.Module () hd pragmas imps [], ds)
            _                             -> Left "Invalid Happy header or footer: not a plain Haskell module"
        syn <- syn1 --> syn2
        return (Y.BookendedAbsSyn (opts1 <|> opts2)
                            -- The header contains the pragmas, the Haskell module declaration and imports.
                            (Just (H.prettyPrint shell))
                            syn
                            -- The footer contains Haskell declarations.
                            (Just (unlines (map H.prettyPrint decls))))
        where
            parse header footer = case H.parseModuleWithMode parseMode (unlines (catMaybes [header, footer])) of
                H.ParseOk modl      -> Right (void modl)
                H.ParseFailed _ msg -> Left ("Invalid Happy header or footer: " ++ msg)