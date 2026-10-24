{-# LANGUAGE FlexibleContexts, TypeFamilies, UndecidableInstances #-}
module LH.Control.Superimposable where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Prelude hiding (mod, rem)

import qualified Happy.Frontend.AbsSyn as Happy
import qualified Language.Haskell.Syntax as Haskell

infixr 6 -->

class Superimposable a where
    (-->) :: a -> a -> a

instance Superimposable Haskell.HsModule where
    Haskell.HsModule src mod1 exps1 imps1 decls1 --> Haskell.HsModule _ mod2 exps2 imps2 decls2 
        | mod1 == mod2 = Haskell.HsModule src mod1 (liftA2 (-->) exps1 exps2) (imps1 --> imps2) (decls1 --> decls2)
        | otherwise    = error "Cannot merge differing modules"

instance Superimposable Haskell.HsExportSpec where
    h1 --> h2 = undefined

instance Superimposable Haskell.HsImportSpec where
    h1 --> h2 = undefined

instance Superimposable Haskell.HsImportDecl where
    h1 --> h2 = undefined

instance Superimposable Haskell.HsDecl where
    h1 --> h2 = undefined

instance Superimposable Haskell.HsConDecl where
    h1 --> h2 = undefined

-- instance Superimposable Happy.BookendedAbsSyn where
--     h1 --> h2 = undefined
    
class Identifiable a where
    type Identifier a
    identify :: a -> Identifier a

data HsExportSpecIdentifier
    = HsExpNameI   Haskell.HsQName -- a variable, type, or class
    | HsExpModuleI Haskell.Module
    deriving (Eq, Ord)

instance Identifiable Haskell.HsExportSpec where
    type Identifier Haskell.HsExportSpec = HsExportSpecIdentifier
    identify (Haskell.HsEVar       name)     = HsExpNameI name
    identify (Haskell.HsEAbs       name)     = HsExpNameI name
    identify (Haskell.HsEThingAll  name)     = HsExpNameI name
    identify (Haskell.HsEThingWith name _)   = HsExpNameI name
    identify (Haskell.HsEModuleContents mod) = HsExpModuleI mod

instance Identifiable Haskell.HsImportDecl where
    type Identifier Haskell.HsImportDecl = (Haskell.Module, Bool, Maybe Haskell.Module)
    identify imp = (Haskell.importModule imp, Haskell.importQualified imp, Haskell.importAs imp)

instance Identifiable Haskell.HsImportSpec where
    type Identifier Haskell.HsImportSpec = Haskell.HsName
    identify (Haskell.HsIVar       name)   = name
    identify (Haskell.HsIAbs       name)   = name
    identify (Haskell.HsIThingAll  name)   = name
    identify (Haskell.HsIThingWith name _) = name

data HsDeclIdentifier
    = HsValI Haskell.HsName         -- functions, variables
    | HsPatI [Haskell.HsName]       -- patterns
    | HsSigI Haskell.HsName         -- type signatures
    | HsTyI  Haskell.HsName         -- type synonyms, data, newtype, typeclasses
    | HsInsI Haskell.HsQName String -- typeclass name + instance types
    | HsIfxI Haskell.HsName         -- fixity declarations
    | HsExpI Haskell.HsName         -- foreign exports
    | HsDefI                        -- default declaration
    deriving (Eq, Ord)

instance Identifiable Haskell.HsDecl where
    type Identifier Haskell.HsDecl = HsDeclIdentifier
    identify (Haskell.HsTypeDecl _ name _ _)          = HsTyI name
    identify (Haskell.HsDataDecl _ _ name _ _ _)      = HsTyI name
    identify (Haskell.HsNewTypeDecl _ _ name _ _ _)   = HsTyI name
    identify (Haskell.HsClassDecl _ _ name _ _)       = HsTyI name
    identify (Haskell.HsInstDecl _ _ cls tys _)       = HsInsI cls (show tys)
    identify (Haskell.HsDefaultDecl _ _)              = HsDefI
    identify (Haskell.HsForeignImport _ _ _ _ name _) = HsValI name
    identify (Haskell.HsForeignExport _ _ _ name _)   = HsExpI name

    identify (Haskell.HsFunBind ((Haskell.HsMatch _ name _ _ _):_)) = HsValI name
    identify (Haskell.HsFunBind []) = error "Cannot identify function with no clauses"

    identify (Haskell.HsPatBind _ (Haskell.HsPVar name) _ _) = HsValI name
    identify (Haskell.HsPatBind _ pat' _ _) = HsPatI (bound pat')
        where
            bound :: Haskell.HsPat -> [Haskell.HsName]
            bound (Haskell.HsPVar name)       = [name]
            bound (Haskell.HsPAsPat name pat) = name : bound pat
            bound (Haskell.HsPNeg pat)        = bound pat
            bound (Haskell.HsPParen pat)      = bound pat
            bound (Haskell.HsPIrrPat pat)     = bound pat
            bound (Haskell.HsPInfixApp pat1 _ pat2) = bound pat1 ++ bound pat2
            bound (Haskell.HsPApp _ pats)     = concatMap bound pats
            bound (Haskell.HsPTuple pats)     = concatMap bound pats
            bound (Haskell.HsPList pats)      = concatMap bound pats
            bound (Haskell.HsPRec _ fields)   = concat [ bound pat | Haskell.HsPFieldPat _ pat <- fields ]
            bound _ = []

    identify (Haskell.HsTypeSig _ [name] _) = HsSigI name
    identify (Haskell.HsTypeSig _ _ _)      = error "Cannot identify multiple names in type signature"
    identify (Haskell.HsInfixDecl _ _ _ [Haskell.HsVarOp name]) = HsIfxI name
    identify (Haskell.HsInfixDecl _ _ _ [Haskell.HsConOp name]) = HsIfxI name
    identify (Haskell.HsInfixDecl _ _ _ _)  = error "Cannot identify multiple names in fixity declaration"

instance (Identifiable a, Ord (Identifier a), Superimposable a) => Superimposable [a] where
    lefts --> rights = sup ++ rem
        where
            sup = [maybe l (l -->) (lookup (identify l) [(identify r, r) | r <- rights]) | l <- lefts]
            rem = [r | r <- rights, notElem (identify r) (map identify lefts)]