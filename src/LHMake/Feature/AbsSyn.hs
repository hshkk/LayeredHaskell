{-# OPTIONS_HADDOCK hide #-}
module LH.Feature.AbsSyn
       (LHsExpr(..), LHsVal(..), LHsBin(..), LHsType(..), LHsPat(..),
        LHsDecl(..), Id, LHsEnv)
       where
import LH.Data.SrcLoc (L)
import Data.Sequence (Seq)
import Data.Map (Map)
import Data.Text (Text)
 
data LHsExpr = LHsInt Int
             | LHsBin LHsBin (L LHsExpr) (L LHsExpr)
             | LHsPre LHsBin
             | LHsNeg (L LHsExpr)
             | LHsIf (L LHsExpr) (L LHsExpr) (L LHsExpr)
             | LHsVar Id
             | LHsCon Id
             | LHsApp (L LHsExpr) (L LHsExpr)
             | LHsLet (Seq (L LHsDecl)) (L LHsExpr)
             deriving (Eq, Show)
 
data LHsBin = LHsAdd
            | LHsSub
            | LHsMul
            | LHsOr
            | LHsAnd
            | LHsLt
            | LHsGt
            | LHsLte
            | LHsGte
            | LHsEq
            | LHsNeq
            deriving (Eq, Show)
 
data LHsVal = LHsVInt Int
            | LHsVCon Id (Seq LHsVal)
            | LHsClos LHsEnv (L LHsExpr)
            deriving (Eq, Show)
 
data LHsType = LHsTyCon Id (Seq (L LHsType))
             | LHsTyVar Id
             deriving (Eq, Show)
 
data LHsPat = LHsPInt Int
            | LHsPCon Id (Seq (L LHsPat))
            | LHsPVar Id
            deriving (Eq, Show)
 
data LHsDecl = LHsData Id (Seq Id) (Seq (Id, Seq (L LHsType)))
             | LHsType Id (Seq Id) (L LHsType)
             | LHsEqn Id (Seq (L LHsPat)) (L LHsExpr)
             | LHsSig Id (L LHsType)
             deriving (Eq, Show)
 
type Id = L Text
 
type LHsEnv = Map Id LHsVal