{- |
Module      : LH.Text.Parser.AST
Description : Abstract Syntax Tree Data Types.
Copyright   : (c) Hangil Kim, 2026
                  Kairon Johnson, 2026
                  Leonidas Sallos, 2026
                  Oscar Abelanet, 2026
License     : MIT
Maintainer  : kimhang@oregonstate.edu
Stability   : experimental

This module provides the AST data type definitions.
-}
module LH.Text.Parser.AST (
    -- * Program AST Root
    Program (..),

    -- * Declaration Nodes
    Decl (..),

    -- * Expression Nodes
    Expr (..),

    -- * Atomic Notes
    Atom (..)
    ) where

import Data.Text (Text)

-- | A program (a list of declarations).
newtype Program = Program [Decl]
    deriving (Eq, Show)

-- | A declaration.
data Decl
    = ExprDecl Expr -- ^ An expression declaration /TEMP/.
    deriving (Eq, Show)

-- | An expression.
data Expr
    = AtomExpr Atom      -- ^ An atomic expression.
    | CompExpr Expr Atom -- ^ A compound expression.
    | LamExpr Atom Expr  -- ^ A lambda expression.
    deriving (Eq, Show)

-- | An atomic node.
data Atom
    = AVar Text   -- ^ A variable atomic.
    | ACons Text  -- ^ A constructor atomic.
    | AScope Expr -- ^ An expression in an isolated scope.
    deriving (Eq, Show)
