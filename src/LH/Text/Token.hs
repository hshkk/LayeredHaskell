{- |
Module      : LH.Text.Token
Description : Tokens.
Copyright   : (c) Hangil Kim, 2026
                  Kairon Johnson, 2026
                  Leonidas Sallos, 2026
                  Oscar Abelanet, 2026
License     : MIT
Maintainer  : kimhang@oregonstate.edu
Stability   : experimental

This module provides definitions for tokens.
-}
module LH.Text.Token (
    -- * Tokens
    LHsToken (..),
    -- ** Properties
    isKeyword, isOperator, isVirtual
    ) where

import Data.Text (Text)

-- | A token.
data LHsToken
    ---------------------------------------------------------------------------
    -- Identifiers
    = LHsTkVar    !Text    -- ^ A variable name.
    | LHsTkCon    !Text    -- ^ A constructor name.
    ---------------------------------------------------------------------------
    -- Literals
    | LHsTkInt    !Int     -- ^ An integer literal.
    ---------------------------------------------------------------------------
    -- Arrow operator
    | LHsTkArrow           -- ^ An arrow @->@.
    ---------------------------------------------------------------------------
    -- Arithmetic operators
    | LHsTkPlus            -- ^ A plus @+@.
    | LHsTkMinus           -- ^ A hyphen @-@.
    | LHsTkTimes           -- ^ An asterisk @*@.
    ---------------------------------------------------------------------------
    -- Logical operators
    | LHsTkOr              -- ^ A double pipe @||@.
    | LHsTkAnd             -- ^ A double ampersand @&&@.
    ---------------------------------------------------------------------------
    -- Relational operators
    | LHsTkLt              -- ^ A left angle bracket @<@.
    | LHsTkGt              -- ^ A right angle bracket @>@.
    | LHsTkLte             -- ^ A left angle bracket and an equals sign @<=@.
    | LHsTkGte             -- ^ A right angle bracket and an equals sign @<=@.
    ---------------------------------------------------------------------------
    -- Equality
    | LHsTkEqEq            -- ^ A double equals sign @==@.
    | LHsTkNotEq           -- ^ A forward slash followed by an equals sign @/=@.
    ---------------------------------------------------------------------------
    -- Function operators
    | LHsTkDot             -- ^ A period @.@.
    | LHsTkDollar          -- ^ A dollar sign @$@.
    ---------------------------------------------------------------------------
    -- List operators
    | LHsTkColon           -- ^ A colon @:@.
    | LHsTkPlusPlus        -- ^ A double plus @++@.
    | LHsTkExclExcl        -- ^ A double exclamation mark @!!@.
    ---------------------------------------------------------------------------
    -- Symbols
    | LHsTkLam             -- ^ A backslash @\\@, as used in a λ-abstraction.
    | LHsTkEq              -- ^ An equals sign @=@.
    | LHsTkPipe            -- ^ A pipe @|@.
    | LHsTkColonColon      -- ^ A double colon @::@.
    | LHsTkLParen          -- ^ A left parenthesis @(@.
    | LHsTkRParen          -- ^ A right parenthesis @)@.
    | LHsTkLBrack          -- ^ A left bracket @[@.
    | LHsTkRBrack          -- ^ A right bracket @]@.
    | LHsTkComma           -- ^ A comma @,@.
    | LHsTkUnderscore      -- ^ An underscore @_@.
    ---------------------------------------------------------------------------
    -- Keywords
    | LHsTkLet             -- ^ The keyword @let@.
    | LHsTkIn              -- ^ The keyword @in@.
    | LHsTkCase            -- ^ The keyword @case@.
    | LHsTkOf              -- ^ The keyword @of@.
    | LHsTkData            -- ^ The keyword @data@.
    | LHsTkType            -- ^ The keyword @type@.
    | LHsTkIf              -- ^ The keyword @if@.
    | LHsTkThen            -- ^ The keyword @then@.
    | LHsTkElse            -- ^ The keyword @else@.
    ---------------------------------------------------------------------------
    -- Virtual tokens inserted by layout
    | LHsTkVOpen           -- ^ A /virtual/ opening brace.
    | LHsTkVClose          -- ^ A /virtual/ closing brace.
    | LHsTkVSemi           -- ^ A /virtual/ semicolon.
    ---------------------------------------------------------------------------
    -- End-of-file
    | LHsTkEOF             -- ^ The token indicating end-of-file (EOF).
    deriving (Eq, Show)

-- | Is a given token a keyword?
isKeyword :: LHsToken -> Bool
isKeyword LHsTkLet  = True
isKeyword LHsTkIn   = True
isKeyword LHsTkCase = True
isKeyword LHsTkOf   = True
isKeyword LHsTkData = True
isKeyword LHsTkType = True
isKeyword LHsTkIf   = True
isKeyword LHsTkThen = True
isKeyword LHsTkElse = True
isKeyword _         = False

-- | Is a given token an operator?
isOperator :: LHsToken -> Bool
isOperator LHsTkArrow    = True
isOperator LHsTkPlus     = True
isOperator LHsTkMinus    = True
isOperator LHsTkTimes    = True
isOperator LHsTkOr       = True
isOperator LHsTkAnd      = True
isOperator LHsTkLt       = True
isOperator LHsTkGt       = True
isOperator LHsTkLte      = True
isOperator LHsTkGte      = True
isOperator LHsTkEqEq     = True
isOperator LHsTkNotEq    = True
isOperator LHsTkDot      = True
isOperator LHsTkDollar   = True
isOperator LHsTkColon    = True
isOperator LHsTkPlusPlus = True
isOperator LHsTkExclExcl = True
isOperator _             = False

-- | Is a given token virtual? See 'LHsTkVSemi' and 'LHsTkVClose'.
isVirtual :: LHsToken -> Bool
isVirtual LHsTkVSemi  = True
isVirtual LHsTkVClose = True
isVirtual _           = False