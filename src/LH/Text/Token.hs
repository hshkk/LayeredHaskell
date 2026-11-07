{- |
Module      : LH.Text.Token
Description : Tokens and source positions.
Copyright   : (c) Hangil Kim, 2026
                  Kairon Johnson, 2026
                  Leonidas Sallos, 2026
                  Oscar Abelanet, 2026
License     : MIT
Maintainer  : kimhang@oregonstate.edu
Stability   : experimental

This module provides definitions source positions and tokens.
-}
module LH.Text.Token (
    -- * Source positions
    LHsPos (..), 
    -- * Tokens
    LHsToken (..), 
    -- ** Lexemes
    LHsLexeme (..)
    ) where

import Data.Text (Text)

-- | A position in the source.
data LHsPos = LHsPos 
    { posLine :: !Int      -- ^ A line.
    , posCol  :: !Int      -- ^ A column.
    }
    deriving (Eq, Ord, Show)

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
    -- Operators
    | LHsTkArrow           -- ^ An arrow @->@.
    -- Arithmetic operators
    | LHsTkPlus            -- ^ A plus @+@.
    | LHsTkMinus           -- ^ A hyphen @-@.
    | LHsTkTimes           -- ^ An asterisk @*@.
    -- Logical operators
    | LHsTkOr              -- ^ A double pipe @||@.
    | LHsTkAnd             -- ^ A double ampersand @&&@.
    -- Relational operators
    | LHsTkLt              -- ^ A left angle bracket @<@.
    | LHsTkGt              -- ^ A right angle bracket @>@.
    | LHsTkLte             -- ^ A left angle bracket and an equals sign @<=@.
    | LHsTkGte             -- ^ A right angle bracket and an equals sign @<=@.
    -- Equality
    | LHsTkEqEq            -- ^ A double equals sign @==@.
    | LHsTkNotEq           -- ^ A forward slash followed by an equals sign @/=@.
    -- List operators
    | LHsTkColon           -- ^ A colon @:@.
    | LHsTkPlusPlus        -- ^ A double plus @++@.
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

-- | A token paired with its position in the source.
data LHsLexeme = LHsLexeme !LHsPos !LHsToken
    deriving (Eq, Show)