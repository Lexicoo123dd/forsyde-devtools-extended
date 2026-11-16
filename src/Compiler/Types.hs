{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Central type and literal definitions for the ForSyDe compiler.
--
-- This module acts as the single source of truth for all data types
-- and literal values that flow through ForSyDe signals, actor tokens,
-- and generated C code.
--
-- By defining types and their target-language properties here, adding
-- a new data type only requires updating this file and the GHC Core
-- type extraction logic.
module Compiler.Types
  ( -- * Core Data Types & Literals
    DataType (..),
    DataLiteral (..),
    typeOfLiteral,

    -- * Target C Code Generation Helpers
    cTypeName,
    cUnionField,
    cFormatSpecifier,
    cUnionMember,
    translateLiteral,

    -- * Formatting & Pretty Printing
    prettyDataType,
    literalFormatSpecifier,
  )
where

import Data.Aeson (ToJSON (..), Value (String), object, (.=))
import qualified Data.Text as Text
import GHC.Generics (Generic)

-- -----------------------------------------------------------------------------
-- Data Types
-- -----------------------------------------------------------------------------

-- | Supported data types flowing through ForSyDe signals and actor tokens.
data DataType
  = TInt
  | TFloat
  | TBool
  | TUnknown
  deriving (Show, Eq, Ord, Generic)

-- -----------------------------------------------------------------------------
-- Literals
-- -----------------------------------------------------------------------------

-- | Concrete literal values corresponding to 'DataType'.
data DataLiteral
  = LInt Int
  | LFloat Float
  | LBool Bool
  deriving (Show, Eq, Generic)

-- | Determine the 'DataType' of any given 'DataLiteral'.
typeOfLiteral :: DataLiteral -> DataType
typeOfLiteral (LInt _) = TInt
typeOfLiteral (LFloat _) = TFloat
typeOfLiteral (LBool _) = TBool

-- -----------------------------------------------------------------------------
-- C Code Generation Mappings
-- -----------------------------------------------------------------------------

-- | The C primitive type name corresponding to each 'DataType'.
cTypeName :: DataType -> String
cTypeName TInt = "int"
cTypeName TFloat = "float"
cTypeName TBool = "int"
cTypeName TUnknown = error "Compiler.Types.cTypeName: cannot determine C type for TUnknown"

-- | The field name inside the generated C union (e.g. @token.Int@, @token.Float@).
cUnionField :: DataType -> String
cUnionField TInt = "Int"
cUnionField TFloat = "Float"
cUnionField TBool = "Bool"
cUnionField TUnknown = error "Compiler.Types.cUnionField: cannot access union field for TUnknown"

-- | The @printf@ / @scanf@ format specifier for each 'DataType'.
cFormatSpecifier :: DataType -> String
cFormatSpecifier TInt = "%d"
cFormatSpecifier TFloat = "%f"
cFormatSpecifier TBool = "%d"
cFormatSpecifier TUnknown = error "Compiler.Types.cFormatSpecifier: cannot format TUnknown"

-- | The full struct/union member declaration line for C (e.g. @"  int Int;"@).
cUnionMember :: DataType -> String
cUnionMember t = "  " ++ cTypeName t ++ " " ++ cUnionField t ++ ";"

-- | Format specifier directly from a 'DataLiteral'.
literalFormatSpecifier :: DataLiteral -> String
literalFormatSpecifier = cFormatSpecifier . typeOfLiteral

-- | Translate a 'DataLiteral' directly into its C literal code representation.
translateLiteral :: DataLiteral -> String
translateLiteral (LInt n) = show n
translateLiteral (LFloat f) = show f
translateLiteral (LBool b) = if b then "1" else "0"

-- -----------------------------------------------------------------------------
-- Pretty Printing
-- -----------------------------------------------------------------------------

-- | Pretty printer for 'DataType' used in IR dumps and debug messages.
prettyDataType :: DataType -> String
prettyDataType TInt = "IntType"
prettyDataType TFloat = "FloatType"
prettyDataType TBool = "BoolType"
prettyDataType TUnknown = "UnknownType"

-- -----------------------------------------------------------------------------
-- JSON Serialization
-- -----------------------------------------------------------------------------

instance ToJSON DataType where
  toJSON t = String (Text.pack (show t))

instance ToJSON DataLiteral where
  toJSON (LInt n) =
    object ["type" .= ("Int" :: Text.Text), "value" .= toJSON n]
  toJSON (LFloat f) =
    object ["type" .= ("Float" :: Text.Text), "value" .= toJSON f]
  toJSON (LBool b) =
    object ["type" .= ("Bool" :: Text.Text), "value" .= toJSON b]
