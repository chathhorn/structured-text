{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module StructuredText.Syntax
      ( STxt (..)
      , Global (..)
      , Exp (..)
      , Type (..)
      ) where

import Data.Text (Text)
import Prettyprinter ((<+>), Pretty (..))

data STxt = STxt [Global]
      deriving (Eq, Show)

instance Pretty STxt where
      pretty (STxt gs) = pretty gs

data Global = FunctionBlock Text [Exp]
            | Function Text Type [Exp]
            | Program Text [Exp]
            | TypeDef Text
      deriving (Eq, Show)

instance Pretty Global where
      pretty = \ case
            FunctionBlock f _   -> pretty ("FUNCTION_BLOCK" :: Text) <+> pretty f
            Function f _ _      -> pretty ("FUNCTION" :: Text) <+> pretty f
            Program f _         -> pretty ("PROGRAM" :: Text) <+> pretty f
            TypeDef f           -> pretty ("TYPE" :: Text) <+> pretty f

data Exp = Exp Text
      deriving (Eq, Show)

data Type = TBool
          | TReal | TLReal
          | TInt | TUInt | TSInt | TUSInt | TDInt | TUDInt | TLInt | TULInt
          | TByte | TWord | TDWord | TLWord
          | TTime | TDate | TTimeOfDay | TDateTime
          | TString | TWString
          | TId Text
      deriving (Eq, Show)

