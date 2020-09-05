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
      deriving Show

instance Pretty STxt where
      pretty (STxt gs) = pretty gs

data Global = FunctionBlock Text [Exp]
            | Function Text Type [Exp]
            | Program Text [Exp]
            | TypeDef Text
      deriving Show

instance Pretty Global where
      pretty = \ case
            FunctionBlock f _   -> pretty ("FUNCTION_BLOCK" :: Text) <+> pretty f
            Function f _ _      -> pretty ("FUNCTION" :: Text) <+> pretty f
            Program f _         -> pretty ("PROGRAM" :: Text) <+> pretty f
            TypeDef f           -> pretty ("TYPE" :: Text) <+> pretty f

data Exp = Exp Text
      deriving Show

data Type = Type Text
          | NoType
      deriving Show
