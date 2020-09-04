{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module StructuredText.Syntax
      ( STxt (..)
      ) where

import Data.Text (Text)
import Prettyprinter ((<+>), Pretty (..))

data STxt = FunBlock Text

instance Pretty STxt where
      pretty = \ case
            FunBlock f -> pretty ("funblock" :: Text) <+> pretty f
