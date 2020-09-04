{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module StructuredText.Syntax
      ( STxt (..)
      ) where

import Data.Text (Text)
import Text.PrettyPrint ((<+>), Pretty (..))

data STxt = FunBlock Text

instance Pretty STxt where
      pretty = \ case
            FunBlock f -> "funblock" <+> f
