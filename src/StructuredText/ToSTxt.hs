module StructuredText.ToSTxt ( toSTxt ) where

import qualified StructuredText.Syntax as ST
import StructuredText.DFA (DFA (..))

toSTxt :: DFA a -> ST.Global
toSTxt = undefined
