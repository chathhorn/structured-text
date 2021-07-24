module StructuredText.ToSTxt ( toSTxt ) where

import StructuredText.Syntax (STxt)
import qualified StructuredText.Syntax as ST
import StructuredText.DFA (DFA (..))
import Data.Text (Text)

-- | First argument: name.
toSTxt :: Text -> DFA a -> STxt
toSTxt = undefined
