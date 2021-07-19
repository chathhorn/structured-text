module StructuredText.ToSTxt ( toSTxt ) where

import qualified StructuredText.Syntax as ST

import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import StructuredText.LTL (AtomicProp (..), NormLTL (..), negNormLTL, atoms, BasicTerm (..))
import StructuredText.ABA (ABA (..), deltaP)
import StructuredText.ToABA (LABA)
import StructuredText.Boolean (B (..))

toSTxt = undefined
