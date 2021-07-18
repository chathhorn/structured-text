module StructuredText.ToSTxt ( toSTxt, crystalize ) where

import qualified StructuredText.Syntax as ST

import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import StructuredText.LTL (AtomicProp (..), NormLTL (..), negNormLTL, atoms, BasicTerm (..))
import StructuredText.ABA (ABA (..), deltaP)
import StructuredText.ToABA (LABA)
import StructuredText.Boolean (B (..))

type SatSet s = Set (Set s)

data ABA' a = ABA'
      { currABA'  :: SatSet a -- current/initial state
      , finalABA' :: Set a    -- final states (must be a subset of some set in the current state)
      , deltaABA' :: Map (SatSet a, Set a) (SatSet a)
      }

crystalize :: LABA a -> ABA' a
crystalize = undefined
-- crystalize aba = ABA'
--       { currABA'  = satSet (currABA aba)
--       --, finalABA' = S.map satSet (finalABA aba)
--       , deltaABA' = mapify $ deltaP (deltaABA aba)
--       }

satSet :: B (NormLTL a) -> SatSet a
satSet = undefined

sat :: NormLTL a -> Set a
sat = undefined

mapify :: (B (NormLTL a) -> Set a -> B (NormLTL a))
       -> Map (SatSet a, Set a) (SatSet a)
mapify = undefined


toSTxt = undefined
