module StructuredText.ABA
      ( ABA (..)
      , B (..)
      , run
      ) where

import Prelude hiding (concat)
import Data.Set (Set, fromList)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Map.Strict as M

data B s = BTerm s
         | BAnd (B s) (B s)
         | BOr (B s) (B s)

data ABA s a = ABA
      { states :: Set s
      , start   :: s
      , final  :: Set s
      , delta  :: s -> a -> B s
      }

run :: ABA s a -> [a] -> Bool
run = undefined
