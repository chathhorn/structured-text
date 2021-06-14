module StructuredText.Buchi
      ( NBA (..)
      , acceptNBA 
      ) where

import Prelude hiding (concat)
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.List (last)
import Text.Show.Functions
--import StructuredText.ABA
import StructuredText.LTL (atoms)

data NBA state alph = NBA
      { statesNBA :: Set state
      , initsNBA  :: Set state
      , finalNBA  :: Set state
      , deltaNBA  :: state -> alph -> Set state
      }

unionMapSet :: (Ord a, Ord b) => (a -> Set b) -> Set a -> Set b
unionMapSet f s = S.unions (S.map f s)

-- Executes an NFA. Takes a stream of inputs and produces a stream of state-sets.
runNBA :: (Eq a, Ord s) => NBA s a -> [a] -> [Set s]
runNBA aut word = case word of
     (a : as) -> initsNBA aut : runNBA aut {initsNBA = unionMapSet ((flip (deltaNBA aut)) a) (initsNBA aut)} as
     []       -> [initsNBA aut]

acceptNBA :: (Eq a, Ord s) => NBA s a -> [a] -> Bool
acceptNBA aut word = not (S.null ((Data.List.last(runNBA aut word)) `S.intersection` (finalNBA aut)))

--examples for testing

{-
tran :: Map (Char, Integer) (Set Char)
tran = M.fromList[(("state 0", 0), S.singleton "state 0" ), (("state 0", 1), S.singleton "state 1"), (("state 0", 2), S.singleton "state 0")]

tran_func :: Char -> Integer -> Set Char
tran_func s a = M.findWithDefault S.empty (s, a) tran

cantoraba :: NBA Char Integer
cantoraba = NBA {statesNBA = S.fromList ["state 0", "state 1"] , initsNBA = S.fromList ["state 0"] , finalNBA = S.fromList["state 0"] , deltaNBA = tran_func}

-}
