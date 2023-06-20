module StructuredText.Buchi
      ( NBA (..)
      , runNBA
      , acceptNBA, acceptNBA'
      ) where

import Prelude hiding (concat)
import qualified Data.Set as S
import Data.Set (Set)
import Data.List (last)

data NBA state alph = NBA
      { statesNBA :: Set state
      , initsNBA  :: Set state
      , finalNBA  :: Set state
      , deltaNBA  :: state -> alph -> Set state
      }

unionMapSet :: (Ord a, Ord b) => (a -> Set b) -> Set a -> Set b
unionMapSet f s = S.unions (S.map f s)

-- | Executes an NFA. Takes a stream of inputs and produces a stream of state-sets.
runNBA :: (Eq a, Ord s) => NBA s a -> [a] -> [Set s]
runNBA aut word = case word of
     (a : as) -> initsNBA aut : runNBA aut {initsNBA = unionMapSet (flip (deltaNBA aut) a) (initsNBA aut)} as
     []       -> [initsNBA aut]

-- | Assumes that run has already been generated.
acceptNBA :: (Eq a, Ord s) => NBA s a -> [a] -> Bool
acceptNBA aut word = not (S.null (Data.List.last (runNBA aut word) `S.intersection` finalNBA aut))

-- | Does not call on runNBA
acceptNBA' :: (Eq a, Ord s) => NBA s a -> [a] -> Bool
acceptNBA' aut word = case word of
     (a : as) -> acceptNBA' aut{initsNBA = unionMapSet (flip (deltaNBA aut) a) (initsNBA aut)} as
     []       -> not (S.null (initsNBA aut `S.intersection` finalNBA aut))


