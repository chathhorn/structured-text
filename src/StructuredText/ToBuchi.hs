module StructuredText.ToBuchi (toBuchi) where

import StructuredText.Automata (NFA, nfa)
import qualified StructuredText.Automata as A
import StructuredText.LTL (NormLTL (..))

import Data.Set (fromList)

-- TermN    a
-- AndN     (NormLTL a) (NormLTL a)
-- OrN      (NormLTL a) (NormLTL a)
-- UntilN   (NormLTL a) (NormLTL a)
-- ReleaseN (NormLTL a) (NormLTL a)
-- NextN    (NormLTL a)

toBuchi :: (a -> w -> Bool) -> NormLTL a -> NFA w
toBuchi trueAt = \ case
      TermN a      -> nfa [0] [2] (f a)
            where f a w 0 | a `trueAt` w = [2]
                  f _ _ 2                = [2]
                  f _ _ _                = [1]
      AndN a b     -> A.intersection (toBuchi trueAt a) (toBuchi trueAt b)
      OrN a b      -> A.union (toBuchi trueAt a) (toBuchi trueAt b)
      UntilN a b   -> undefined
      ReleaseN a b -> undefined
      NextN a      -> A.concat (nfa [0] [1] f) (toBuchi trueAt a)
            where f _ _ = [1]
