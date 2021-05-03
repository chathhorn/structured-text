module StructuredText.ToBuchi (toBuchi) where

import StructuredText.Automata (NFA, nfa)
import qualified StructuredText.Automata as A
import StructuredText.LTL (NormLTL (..))

-- TermN    a
-- AndN     (NormLTL a) (NormLTL a)
-- OrN      (NormLTL a) (NormLTL a)
-- UntilN   (NormLTL a) (NormLTL a)
-- ReleaseN (NormLTL a) (NormLTL a)
-- NextN    (NormLTL a)

toBuchi :: (a -> w -> Bool) -> NormLTL a -> NFA w
toBuchi trueAt = \ case
      TermN a      -> nfa [0] [2] [1] f
            where f 0 w | a `trueAt` w = [2]
                  f 2 _              = [2]
                  f _ _              = [1]
      AndN a b     -> A.intersection (toBuchi trueAt a) (toBuchi trueAt b)
      OrN a b      -> A.union (toBuchi trueAt a) (toBuchi trueAt b)
      UntilN a b   -> undefined
      ReleaseN a b -> undefined
      NextN a      -> A.concat (nfa [0] [1] [] f) (toBuchi trueAt a)
            where f _ _ = [1]
