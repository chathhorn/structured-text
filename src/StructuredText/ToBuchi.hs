module StructuredText.ToBuchi (toABA) where

import StructuredText.Automata (NFA, nfa)
import qualified StructuredText.Automata as A
import StructuredText.LTL (NormLTL (..))
import StructuredText.ABA (ABA (..), B (..))
import Data.Set (Set)

-- NormLTL a =
-- | TermN    a
-- | AndN     (NormLTL a) (NormLTL a)
-- | OrN      (NormLTL a) (NormLTL a)
-- | UntilN   (NormLTL a) (NormLTL a)
-- | ReleaseN (NormLTL a) (NormLTL a)
-- | NextN    (NormLTL a)

toABA :: NormLTL a -> ABA (NormLTL a) (Set a)
toABA ltl = ABA { states = subformulas
                , start   = ltl
                , final  = allUntils
                , delta  = trans
                }
      where subformulas :: Set (NormLTL a)
            subformulas = case ltl of
                  TermN a    -> undefined
                  AndN a b   -> undefined
                  OrN a b    -> undefined
                  UntilN a b -> undefined
                  NextN a    -> undefined
                  _          -> undefined

            allUntils :: Set (NormLTL a)
            allUntils = undefined

            trans :: NormLTL a -> Set a -> B (NormLTL a)
            trans (TermN a)    = undefined
            trans (AndN a b)   = undefined
            trans (OrN a b)    = undefined
            trans (UntilN a b) = undefined
            trans (NextN a)    = undefined
            trans _            = undefined

