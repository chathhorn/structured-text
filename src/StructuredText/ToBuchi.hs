module StructuredText.ToBuchi 
      ( toABA
      , dual
      , subformulas
      , ltl1, ltl2, ltlVardi, ltl4
      , bool1
      ) where

import StructuredText.Automata (NFA, nfa)
import StructuredText.LTL (NormLTL (..))
import StructuredText.ABA (ABA (..), B (..))
import Data.Set (Set)
import qualified Data.Set as S
--import Data.Map.Strict (Map)
--import qualified Data.Map.Strict as M  

-- NormLTL a =
-- | TermN    a
-- | AndN     (NormLTL a) (NormLTL a)
-- | OrN      (NormLTL a) (NormLTL a)
-- | UntilN   (NormLTL a) (NormLTL a)
-- | ReleaseN (NormLTL a) (NormLTL a)
-- | NextN    (NormLTL a)
-- | NegN     (NormLTL a)

neg :: NormLTL a -> NormLTL a
neg (NegN s) = s
neg s        = NegN s

dual :: B (NormLTL a) -> B (NormLTL a)
dual form = case form of
        BTrue        -> BFalse
        BFalse       -> BTrue
        BTerm b'     -> BTerm (neg b')
        BAnd b1 b2   -> BOr (dual b1) (dual b2)
        BOr b1 b2    -> BAnd (dual b1) (dual b2)

--condition (Ord (NormLTL a)) needed for Set operations, added "deriving Ord" to NormLTL
subformulas :: (Ord (NormLTL a)) => NormLTL a -> Set (NormLTL a)
subformulas ltl = case ltl of
     TermN a    -> S.singleton (TermN a)
     AndN a b   -> S.unions [S.singleton (AndN a b),   (subformulas a), (subformulas b)]
     OrN a b    -> S.unions [S.singleton (OrN a b),    (subformulas a), (subformulas b)]
     UntilN a b -> S.unions [S.singleton (UntilN a b), (subformulas a), (subformulas b)]
     NextN a    -> S.union  (S.singleton (NextN a))    (subformulas a)
     NegN a     -> S.union  (S.singleton (NegN a))     (subformulas a) 
     _          -> S.empty

allUntils :: Set (NormLTL a) -> Set (NormLTL a)
allUntils formula = S.filter (\xs -> case xs of
                                       NegN (UntilN _ _) -> True
                                       _                 -> False) formula

toABA :: (Ord (NormLTL a), Ord a) => NormLTL a -> ABA (NormLTL a) (Set a)
toABA ltl = ABA { states = S.union (subformulas ltl) (S.map neg (subformulas ltl))
                , start  = BTerm ltl
                , final  = allUntils (S.union (subformulas ltl) (S.map neg (subformulas ltl))) --final = allUntils states gives error
                , delta  = transition
                }
      where transition :: (Ord a) => NormLTL a -> Set a -> B (NormLTL a)
            transition formula a = case formula of
                 TermN s        -> if S.member s a then BTrue else BFalse
                 AndN s1 s2     -> BAnd (transition s1 a) (transition s2 a)
                 OrN s1 s2      -> BOr (transition s1 a) (transition s2 a)
                 UntilN s1 s2   -> BOr (transition s2 a) (BAnd (transition s1 a) (BTerm (UntilN s1 s2)))
                 ReleaseN s1 s2 -> transition (NegN (UntilN (NegN s1) (NegN s2))) a --Vardi's version of LTL doesn't have a R operator 
                 NextN s        -> BTerm s
                 NegN s         -> dual (transition s a)

--small examples for testing

ltl1 :: NormLTL Char
ltl1 = NegN (UntilN (TermN 's') (TermN 't')) 

ltl2 :: NormLTL Char
ltl2 = AndN (TermN 'r') (TermN 't')

ltlVardi :: NormLTL Char
ltlVardi = (NextN (NegN (TermN 'p'))) `UntilN` (TermN 'q')

ltl4 :: NormLTL Char
ltl4 = NegN ((NextN (NegN (TermN 'p'))) `UntilN` (TermN 'q'))

bool1 :: B (NormLTL Char)
bool1 = BOr (BAnd (BTerm ltl1) (BTerm ltl2)) BTrue
