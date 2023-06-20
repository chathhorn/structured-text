{-# LANGUAGE ScopedTypeVariables #-}

module StructuredText.ToBuchi
      ( toBuchi, toBuchi2
      -- , phi
      -- , o, p, r
      -- , ltl2, boolset
      , exists
      ) where

import StructuredText.LTL (AtomicProp (..))
import StructuredText.ABA (ABA (..))
import StructuredText.Boolean (B, satisfies, dnfTrue, dnfAnd, toList)
import StructuredText.Buchi (NBA (..))
import Data.Set (Set)
import qualified Data.Set as S
import Data.List (find)

toBuchi :: (AtomicProp s, Ord a, Ord s) => ABA s a -> NBA (Set s, Set s) a
toBuchi aba = NBA { statesNBA = states
                  , initsNBA  = S.singleton (S.fromList (extract (currABA aba)), S.empty) --currABA saved as BTerm s, need just s
                  , finalNBA  = S.cartesianProduct (S.singleton S.empty) (S.powerSet (statesABA aba))
                  , deltaNBA  = transition
                  }
     where states = S.cartesianProduct (S.powerSet (statesABA aba)) (S.powerSet (statesABA aba))

           extract :: Ord s => B s -> [s]
           extract = concat . toList

           --transition :: (Set s, Set s) -> a -> Set ((Set s, Set s))
           transition (u, v) alph = S.filter (tfilter (u, v) alph) states

           --tfilter :: (Set s, Set s) -> a -> (Set s, Set s) -> Bool
           tfilter (u, v) alph (u', v') = not (S.null (S.filter (tfilter2 (u, v) alph (u', v')) states))

           --tfilter2 :: (Set s, Set s) -> a -> (Set s, Set s) -> (Set s, Set s) -> Bool
           tfilter2 (u, v) alph (u', v') (x, y) | S.null u  = satisfies y (bigAnd v alph)
                                                            && u' == S.difference y (finalABA aba)
                                                            && v' == S.intersection y (finalABA aba)
                                                | otherwise = satisfies x (bigAnd u alph)
                                                            && satisfies y (bigAnd v alph)
                                                            && u' == S.difference x (finalABA aba)
                                                            && v' == S.union y (S.intersection x (finalABA aba))

           --bigAnd :: Set s -> a -> B s (deltaABA :: Map (s, a) (B s))
           bigAnd set alph = S.foldl dnfAnd dnfTrue (S.map (flip (deltaABA aba) alph) set)

--trying to make toBuchi more efficient

--does there exist an element of set satisfying condition?
exists :: Eq s => (s -> Bool) -> Set s -> Bool
exists condition set | find condition set == Nothing = False
                     | otherwise = True

toBuchi2 :: (AtomicProp s, Ord a, Ord s) => ABA s a -> NBA (Set s, Set s) a
toBuchi2 aba = NBA { statesNBA = states
                  , initsNBA  = S.singleton (S.fromList (extract (currABA aba)), S.empty) --currABA saved as BTerm s, need just s
                  , finalNBA  = S.cartesianProduct (S.singleton S.empty) (S.powerSet (statesABA aba))
                  , deltaNBA  = transition
                  }
     where powerset = S.powerSet (statesABA aba)

           states = S.cartesianProduct powerset powerset

           extract :: Ord s => B s -> [s]
           extract = concat . toList

           --transition :: (Set s, Set s) -> a -> Set ((Set s, Set s))
           transition (u, v) alph = S.filter (tfilter (u, v) alph) states

           --tfilter :: (Set s, Set s) -> a -> (Set s, Set s) -> Bool
           tfilter (u, v) alph (u', v') | u == S.empty = exists (tfilter2' v alph (u',v')) powerset
                                        | otherwise    = exists (tfilter2 (u,v) alph (u',v')) states

           --tfilter2 :: (Set s, Set s) -> a -> (Set s, Set s) -> (Set s, Set s) -> Bool
           tfilter2 (u, v) alph (u', v') (x, y) = satisfies x (bigAnd u alph)
                                                && satisfies y (bigAnd v alph)
                                                && u' == S.difference x (finalABA aba)
                                                && v' == S.union y (S.intersection x (finalABA aba))

           --tfilter2' :: Set s -> a -> (Set s, Set s) -> Set s -> Bool
           tfilter2' v alph (u', v') y = satisfies y (bigAnd v alph)
                                       && u' == S.difference y (finalABA aba)
                                       && v' == S.intersection y (finalABA aba)

           --bigAnd :: Set s -> a -> B s (deltaABA :: Map (s, a) (B s))
           bigAnd set alph = S.foldl dnfAnd dnfTrue (S.map (flip (deltaABA aba) alph) set)

--need to defined instance Eq (NBA s a) for this to work
--toBuchiEquiv :: NormLTL a -> Bool
--toBuchiEquiv ltl = toBuchi (toABA ltl) == toBuchi2 (toABA ltl)

--TESTING

--ltl1 :: NormLTL Char
--ltl1 = NegN (UntilN (TermN 's') (TermN 't'))

-- ltl2 :: NormLTL Char
-- ltl2 = AndN (TermN 'r') (TermN 't')

--ltlVardi :: NormLTL Char
--ltlVardi = (NextN (NegN (TermN 'p'))) `UntilN` (TermN 'q')

-- phi :: NormLTL Char
-- phi = NextN (TermN 'p')

--aba = toABA phi

--nba1 = toBuchi aba

--nba2 = toBuchi2 aba

-- o :: Set a
-- o = S.empty
-- 
-- p :: Set Char
-- p = S.singleton 'p'
-- 
-- r :: Set Char
-- r = S.fromList ['p', 'q']

--ltl4 :: NormLTL Char
--ltl4 = NegN ((NextN (NegN (TermN 'p'))) `UntilN` (TermN 'q'))

-- boolset :: Set (B Char)
-- boolset = S.fromList [BTerm 's', BTerm 'r', BOr (BTerm 't') (BTerm 'u')]

--bool1 :: B (NormLTL Char)
--bool1 = BOr (BAnd (BTerm ltl1) (BTerm ltl2)) BTrue
