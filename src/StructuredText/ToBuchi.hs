{-# LANGUAGE ScopedTypeVariables #-}

module StructuredText.ToBuchi 
      ( toBuchi
      --, ltlVardi
      , phi
      --, aba, nba1, nba2
      , o, p, r
      ) where

import StructuredText.LTL (NormLTL (..))
import StructuredText.ABA (ABA (..), B (..), satisfy, simplify)
import StructuredText.Buchi (NBA (..))
import StructuredText.ToABA (toABA)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map.Strict (Map, insert)
import qualified Data.Map.Strict as M
import Data.List (find)

-- NormLTL a =
-- | TermN    a
-- | AndN     (NormLTL a) (NormLTL a)
-- | OrN      (NormLTL a) (NormLTL a)
-- | UntilN   (NormLTL a) (NormLTL a)
-- | ReleaseN (NormLTL a) (NormLTL a)
-- | NextN    (NormLTL a)

boolAnd :: (Eq s) => B s -> B s -> B s
boolAnd b1 b2 = simplify (BAnd b1 b2)

toBuchi :: (Ord a, Ord s, Ord (B s), Ord (NormLTL a)) => ABA s a -> NBA (Set s, Set s) a
toBuchi aba = NBA { statesNBA = states
                  , initsNBA  = S.singleton (S.singleton (extract (initABA aba)), S.empty) --initABA saved as BTerm s, need just s
                  , finalNBA  = S.cartesianProduct (S.singleton S.empty) (S.powerSet (statesABA aba))
                  , deltaNBA  = transition
                  }
     where states = S.cartesianProduct (S.powerSet (statesABA aba)) (S.powerSet (statesABA aba))
           
           extract :: B s -> s
           extract (BTerm b) = b

           --transition :: (Set s, Set s) -> a -> Set ((Set s, Set s))           
           transition (u, v) alph = S.filter (tfilter (u, v) alph) states          
          
           --tfilter :: (Set s, Set s) -> a -> (Set s, Set s) -> Bool
           tfilter (u, v) alph (u', v') = not (S.null (S.filter (tfilter2 (u, v) alph (u', v')) states)) 
 
           --tfilter2 :: (Set s, Set s) -> a -> (Set s, Set s) -> (Set s, Set s) -> Bool                            
           tfilter2 (u, v) alph (u', v') (x, y) | (S.null u) = satisfy (bigAnd v alph) y
                                                             && u' == S.difference y (finalABA aba)
                                                             && v' == S.intersection y (finalABA aba)         
                                                | otherwise  = satisfy (bigAnd u alph) x
                                                             && satisfy (bigAnd v alph) y
                                                             && u' == S.difference x (finalABA aba)
                                                             && v' == S.union y (S.intersection x (finalABA aba))         
                    
           --bigAnd :: Set s -> a -> B s
           --bigAnd set alph = S.foldl boolAnd BTrue (S.map ((flip (deltaABA aba)) alph) set)                 
              
           --bigAnd :: Set s -> a -> B s (deltaABA :: Map (s, a) (B s))
           bigAnd set alph = S.foldl boolAnd BTrue (S.map ((flip (mapFunc (deltaABA aba))) alph) set)

           --mapFunc :: Map (s, a) (B s) -> s -> a -> B s
           mapFunc map s a= M.findWithDefault BTrue (s, a) map

--trying to make toBuchi more efficient

--does there exist an element of set satisfying condition?
exists :: (Eq s) => (s -> Bool) -> Set s -> Bool
exists condition set | find condition set == Nothing = False
                     | otherwise = True 

toBuchi2 :: (Ord a, Ord s, Ord (B s), Ord (NormLTL a)) => ABA s a -> NBA (Set s, Set s) a
toBuchi2 aba = NBA { statesNBA = states
                  , initsNBA  = S.singleton (S.singleton (extract (initABA aba)), S.empty) --initABA saved as BTerm s, need just s
                  , finalNBA  = S.cartesianProduct (S.singleton S.empty) (S.powerSet (statesABA aba))
                  , deltaNBA  = transition
                  }
     where powerset = S.powerSet (statesABA aba)	
           
           states = S.cartesianProduct powerset powerset
           
           extract :: B s -> s
           extract (BTerm b) = b

           --transition :: (Set s, Set s) -> a -> Set ((Set s, Set s))           
           transition (u, v) alph = S.filter (tfilter (u, v) alph) states          
          
           --tfilter :: (Set s, Set s) -> a -> (Set s, Set s) -> Bool
           tfilter (u, v) alph (u', v') | u == S.empty = exists (tfilter2' v alph (u',v')) powerset
                                        | otherwise    = exists (tfilter2 (u,v) alph (u',v')) states
 
           --tfilter2 :: (Set s, Set s) -> a -> (Set s, Set s) -> (Set s, Set s) -> Bool                            
           tfilter2 (u, v) alph (u', v') (x, y) = satisfy (bigAnd u alph) x
                                                && satisfy (bigAnd v alph) y
                                                && u' == S.difference x (finalABA aba)
                                                && v' == S.union y (S.intersection x (finalABA aba))         
          
           --tfilter2' :: Set s -> a -> (Set s, Set s) -> Set s -> Bool
           tfilter2' v alph (u', v') y = satisfy (bigAnd v alph) y
                                       && u' == S.difference y (finalABA aba)
                                       && v' == S.intersection y (finalABA aba)         
           
           --bigAnd :: Set s -> a -> B s
           --bigAnd set alph = S.foldl boolAnd BTrue (S.map ((flip (deltaABA aba)) alph) set)                 

           --bigAnd :: Set s -> a -> B s (deltaABA :: Map (s, a) (B s))
           bigAnd set alph = S.foldl boolAnd BTrue (S.map ((flip (mapFunc (deltaABA aba))) alph) set)

           --mapFunc :: Map (s, a) (B s) -> s -> a -> B s
           mapFunc map s a= M.findWithDefault BTrue (s, a) map

--need to defined instance Eq (NBA s a) for this to work
--toBuchiEquiv :: NormLTL a -> Bool
--toBuchiEquiv ltl = toBuchi (toABA ltl) == toBuchi2 (toABA ltl)

--TESTING

--ltl1 :: NormLTL Char
--ltl1 = NegN (UntilN (TermN 's') (TermN 't')) 

ltl2 :: NormLTL Char
ltl2 = AndN (TermN 'r') (TermN 't')

--ltlVardi :: NormLTL Char
--ltlVardi = (NextN (NegN (TermN 'p'))) `UntilN` (TermN 'q')

phi :: NormLTL Char
phi = NextN (TermN 'p')

--aba = toABA phi

--nba1 = toBuchi aba

--nba2 = toBuchi2 aba

o = S.empty
p = S.singleton 'p'
r = S.fromList ['p', 'q']

--ltl4 :: NormLTL Char
--ltl4 = NegN ((NextN (NegN (TermN 'p'))) `UntilN` (TermN 'q'))

boolset :: Set (B Char)
boolset = S.fromList [BTerm 's', BTerm 'r', BOr (BTerm 't') (BTerm 'u')]

--bool1 :: B (NormLTL Char)
--bool1 = BOr (BAnd (BTerm ltl1) (BTerm ltl2)) BTrue
