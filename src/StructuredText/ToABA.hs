{-# LANGUAGE ScopedTypeVariables #-}

module StructuredText.ToABA 
      ( toABA
      , funcMap
      , ltlVardi
      , phi, aba, o, p, r
      ) where

import StructuredText.Automata (NFA, nfa)
import StructuredText.LTL (NormLTL (..), atomSet)
import StructuredText.ABA (ABA (..), B (..), satisfy, simplify)
import StructuredText.Buchi (NBA (..))
import Data.Set (Set, null)
import qualified Data.Set as S
import Data.Map.Strict (Map, insert, empty)
import qualified Data.Map.Strict as M
import Data.List (find)

--import Data.Map.Strict (Map)
--import qualified Data.Map.Strict as M  

-- NormLTL a =
-- | TermN    a
-- | AndN     (NormLTL a) (NormLTL a)
-- | OrN      (NormLTL a) (NormLTL a)
-- | UntilN   (NormLTL a) (NormLTL a)
-- | ReleaseN (NormLTL a) (NormLTL a)
-- | NextN    (NormLTL a)

--THIS NEEDS TO BE EDITED!!!
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
     TermN a      -> S.singleton (TermN a)
     AndN a b     -> S.unions [S.singleton (AndN a b),   (subformulas a), (subformulas b)]
     OrN a b      -> S.unions [S.singleton (OrN a b),    (subformulas a), (subformulas b)]
     UntilN a b   -> S.unions [S.singleton (UntilN a b), (subformulas a), (subformulas b)]
     ReleaseN a b -> S.unions [S.singleton (ReleaseN a b), (subformulas a), (subformulas b)]    
     NextN a      -> S.union  (S.singleton (NextN a))    (subformulas a)
     --RJ elimNeg NegN a       -> S.union  (S.singleton (NegN a))     (subformulas a) 
     _            -> S.empty

allUntils :: Set (NormLTL a) -> Set (NormLTL a)
allUntils formula = S.filter (\xs -> case xs of
                                       NegN (UntilN _ _) -> True
                                       _                 -> False) formula

toABA :: (Ord (NormLTL a), Ord a) => NormLTL a -> ABA (NormLTL a) (Set a)
toABA ltl = ABA { statesABA = negSub
                , initABA   = BTerm ltl
                , finalABA  = allUntils negSub
                , deltaABA  = funcMap transition (S.toList (S.cartesianProduct negSub (S.powerSet (atomSet ltl))))
                }
      where negSub = S.union (subformulas ltl) (S.map neg (subformulas ltl))
                        
            transition :: (Ord a) => NormLTL a -> Set a -> B (NormLTL a)
            transition formula a = case formula of
                 TermN s        -> if S.member s a then BTrue else BFalse
                 AndN s1 s2     -> BAnd (transition s1 a) (transition s2 a)
                 OrN s1 s2      -> BOr (transition s1 a) (transition s2 a)
                 UntilN s1 s2   -> BOr (transition s2 a) (BAnd (transition s1 a) (BTerm (UntilN s1 s2)))
                 ReleaseN s1 s2 -> transition (NegN (UntilN (NegN s1) (NegN s2))) a --def of ReleaseN in terms of NegN and UntilN 
                 NextN s        -> BTerm s
                 NegN s         -> dual (transition s a)

funcMap :: (Ord a, Ord b) => (a -> b -> c) -> [(a, b)] -> Map (a, b) c
funcMap func list = case list of
     x : xs -> M.insert x (func (fst x) (snd x)) (funcMap func xs)
     []     -> M.empty 
     
--TESTING

ltl1 :: NormLTL Char
ltl1 = NegN (UntilN (TermN 's') (TermN 't')) 

ltl2 :: NormLTL Char
ltl2 = AndN (TermN 'r') (TermN 't')

ltlVardi :: NormLTL Char
ltlVardi = (NextN (NegN (TermN 'p'))) `UntilN` (TermN 'q')

phi :: NormLTL Char
phi = NextN (TermN 'p')

aba = toABA phi

o = S.empty
p = S.singleton 'p'
r = S.fromList ['p', 'q']

ltl4 :: NormLTL Char
ltl4 = NegN ((NextN (NegN (TermN 'p'))) `UntilN` (TermN 'q'))

boolset :: Set (B Char)
boolset = S.fromList [BTerm 's', BTerm 'r', BOr (BTerm 't') (BTerm 'u')]

bool1 :: B (NormLTL Char)
bool1 = BOr (BAnd (BTerm ltl1) (BTerm ltl2)) BTrue
