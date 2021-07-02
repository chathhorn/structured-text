{-# LANGUAGE ScopedTypeVariables #-}

module StructuredText.ToABA
      ( toABA
      , funcMap
      , o, p, r
      , boolset
      , dual
      ) where

import StructuredText.LTL (AtomicProp (..), NormLTL (..), negNormLTL, atomSet)
import StructuredText.ABA (ABA (..), B (..))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

-- NormLTL a =
-- | TermN    a
-- | AndN     (NormLTL a) (NormLTL a)
-- | OrN      (NormLTL a) (NormLTL a)
-- | UntilN   (NormLTL a) (NormLTL a)
-- | ReleaseN (NormLTL a) (NormLTL a)
-- | NextN    (NormLTL a)

--THIS NEEDS TO BE EDITED!!!

dual :: AtomicProp a => B (NormLTL a) -> B (NormLTL a)
dual formula = case formula of
     BTrue        -> BFalse
     BFalse       -> BTrue
     BTerm b      -> BTerm (negNormLTL b)
     BAnd b1 b2   -> BOr (dual b1) (dual b2)
     BOr b1 b2    -> BAnd (dual b1) (dual b2)

--condition (Ord (NormLTL a)) needed for Set operations, added "deriving Ord" to NormLTL
subformulas :: Ord a => NormLTL a -> Set (NormLTL a)
subformulas ltl = case ltl of
     TermN a      -> S.singleton (TermN a)
     AndN a b     -> S.unions [S.singleton (AndN a b),   (subformulas a), (subformulas b)]
     OrN a b      -> S.unions [S.singleton (OrN a b),    (subformulas a), (subformulas b)]
     UntilN a b   -> S.unions [S.singleton (UntilN a b), (subformulas a), (subformulas b)]
     ReleaseN a b -> S.unions [S.singleton (ReleaseN a b), (subformulas a), (subformulas b)]
     NextN a      -> S.union  (S.singleton (NextN a))    (subformulas a)
     --RJ elimNeg NegN a       -> S.union  (S.singleton (NegN a))     (subformulas a)

allReleases :: Set (NormLTL a) -> Set (NormLTL a)
allReleases formulas = S.filter (\xs -> case xs of
                                       (ReleaseN _ _) -> True
                                       _              -> False) formulas

--letters in the alphabet has type AP a where a is the type of each propositional atom
toABA :: (AtomicProp a, Ord a) => NormLTL a -> ABA (NormLTL a) (Set a)
toABA ltl = ABA { statesABA = negSub
                , initABA   = BTerm ltl
                , finalABA  = allReleases negSub
                , deltaABA  = funcMap transition (S.toList (S.cartesianProduct negSub (S.powerSet (atomSet ltl))))
                }
      where negSub = S.union (subformulas ltl) (S.map negNormLTL (subformulas ltl))

            transition :: Ord a => NormLTL a -> Set a -> B (NormLTL a)
            transition formula alph = case formula of
                 TermN s             -> if S.member s alph then BTrue else BFalse
                 AndN s1 s2          -> BAnd (transition s1 alph) (transition s2 alph)
                 OrN s1 s2           -> BOr (transition s1 alph) (transition s2 alph)
                 UntilN s1 s2        -> BOr (transition s2 alph) (BAnd (transition s1 alph) (BTerm (UntilN s1 s2)))
                 ReleaseN s1 s2      -> BAnd (transition s2 alph) (BOr (transition s1 alph) (BTerm (ReleaseN s1 s2)))
                 NextN s             -> BTerm s

funcMap :: (Ord a, Ord b) => (a -> b -> c) -> [(a, b)] -> Map (a, b) c
funcMap func list = case list of
     x : xs -> M.insert x (func (fst x) (snd x)) (funcMap func xs)
     []     -> M.empty

--TESTING

-- ltl1 :: NormLTL (AP Char)
-- ltl1 = NegN (UntilN (TermN 's') (TermN 't'))
-- ltl1 = ReleaseN (TermN (atNot 's')) (TermN (atNot 't'))

-- ltl2 :: NormLTL (AP Char)
-- ltl2 = AndN (TermN 'r') (TermN 't')

-- ltlVardi :: NormLTL Char
-- ltlVardi = (NextN (NegN (TermN 'p'))) `UntilN` (TermN 'q')
-- ltlVardi = (NextN (TermN (atNot 'p'))) `UntilN` (TermN 'q')

-- phi :: NormLTL (AP Char)
-- phi = NextN (TermN 'p')

-- aba = toABA phi

o :: Set a
o = S.empty

p :: Set Char
p = S.singleton 'p'

r :: Set Char
r = S.fromList ['p', 'q']

-- ltl4 :: NormLTL (AP Char)
-- ltl4 = NegN ((NextN (NegN (TermN 'p'))) `UntilN` (TermN 'q'))
-- ltl4 = (NextN (TermN 'p')) `ReleaseN` (TermN (atNot 'q'))

boolset :: Set (B Char)
boolset = S.fromList [BTerm 's', BTerm 'r', BOr (BTerm 't') (BTerm 'u')]

-- bool1 :: B (NormLTL Char)
-- bool1 = BOr (BAnd (BTerm ltl1) (BTerm ltl2)) BTrue
