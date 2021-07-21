module StructuredText.ToABA
      ( toABA
      , funcMap
      , LABA
      ) where

import StructuredText.LTL (AtomicProp (..), NormLTL (..), atoms)
import StructuredText.ABA (ABA (..))
import StructuredText.Boolean (B, dnfTrue, dnfFalse, dnfAnd, dnfOr, dnfTerm)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

-- | Condition (Ord (NormLTL a)) needed for Set operations, added "deriving Ord" to NormLTL
subformulas :: Ord a => NormLTL a -> Set (NormLTL a)
subformulas ltl = case ltl of
     TermN a      -> S.singleton (TermN a)
     AndN a b     -> S.unions [S.singleton (AndN a b),     subformulas a, subformulas b]
     OrN a b      -> S.unions [S.singleton (OrN a b),      subformulas a, subformulas b]
     UntilN a b   -> S.unions [S.singleton (UntilN a b),   subformulas a, subformulas b]
     ReleaseN a b -> S.unions [S.singleton (ReleaseN a b), subformulas a, subformulas b]
     NextN a      -> S.union  (S.singleton (NextN a))    $ subformulas a

allReleases :: Set (NormLTL a) -> Set (NormLTL a)
allReleases = S.filter $ \ case
      ReleaseN _ _ -> True
      _            -> False

type LABA a = ABA (NormLTL a) (Set a)

-- | Letters in the alphabet has type AP a where a is the type of each propositional atom
toABA :: (AtomicProp a, Ord a) => NormLTL a -> LABA a
toABA ltl = ABA { statesABA = negSub
                , alphaABA  = alphabet
                , currABA   = dnfTerm ltl
                , finalABA  = allReleases negSub
                , deltaABA  = transition
                }
      where negSub = S.union (subformulas ltl) (S.map atNot (subformulas ltl))

            alphabet = S.powerSet (atoms ltl)

            transition :: (AtomicProp a, Ord a) => NormLTL a -> Set a -> B (NormLTL a)
            transition formula alph = case formula of
                 TermN s             -> if S.member s alph then dnfTrue else dnfFalse
                 AndN s1 s2          -> transition s1 alph `dnfAnd` transition s2 alph
                 OrN s1 s2           -> transition s1 alph `dnfOr` transition s2 alph
                 UntilN s1 s2        -> transition s2 alph `dnfOr` (transition s1 alph `dnfAnd` dnfTerm (UntilN s1 s2))
                 ReleaseN s1 s2      -> transition s2 alph `dnfAnd` (transition s1 alph `dnfOr` dnfTerm (ReleaseN s1 s2))
                 NextN s             -> dnfTerm s

funcMap :: (Ord a, Ord b) => (a -> b -> c) -> [(a, b)] -> Map (a, b) c
funcMap func list = case list of
     x : xs -> M.insert x (uncurry func x) (funcMap func xs)
     []     -> M.empty

