module StructuredText.Testing
      ( f
      -- , prop_RevRev
      -- , prop_RevApp
      -- , prop_simpl
      -- , prop_simpl_sat
      -- , prop_aba_nba_equiv
      -- , prop_Vardi
      , prop_trans_correct
      , sequences
      , cantoraba
      ) where

-- import Test.QuickCheck (quickCheck, quickCheckWith, verboseCheck, verboseCheckWith, Arbitrary, stdArgs, Args)
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import StructuredText.Buchi (NBA (..))
import StructuredText.ToABA (toABA)
import StructuredText.LTL (NormLTL (..), AtomicProp (..), satisfies)

import StructuredText.DFA (DFA (..), accept, toDFA)

-- prop_RevRev :: (Eq a) => [a] -> Bool
-- prop_RevRev xs = reverse (reverse xs) == xs
--
-- prop_RevApp :: (Eq a) => [a] -> [a] -> Bool
-- prop_RevApp xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

--passing type argument to properties
f :: a -> ([a] -> b) -> [a] -> b
f _ prop = prop

--simplify is idempotent, applying the function twice gives the same result as applying once
-- prop_simpl :: (AtomicProp s, Eq s, Ord s) => B s -> Bool
-- prop_simpl b = simplify (simplify b) == simplify b

--simplify does not change whether a set satisfies a formula
--needs to be able to generate arbitrary B s and Set s instances

-- prop_simpl_sat :: (AtomicProp s, Eq s, Ord s) => B s -> Set s -> Bool
-- prop_simpl_sat formula set = satisfies set (simplify formula) == satisfies set formula

--an ABA and its equivalent NBA accept the same strings
-- prop_aba_nba_equiv :: (AtomicProp s, Eq a, Ord a, Ord s) => ABA s a -> [a] -> Bool
-- prop_aba_nba_equiv aut word = acceptABA aut word == acceptNBA (toBuchi (aut)) word

--Vardi ABA and equivalent NBA accept the same strings
--prop_Vardi :: (Eq a, Ord a, Ord s) => [a] -> Bool
--prop_Vardi word = prop_aba_nba_equiv (toABA ltlVardi) word

trans :: (Ord a, AtomicProp a) => NormLTL a -> DFA (Set a)
trans = toDFA . toABA

-- | Correctness for monitoring (though should define satisfies on LTL instead -- of NormLTL):
--   For all finite m :: [Set a], p :: NormLTL, (satisfies m p) if and only if (accept (trans p) m)
prop_trans_correct :: (Ord a, AtomicProp a) => NormLTL a -> [Set a] -> Bool
prop_trans_correct p m = satisfies m p == accept (trans p) m

sequences :: Ord a => DFA a -> [[a]]
sequences dfa = S.toList $ sequences' (statesDFA dfa) $ alphaDFA dfa

-- | length, alphabet, 
sequences' :: Ord a => Int -> Set a -> Set [a]
sequences' 0 _    = S.singleton []
sequences' n alph = s0 `S.union` S.map p2app (S.cartesianProduct s0 alph)
      where s0           = sequences' (n - 1) alph 
            p2app (a, b) = a ++ [b]

cantoraba :: NBA String Integer
cantoraba = NBA {statesNBA = S.fromList ["state 0", "state 1"] , initsNBA = S.fromList ["state 0"] , finalNBA = S.fromList["state 0"] , deltaNBA = tranFunc}
      where tranFunc :: String -> Integer -> Set String
            tranFunc s a = M.findWithDefault S.empty (s, a) tran

            tran :: Map (String, Integer) (Set String)
            tran = M.fromList[(("state 0", 0), S.singleton "state 0" ), (("state 0", 1), S.singleton "state 1"), (("state 0", 2), S.singleton "state 0")]
