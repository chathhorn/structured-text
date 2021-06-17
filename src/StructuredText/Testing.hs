module StructuredText.Testing
      ( prop_RevRev
      , prop_RevApp
      , prop_simpl
      , prop_simpl_sat
      , prop_aba_nba_equiv
      , prop_Vardi
      ) where

import Test.QuickCheck (quickCheck, quickCheckWith, verboseCheck, verboseCheckWith)
import Data.Set (Set)
import StructuredText.ABA 
      ( B (..)
      , ABA (..)
      , simplify
      , satisfy
      , acceptABA)
import StructuredText.Buchi (acceptNBA)
import StructuredText.ToBuchi (toABA, toBuchi, ltlVardi)

--testing quickCheck
prop_RevRev :: Eq a => [a] -> Bool
prop_RevRev xs = reverse (reverse xs) == xs

prop_RevApp :: [Int] -> [Int] -> Bool
prop_RevApp xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

--simplify is idempotent
--needs to be able to generate arbitrary instances of B s
prop_simpl :: (Eq s) => B s -> Bool
prop_simpl b = simplify (simplify b) == simplify b

--simplify does not change whether a set satisfies a formula
--needs to be able to generate arbitrary B s and Set s instances

prop_simpl_sat :: (Eq s, Ord s) => B s -> Set s -> Bool
prop_simpl_sat formula set = satisfy (simplify formula) set == satisfy formula set

--an ABA and its equivalent NBA accept the same strings
prop_aba_nba_equiv :: (Eq a, Ord a, Ord s) => ABA s a -> [a] -> Bool
prop_aba_nba_equiv aut word = acceptABA aut word == acceptNBA (toBuchi (aut)) word

--Vardi ABA and equivalent NBA accept the same strings
--prop_Vardi :: (Eq a, Ord a, Ord s) => [a] -> Bool
prop_Vardi word = prop_aba_nba_equiv (toABA ltlVardi) word
