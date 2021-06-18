{-# LANGUAGE RankNTypes #-}

module StructuredText.ABA
      ( ABA (..)
      , B (..)
      , simplify
      , simplifyLTL
      , satisfy
      , acceptABA
      , formulaRun, acceptABAwithRun    
      , aba1, set1, set2, set3, bs1, bs2, bs3
      ) where

import Prelude hiding (concat)
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.List (last, find)
import Text.Show.Functions
import StructuredText.LTL
import Test.QuickCheck (oneof, sized, Arbitrary (..), Gen (..))

data B s = BTrue
         | BFalse
         | BTerm s
         | BAnd (B s) (B s)
         | BOr (B s) (B s)
     deriving (Show, Ord, Eq)

instance Arbitrary (B s) where
     arbitrary = sized arbB

arbB :: Int -> Gen (B s)
arbB 0 = oneof [pure BTrue, pure BFalse] 
arbB n = do
     a <- arbB (n - 1)
     b <- arbB (n - 1)
     oneof [pure (BAnd a b), pure (BOr a b)]

{- instance Eq (B s) where
 -  BAnd BFalse _ == BFalse  = True
 -  BAnd _ BFalse == BFalse  = True
 -  BOr BTrue _   == BTrue   = True
 -  BOr _ BTrue   == BTrue   = True 
-}

--want to specify behavior when taking boolean combination of NormLTL formulas as opposed to other types
--possible to do cases based on the TYPE of s, not the pattern?
simplifyLTL :: (Eq a) => B (NormLTL a) -> B (NormLTL a)
simplifyLTL (BAnd (BTerm a) (BTerm (NegN b))) | (a == b) = BFalse
                                              | otherwise = BAnd (BTerm a) (BTerm (NegN b))
simplifyLTL (BOr (BTerm a) (BTerm (NegN b)))  | (a == b) = BTrue      
                                              | otherwise = BOr (BTerm a) (BTerm (NegN b))
 
simplify :: (Eq s) => B s -> B s
simplify (BAnd BFalse _) = BFalse
simplify (BAnd _ BFalse) = BFalse
simplify (BAnd BTrue b)  = simplify b
simplify (BAnd b BTrue)  = simplify b
simplify (BAnd b1 b2)    | (b1 == b2) = simplify b1
simplify (BOr BTrue _)   = BTrue
simplify (BOr _ BTrue)   = BTrue
simplify (BOr BFalse b)  = simplify b
simplify (BOr b BFalse)  = simplify b
simplify (BOr b1 b2)     | (b1 == b2) = simplify b1
simplify ltl             = ltl

satisfy :: (Ord s) => B s -> Set s -> Bool
satisfy formula set = case formula of
     BTrue          -> True
     BFalse         -> False
     BTerm b'       -> S.member b' set
     BAnd b1 b2     -> (satisfy b1 set) && (satisfy b2 set)
     BOr b1 b2      -> (satisfy b1 set) || (satisfy b2 set)

--is the given formula satisfied by any subset of the atoms?
satisfied :: (Ord s) => B s -> Set s -> Bool
satisfied formula atoms | find (satisfy (simplify formula)) (S.powerSet atoms) == Nothing = False
                        | otherwise = True

--what are all subsets of atoms that satisfy the formula?
children :: (Ord s) => B s -> Set s -> Set (Set s)
children formula atoms = S.filter (satisfy (simplify formula)) (S.powerSet atoms)

data ABA state alph = ABA
      { statesABA :: Set state
      , initABA   :: B state -- BTerm s
      , finalABA  :: Set state
      , deltaABA  :: state -> alph -> B state
      }

--Expand (s -> a -> B s) to (B s -> a -> B s) 
deltaP :: (Ord s, Ord a) => (s -> a -> B s) -> (B s -> a -> B s)
deltaP delta t a = case t of
        BTrue -> BTrue
        BFalse -> BFalse
        BTerm s -> delta s a
        BAnd b1 b2 -> BAnd (deltaP delta (simplify b1) a) (deltaP delta (simplify b2) a)
        BOr b1 b2 -> BOr (deltaP delta (simplify b1) a) (deltaP delta (simplify b2) a)

--does the ABA aut accept the input word?
--if boolean expression BTrue, accept; if BFalse; reject; else continue run
acceptABA :: (Ord s, Ord a) => ABA s a -> [a] -> Bool
acceptABA aut (a : as) = case next_state of
     BTrue     -> True
     BFalse    -> False
     otherwise -> acceptABA aut{initABA = next_state} as
     where next_state = simplify (deltaP (deltaABA aut) (initABA aut) a)
acceptABA aut [] = satisfied (initABA aut) (finalABA aut)      

--sequence of B s expressions for a word through an ABA aut
formulaRun :: (Ord s, Ord a) => ABA s a -> [a] -> [B s]
formulaRun aut word = case word of          
     (a : as) -> simplify(initABA aut): formulaRun aut{initABA = deltaP (deltaABA aut) (initABA aut) a} as 
     []       -> [simplify (initABA aut)] 

--is word accepted by automata? what is its formula-run?
--uses formulaRun so entire run is generated
acceptABAwithRun :: (Ord s, Ord a) => ABA s a -> [a] -> (Bool, [B s])
acceptABAwithRun aut word = (satisfied (Data.List.last run) (finalABA aut), run)
     where run = formulaRun aut word
        
--for testing

tran :: Map (Char, Integer) (B Char)
tran =  M.fromList[(('r', 0), BOr (BAnd (BTerm 'r') (BTerm 's')) (BTerm 't')), (('r', 1), BOr (BTerm 'r') (BTerm 's')), (('s', 0), BAnd (BTerm 's') (BTerm 't')), (('s', 1), BTrue), (('t', 0), BFalse), (('t', 1), BTerm 'r')]

tran_func :: Char -> Integer -> B Char
tran_func s a =  M.findWithDefault BFalse (s, a) tran 

aba1 :: ABA Char Integer
aba1 = ABA{statesABA = S.fromList['r','s','t'], initABA = BTerm 's', finalABA = S.fromList['t'], deltaABA = tran_func}

set1 :: Set Char
set1 = S.fromList ['r', 's', 't'] 

set2 :: Set Char
set2 = S.fromList ['r', 's', 't', 'u']

set3 :: Set Char
set3 = S.fromList ['s', 't', 'u']

bs1 :: B Char
bs1 = BAnd (BTerm 'r') (BOr (BTerm 's') (BTerm 't'))

bs2 :: B Char
bs2 = BOr BTrue (BTerm 'r')

bs3 :: B Char
bs3 = BAnd BFalse (BTerm 'r')
