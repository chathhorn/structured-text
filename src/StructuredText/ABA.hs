module StructuredText.ABA
      ( ABA (..)
      , B (..)
      , simplify
      , simplifyLTL
      , satisfy
      , children
      , acceptRun
      , formulaRun     
      , aba1, set1, set2, set3, bs1, bs2, bs3
      ) where

import Prelude hiding (concat)
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.List (last)
import Text.Show.Functions
import StructuredText.LTL

data B s = BTrue
         | BFalse
         | BTerm s
         | BAnd (B s) (B s)
         | BOr (B s) (B s)
     deriving (Show, Ord, Eq)

{-
instance Eq (B s) where
     BAnd BFalse _ == BFalse  = True
     BAnd _ BFalse == BFalse  = True
     BOr BTrue _   == BTrue   = True
     BOr _ BTrue   == BTrue   = True
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
simplify (BAnd BTrue b)  = b
simplify (BAnd b BTrue)  = b
simplify (BAnd b1 b2)    | (b1 == b2) = b1
simplify (BOr BTrue _)   = BTrue
simplify (BOr _ BTrue)   = BTrue
simplify (BOr BFalse b)  = b
simplify (BOr b BFalse)  = b
simplify (BOr b1 b2)     | (b1 == b2) = b1
simplify ltl             = ltl

satisfy :: (Ord s) => B s -> Set s -> Bool
satisfy formula set = case formula of
     BTrue          -> True
     BFalse         -> False
     BTerm b'       -> S.member b' set
     BAnd b1 b2     -> (satisfy b1 set) && (satisfy b2 set)
     BOr b1 b2      -> (satisfy b1 set) || (satisfy b2 set)

--take powerSet of states s, filter based on whether or not satisfy formula f
children :: (Ord s) => B s -> Set s -> Set (Set s)
children formula atoms = S.filter (satisfy formula) (S.powerSet atoms)

data ABA state alph = ABA
      { statesABA :: Set state
      , initABA   :: B state -- BTerm s
      , finalABA  :: Set state
      , deltaABA  :: state -> alph -> B state
      }

--Expands transition function to (B s, a) -> B s 
deltaP :: (Ord s, Ord a) => (s -> a -> B s) -> (B s -> a -> B s)
deltaP delta t a = case t of
        BTrue -> BTrue
        BFalse -> BFalse
        BTerm s -> delta s a
        BAnd b1 b2 -> BAnd (deltaP delta (simplify b1) a) (deltaP delta (simplify b2) a)
        BOr b1 b2 -> BOr (deltaP delta (simplify b1) a) (deltaP delta (simplify b2) a)

--Currently, computing formula-run and then checking whether or not accepted by ABA. In practice, better to compute this level's formula, check if it's satisfiable, then compute next level's formula. If formula equivalent to true (or false), can actually stop run

--Need to have a separate formulaRunLTL function?

formulaRun :: (Ord s, Ord a) => ABA s a -> [a] -> [B s]
formulaRun aut word = case word of          
     [a]      -> simplify(initABA aut): [simplify (deltaP (deltaABA aut) (initABA aut) a)]
     (a : as) -> simplify(initABA aut): formulaRun aut{initABA = deltaP (deltaABA aut) (initABA aut) a} as 
     []       -> [] 

acceptRun :: (Ord s, Ord a) => ABA s a -> [a] -> Bool
acceptRun aut word = case word of
     []    -> True
     (a:_) -> not (S.null (children (Data.List.last (formulaRun aut word)) (finalABA aut))) --accept condition for finite words

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
