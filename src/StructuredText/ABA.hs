module StructuredText.ABA
      ( ABA (..)
      , B (..)
      , satisfy
      , children
      , acceptRun
      , formulaRun     
      , aba1, set1, set2, set3, bs1
      ) where

import Prelude hiding (concat)
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.List (last)
import Text.Show.Functions
import StructuredText.LTL (atoms)

data B s = BTrue
         | BFalse
         | BTerm s
         | BAnd (B s) (B s)
         | BOr (B s) (B s)
     deriving (Show, Eq) 

--Where/how can I indicate that any disjunction with True is equivalent to True and that any conjunction with False is equivalent to False?
--If done, then wouldn't need so many cases for satisfy function
--Also want to specify "rules" such as "r and r equiv r"

satisfy :: (Ord s) => B s -> Set s -> Bool
satisfy formula set = case formula of
        BTrue          -> True
        BFalse         -> False
        BTerm b'       -> S.member b' set
        --BAnd BFalse _ -> False --Should this be here?
        --BAnd _ BFalse -> False --How to combine with case above? 
        --BAnd b1 b2     -> (satisfy b1 set) && (satisfy b2 set) 
        BAnd b1 b2     | (b1 == BFalse) || (b2 == BFalse) -> False
                       | otherwise -> (satisfy b1 set) && (satisfy b2 set)
        BOr b1 b2      | (b1 == BTrue) || (b2 == BTrue) -> True
                       | otherwise -> (satisfy b1 set) || (satisfy b2 set)
        --BOr BTrue _   -> True --Should this be here?
        --BOr _ BTrue   -> True --How to combine with case above?
        --BOr b1 b2      -> (satisfy b1 set) || (satisfy b2 set)   

children :: (Ord s) => B s -> Set s -> Set (Set s)
children formula atoms = S.filter (satisfy formula) (S.powerSet atoms)

--take powerSet of states s, filter based on whether or not satisfy formula f

data ABA state alph = ABA
      { states :: Set state
      , start  :: B state -- BTerm s
      , final  :: Set state
      , delta  :: state -> alph -> B state
      }

--Expands transition function to (B s, a) -> B s 
deltaP :: (Ord s, Ord a) => (s -> a -> B s) -> (B s -> a -> B s)
deltaP delta t a = case t of
        BTrue -> BTrue
        BFalse -> BFalse
        BTerm s -> delta s a
        BAnd b1 b2 -> BAnd (deltaP delta b1 a) (deltaP delta b2 a)
        BOr b1 b2 -> BOr (deltaP delta b1 a) (deltaP delta b2 a)

--Currently, computing formula-run and then checking whether or not accepted by ABA. In practice, better to compute this level's formula, check if it's satisfiable, then compute next level's formula. If formula equivalent to true (or false), can actually stop run

formulaRun :: (Ord s, Ord a) => ABA s a -> [a] -> [B s]
formulaRun aut word = case word of          
     [a]      -> start aut: [deltaP (delta aut) (start aut) a]
     (a : as) -> start aut: formulaRun aut{start = deltaP (delta aut) (start aut) a} as 
     []       -> [] 

acceptRun :: (Ord s, Ord a) => ABA s a -> [a] -> Bool
acceptRun aut word = case word of
     []  -> True
     (a) -> not (S.null (children (Data.List.last (formulaRun aut word)) (final aut))) --accept condition for finite words

--for testing

tran :: Map (Char, Integer) (B Char)
tran =  M.fromList[(('r', 0), BOr (BAnd (BTerm 'r') (BTerm 's')) (BTerm 't')), (('r', 1), BOr (BTerm 'r') (BTerm 's')), (('s', 0), BAnd (BTerm 's') (BTerm 't')), (('s', 1), BTrue), (('t', 0), BFalse), (('t', 1), BTerm 'r')]

tran_func :: Char -> Integer -> B Char
tran_func s a =  M.findWithDefault BFalse (s, a) tran 

aba1 :: ABA Char Integer
aba1 = ABA{states = S.fromList['r','s','t'], start = BTerm 's', final = S.fromList['t'], delta = tran_func}

set1 :: Set Char
set1 = S.fromList ['r', 's', 't'] 

set2 :: Set Char
set2 = S.fromList ['r', 's', 't', 'u']

set3 :: Set Char
set3 = S.fromList ['s', 't', 'u']

bs1 :: B Char
bs1 = BAnd (BTerm 'r') (BOr (BTerm 's') (BTerm 't'))
