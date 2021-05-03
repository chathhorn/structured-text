module StructuredText.Automata
      ( NFA (..)
      , nfa
      , union
      , concat
      , intersection
      ) where

import Prelude hiding (concat)
import Data.Set (Set, fromList)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Map.Strict as M

type Bounds = (Integer, Integer) -- [min, max)
type State = Integer
type States = Set State
-- type BoundedStates = (States, Bounds)

data NFA a = NFA
      { bounds :: Bounds -- First state, last state.
      , inits  :: States
      , final  :: States
      , delta  :: Map State (a -> States)
      }

-- concatMapSet :: (Ord a, Ord b) => (a -> Set b) -> Set a -> Set b
-- concatMapSet f s = S.unions (S.map f s)

-- fize :: (Eq a, Eq b, Ord c) => Set (a, b, c) -> a -> b -> Set c
-- fize s a b = S.map thrd (S.filter (\ x -> fst3 x == a && snd3 x == b) s)
--       where fst3 :: (a, b, c) -> a
--             fst3 (a, _, _) = a
-- 
--             snd3 :: (a, b, c) -> b
--             snd3 (_, b, _) = b
-- 
--             thrd :: (a, b, c) -> c
--             thrd (_, _, c) = c

-- run :: Eq a => NFA a -> [a] -> States
-- run nfa as = run' nfa as (inits nfa)
--       where run' :: Eq a => NFA a -> [a] -> States -> States
--             run' nfa (a : as) s = run' nfa as (concatMapSet (delta nfa a) s)
--             run' _ []         s = s

-- A perhaps more convenient NFA constructor:
-- init states,
-- final states,
-- other states,
-- transition function (delta).
nfa :: [State] -> [State] -> [State] -> (State -> a -> [State]) -> NFA a
nfa ins fins others f = NFA (lower, upper) ins' fins' d
      where lower :: Integer
            lower = fromMaybe 0 $ S.lookupMin allStates

            upper :: Integer
            upper = fromMaybe 0 $ S.lookupMax allStates

            ins' :: States
            ins' = fromList ins

            fins' :: States
            fins' = fromList fins

            others' :: States
            others' = fromList others

            allStates :: States
            allStates = ins' `S.union` fins' `S.union` others'

            -- d :: Map State (a -> States)
            d = M.fromSet (\ s -> fromList . (f s)) allStates

shift :: Bounds -> Bounds -> Integer
shift aBnds bBnds | fst bBnds >= snd aBnds = 0
shift aBnds bBnds = snd aBnds - fst bBnds

bnds :: Bounds -> Bounds -> Bounds
bnds aBnds bBnds = (fst aBnds, (shift aBnds bBnds) + snd bBnds)

djUnion :: Integer -> States -> States -> States
djUnion sh a = S.union a . S.map (+ sh)

deltaUnion :: Integer
           -> Map State (a -> States)
           -> Map State (a -> States)
           -> Map State (a -> States)
deltaUnion sh a = M.union a . M.mapKeys (+ sh) . M.map (S.map (+ sh) .)

union :: NFA a -> NFA a -> NFA a
union a b = NFA (bnds (bounds a) (bounds b))
      (djUnion sh (inits a) (inits b))
      (djUnion sh (final a) (final b))
      (deltaUnion sh (delta a) (delta b))
      where sh :: Integer
            sh = shift (bounds a) (bounds b)

intersection :: NFA a -> NFA a -> NFA a
intersection a b = undefined

concat :: NFA a -> NFA a -> NFA a
concat a b = NFA (bnds (bounds a) (bounds b))
      i'
      (S.map (+ shift (bounds a) (bounds b)) $ final b)
      (deltaUnion sh (conc (delta a)) (delta b))
      where i' :: States
            i' = if S.disjoint (inits a) (final a)
                 then inits a else S.union (inits a) (S.map (+ sh) $ inits b)

            conc :: Map State (a -> States) -> Map State (a -> States)
            conc = M.map $ \ f w -> if S.disjoint (f w) (final a) then (f w)
                        else S.union (f w) (S.map (+ sh) $ inits b)
            -- conc = M.map (\ s -> if S.disjoint s (final a) then s
            --    else S.union s (S.map (+ sh) $ inits b))
            -- conc f w s | S.disjoint (f w s) (final a)
            --       = f w s
            -- conc f w s
            --       = S.union (f w s) (S.map (+ sh) $ inits b)

            sh :: Integer
            sh = shift (bounds a) (bounds b)
