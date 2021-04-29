module StructuredText.Automata (NFA (..), nfa, union, concat) where

import Prelude hiding (concat)
import Data.Set (Set, fromList)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S

concatMapSet :: (Ord a, Ord b) => (a -> Set b) -> Set a -> Set b
concatMapSet f s = S.unions (S.map f s)

type Bounds = (Int, Int)
type States = Set Int
type BoundedStates = (States, Bounds)

data NFA a = NFA
      { bounds :: Bounds -- First state, last state.
      , inits  :: States
      , final  :: States
      , delta  :: a -> Int -> States
      }

fize :: (Eq a, Eq b, Ord c) => Set (a, b, c) -> a -> b -> Set c
fize s a b = S.map thrd (S.filter (\ x -> fst3 x == a && snd3 x == b) s)
      where fst3 :: (a, b, c) -> a
            fst3 (a, _, _) = a

            snd3 :: (a, b, c) -> b
            snd3 (_, b, _) = b

            thrd :: (a, b, c) -> c
            thrd (_, _, c) = c

run :: Eq a => NFA a -> [a] -> States
run nfa as = run' nfa as (inits nfa)
      where run' :: Eq a => NFA a -> [a] -> States -> States
            run' nfa (a : as) s = run' nfa as (concatMapSet (delta nfa a) s)
            run' _ []         s = s

nfa :: [Int] -> [Int] -> (a -> Int -> [Int]) -> NFA a
nfa i f d = NFA (lower, upper) ins fins (\ a b -> fromList (d a b))
      where lower :: Int
            lower = fromMaybe minBound $ S.lookupMin ins

            upper :: Int
            upper = fromMaybe maxBound $ S.lookupMax fins

            ins :: States
            ins = fromList i

            fins :: States
            fins = fromList f

shift :: Bounds -> Bounds -> Int
shift aBnds bBnds | fst bBnds >= snd aBnds = 0
shift aBnds bBnds = snd aBnds - fst bBnds

bnds :: Bounds -> Bounds -> Bounds
bnds aBnds bBnds = (fst aBnds, (shift aBnds bBnds) + snd bBnds)

djUnion :: Int -> States -> States -> States
djUnion sh a = S.union a . S.map (+ sh)

deltaUnion :: Int -> (a -> Int -> States) -> (a -> Int -> States) -> a -> Int -> States
deltaUnion sh a b w s = S.union (a w s) (S.map (+ sh) $ b w (s - sh))

union :: NFA a -> NFA a -> NFA a
union a b = NFA (bnds (bounds a) (bounds b))
      (djUnion sh (inits a) (inits b))
      (djUnion sh (final a) (final b))
      (deltaUnion sh (delta a) (delta b))
      where sh :: Int
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

            conc f w s | S.disjoint (f w s) (final a) = f w s
            conc f w s                                = S.union (f w s) (S.map (+ sh) $ inits b)

            sh :: Int
            sh = shift (bounds a) (bounds b)
