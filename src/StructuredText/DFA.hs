module StructuredText.DFA
      ( DFA (..)
      , LDFA
      , toDFA
      ) where

import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Control.Arrow ((&&&), (***))
import StructuredText.LTL (AtomicProp (..))
import StructuredText.ABA (ABA (..))
import StructuredText.Boolean (satSet, B (..), injectDNF, dnf)

import Debug.Trace (trace)

data ABA' s a = ABA'
      { statesABA' :: !(Set (Set (Set s)))
      , alphaABA'  :: !(Set a)
      , currABA'   :: !(Set (Set s))
      , finalABA'  :: !(Set s)
      , deltaABA'  :: !(Map (Set (Set s), a) (Set (Set s)))
      }

toABA' :: (Ord s, Ord a, AtomicProp s) => ABA s a -> ABA' s a
toABA' aba = ABA'
      { statesABA' = S.map satSet allStates
      , alphaABA'  = alphaABA aba
      , currABA'   = satSet $ currABA aba
      , finalABA'  = finalABA aba
      , deltaABA'  = mapify
      }
      where -- mapify :: (B s -> a -> B s) -> Map (Set (Set s), a) (Set (Set s))
            mapify = M.fromAscList $ S.toAscList (S.map ((satSet *** id) &&& satSet . (uncurry d)) allCases)

            -- allCases :: Set (B s, a)
            allCases = S.cartesianProduct allStates (alphaABA aba)

            -- allStates :: Set (B s)
            allStates = S.singleton BFalse `S.union` iter2 d (S.singleton $ currABA aba) (alphaABA aba)

            -- d :: B s -> a -> B s
            d s = injectDNF . dnf . (deltaP' (deltaABA aba) s)

            -- | s U map f (s x bs) U map f (map f (s x bs)) U ... until fixed point reached.
            iter2 :: Ord a => (a -> b -> a) -> Set a -> Set b -> Set a
            iter2 f s bs = iter2' f s ss bs
                  where ss = S.map (uncurry f) (S.cartesianProduct s bs) S.\\ s

            iter2' :: Ord a => (a -> b -> a) -> Set a -> Set a -> Set b -> Set a
            iter2' f s ss bs = trace (show (S.size s) ++ ": " ++ show (S.size ss) ++ " x " ++ show (S.size bs)) $
                        if S.size ss == 0 then s else iter2' f s' ss' bs
                  where s' = s `S.union` ss
                        ss' = S.map (uncurry f) (S.cartesianProduct ss bs) S.\\ s


--Expand Map (s, a) (B s) to (B s -> a -> B s)
deltaP' :: (AtomicProp s, Eq s, Ord s, Ord a) => (s -> a -> B s) -> B s -> a -> B s
deltaP' delta t a = case t of
     BTrue -> BTrue
     BFalse -> BFalse
     BTerm s -> delta s a
     BAnd b1 b2 -> BAnd (deltaP' delta b1 a) (deltaP' delta b2 a)
     BOr b1 b2 -> BOr (deltaP' delta b1 a) (deltaP' delta b2 a)

type LDFA a = DFA (Set a)

data DFA a = DFA
      { statesDFA :: !Int
      , alphaDFA  :: !(Set a)
      , currDFA   :: !Int
      , finalDFA  :: !(Set Int)
      , deltaDFA  :: !(Map (Int, a) Int)
      }

toDFA :: (AtomicProp s, Ord s, Ord a) => ABA s a -> DFA a
toDFA aba = DFA
      { statesDFA = S.size (statesABA' aba')
      , alphaDFA  = alphaABA' aba'
      , currDFA   = toTag (currABA' aba')
      , finalDFA  = S.map toTag $ S.filter (any (`S.isSubsetOf` finalABA' aba')) (statesABA' aba')
      , deltaDFA  = M.fromAscList $ map (\ ((t, a), v) -> ((toTag t, a), toTag v)) (M.toAscList $ deltaABA' aba')
      }
      where toTag = fromMaybe (-1) . flip S.lookupIndex (statesABA' aba')
            aba'  = toABA' aba
