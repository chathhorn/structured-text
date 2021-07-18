module StructuredText.Boolean
      ( B (..)
      , simplify
      , satisfy
      , exists
      , children
      , satSet
      , dnf, injectDNF
      ) where

import Test.QuickCheck (oneof, sized, Arbitrary (..), Gen)
import StructuredText.LTL (AtomicProp (..))
import qualified Data.Set as S
import Data.List (find)
import Data.Set (Set)

data B s = BTrue
         | BFalse
         | BTerm s
         | BAnd (B s) (B s)
         | BOr (B s) (B s)
     deriving (Show, Ord, Eq)

data BOr' s = BFalse' | BOr' (BAnd' s) (BOr' s)
      deriving (Show, Ord, Eq)

bor :: BOr' s -> BOr' s -> BOr' s
bor BFalse' a               = a
bor a BFalse'               = a
bor (BOr' s a) (BOr' s' a') = BOr' s $ BOr' s' (bor a a')

data BAnd' s = BTrue' | BAnd' s (BAnd' s)
      deriving (Show, Ord, Eq)

instance Foldable BAnd' where
      foldr f e (BAnd' s b) = f s (foldr f e b)
      foldr _ e _           = e

band :: BAnd' s -> BAnd' s -> BAnd' s
band BTrue' a                  = a
band a BTrue'                  = a
band (BAnd' s a) (BAnd' s' a') = BAnd' s $ BAnd' s' (band a a')

-- | Convert B expr to disjunctive normal form (OR of ANDs).
dnf :: (Eq s, AtomicProp s) => B s -> BOr' s
dnf = simpl . \ case
      BFalse           -> BFalse'
      BTrue            -> BOr' BTrue' BFalse'
      BTerm s          -> BOr' (BAnd' s BTrue') BFalse'
      BOr a b          -> dnf a `bor` dnf b
      BAnd a b         -> dnf a `bandOO` dnf b
      where bandO :: BAnd' s -> BOr' s -> BOr' s
            bandO a BFalse'    = BFalse'
            bandO a (BOr' b c) = BOr' (a `band` b) (a `bandO` c)

            bandOO :: BOr' s -> BOr' s -> BOr' s
            bandOO a BFalse'    = BFalse'
            bandOO a (BOr' b c) = (b `bandO` a) `bor` (a `bandOO` c)

-- | Simplify without AtomicProp constraint.
simplSurface :: Eq s => BOr' s -> BOr' s
simplSurface = \ case
      BFalse'                      -> BFalse'
      BOr' a _
            | a == BTrue'          -> BOr' BTrue' BFalse'
      BOr' a b                     -> case simplSurface b of
            BOr' a' _ | a' == BTrue' -> BOr' BTrue' BFalse'
            b'                       -> BOr' a b'

simpl :: (Eq s, AtomicProp s) => BOr' s -> BOr' s
simpl = \ case
      BFalse'                      -> BFalse'
      BOr' a b
            | bandFalse a          -> simpl b
      BOr' a _
            | bandTrue a           -> BOr' BTrue' BFalse'
      BOr' a b                     -> case simpl b of
            BOr' a' _ | bandTrue a' -> BOr' BTrue' BFalse'
            b'                      -> BOr' (simpl' a) b'

      where bandTrue :: (Eq s, AtomicProp s) => BAnd' s -> Bool
            bandTrue b = simpl' b == BTrue'

            bandFalse :: AtomicProp s => BAnd' s -> Bool
            bandFalse b = case simpl' b of
                  BAnd' a BTrue' -> atEval a == Just False
                  _              -> False

            simpl' :: AtomicProp s => BAnd' s -> BAnd' s
            simpl' = \ case
                  BTrue'                         -> BTrue'
                  BAnd' a b
                        | atEval a == Just True  -> simpl' b
                  BAnd' a _
                        | atEval a == Just False -> BAnd' atFalse BTrue'
                  BAnd' a b                      -> case simpl' b of
                        BAnd' a' _ | atEval a' == Just False -> BAnd' atFalse BTrue'
                        b'                                   -> BAnd' a b'

satSet :: (AtomicProp s, Ord s) => B s -> Set (Set s)
satSet = S.map (foldr S.insert mempty) . bands . dnf
      where bands :: Ord s => BOr' s -> Set (BAnd' s)
            bands BFalse'    = mempty
            bands (BOr' a b) = a `S.insert` bands b

unSatSet :: (Ord s, AtomicProp s) => Set (Set s) -> B s
unSatSet = injectDNF . simpl . toBOr'
      where toBOr' :: Set (Set s) -> BOr' s
            toBOr' = S.foldr (BOr' . (S.foldr BAnd' BTrue')) BFalse'

injectDNF :: BOr' s -> B s
injectDNF = \ case
      BFalse'  -> BFalse
      BOr' a b -> BOr (injectBAnd' a) (injectDNF b)
      where injectBAnd' :: BAnd' s -> B s
            injectBAnd' = \ case
                  BTrue'    -> BTrue
                  BAnd' a b -> BAnd (BTerm a) (injectBAnd' b)

instance Foldable B where
      foldMap f = \ case
            BTerm a        -> f a
            BAnd e1 e2     -> foldMap f e1 <> foldMap f e2
            BOr e1 e2      -> foldMap f e1 <> foldMap f e2
            _              -> mempty

instance AtomicProp s => AtomicProp (B s) where
      atTrue  = BTrue
      atFalse = BFalse
      atNot   = \ case
            BTrue      -> BFalse
            BFalse     -> BTrue
            BTerm e    -> BTerm $ atNot e
            BAnd e1 e2 -> BOr (atNot e1) (atNot e2)
            BOr e1 e2  -> BAnd (atNot e1) (atNot e2)
      atEval = \ case
            BTrue      -> Just True
            BFalse     -> Just False
            BTerm e    -> atEval e
            BAnd e1 e2 -> case (atEval e1, atEval e2) of
                  (Just False, _) -> Just False
                  (_, Just False) -> Just False
                  (Just True, a)  -> a
                  (a, Just True)  -> a
                  _               -> Nothing
            BOr e1 e2  -> case (atEval e1, atEval e2) of
                  (Just True, _)  -> Just True
                  (_, Just True)  -> Just True
                  (Just False, a) -> a
                  (a, Just False) -> a
                  _               -> Nothing

instance Arbitrary (B s) where
     arbitrary = sized arbB

arbB :: Int -> Gen (B s)
arbB 0 = oneof [pure BTrue, pure BFalse]
arbB n = do
     a <- arbB (n - 1)
     b <- arbB (n - 1)
     oneof [pure (BAnd a b), pure (BOr a b)]

simplify :: (AtomicProp s, Eq s) => B s -> B s
simplify = injectDNF . dnf

--does the set satisfy the boolean formula?
satisfy :: (Ord s) => B s -> Set s -> Bool
satisfy formula set = case formula of
     BTrue          -> True
     BFalse         -> False
     BTerm b'       -> S.member b' set
     BAnd b1 b2     -> (satisfy b1 set) && (satisfy b2 set)
     BOr b1 b2      -> (satisfy b1 set) || (satisfy b2 set)

--does there exist an element of set satisfying condition?
exists :: (Eq s) => (s -> Bool) -> Set s -> Bool
exists condition set | find condition set == Nothing = False
                     | otherwise                     = True

--is the given formula satisfied by any subset of the atoms?
satisfied :: (AtomicProp s, Ord s) => B s -> Set s -> Bool
satisfied formula atoms = exists (satisfy (simplify formula)) (S.powerSet atoms)

--what are all subsets of atoms that satisfy the formula?
children :: (AtomicProp s, Ord s) => B s -> Set s -> Set (Set s)
children formula atoms = S.filter (satisfy (simplify formula)) (S.powerSet atoms)

