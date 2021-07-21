module StructuredText.Boolean
      ( B (..), DNF
      , simplify
      , satisfy
      , exists
      , children
      , dnf, injectDNF
      ) where

import Test.QuickCheck (oneof, sized, Arbitrary (..), Gen)
import StructuredText.LTL (AtomicProp (..))
import qualified Data.Set as S
import Data.List (find)
import Data.Set (Set)
import Data.Foldable (fold)

data B s = BTrue
         | BFalse
         | BTerm s
         | BAnd (B s) (B s)
         | BOr (B s) (B s)
     deriving (Show, Ord, Eq)

type DNF s = DnfOr s
type DnfOr s  = Set (DnfAnd s)
type DnfAnd s = Set s

dnfFalse :: Ord a => DNF a
dnfFalse = mempty -- bottom

dnfTrue :: Ord a => DNF a
dnfTrue  = S.singleton mempty -- top

-- | Convert B expr to disjunctive normal form (OR of ANDs).
dnf :: (Ord s, Eq s, AtomicProp s) => B s -> DNF s
dnf = simpl . \ case
      BFalse           -> dnfFalse
      BTrue            -> dnfTrue
      BTerm s          -> S.singleton (S.singleton s)
      BOr a b          -> dnf a <> dnf b
      BAnd a b         -> dnf a `andOrs` dnf b
      where andOrs :: Ord s => DNF s -> DNF s -> DNF s
            andOrs a = fold . S.map (flip S.map a . (<>))

-- | Remove all terms with a constant "true" or "false" value.
simpl :: (Ord s, Eq s, AtomicProp s) => DNF s -> DNF s
simpl = foldr simpl' mempty
      where -- | ORs.
            simpl' :: (Ord s, Eq s, AtomicProp s) => DnfAnd s -> DNF s -> DNF s
            simpl' _ as | as == dnfTrue = dnfTrue
            simpl' a as                 = case S.toList a' of
                  []                             -> dnfTrue
                  [b] | atEval b == Just False   -> as
                  _                              -> S.singleton a' <> as
                  where a' = foldr simpl'' mempty a

            -- | ANDs.
            simpl'' :: (Ord s, Eq s, AtomicProp s) => s -> DnfAnd s -> DnfAnd s
            simpl'' a _  | atEval a == Just False = S.singleton atFalse
            simpl'' a as | atEval a == Just True  = as
            simpl'' a as = case S.toList as of
                  [b] | atEval b == Just False -> as
                  _                            -> S.singleton a <> as

injectDNF :: DNF s -> B s
injectDNF = foldr (BOr . injectAnd) BFalse
      where injectAnd :: DnfAnd s -> B s
            injectAnd = foldr (BAnd . BTerm) BTrue

instance Functor B where
      fmap f = \ case
            BTerm a    -> BTerm $ f a
            BAnd e1 e2 -> BAnd (fmap f e1) (fmap f e2)
            BOr e1 e2  -> BOr  (fmap f e1) (fmap f e2)
            BTrue      -> BTrue
            BFalse     -> BFalse

instance Foldable B where
      foldMap f = \ case
            BTerm a        -> f a
            BAnd e1 e2     -> foldMap f e1 <> foldMap f e2
            BOr e1 e2      -> foldMap f e1 <> foldMap f e2
            _              -> mempty

instance Applicative B where
      pure = BTerm
      BTerm f    <*> a = fmap f a
      BAnd e1 e2 <*> a = BAnd (e1 <*> a) (e2 <*> a)
      BOr e1 e2  <*> a = BOr (e1 <*> a) (e2 <*> a)
      BTrue      <*> _ = BTrue  -- TODO think about these cases
      BFalse     <*> _ = BFalse -- TODO

instance Monad B where
      BTerm a >>= f    = f a
      BAnd e1 e2 >>= f = BAnd (e1 >>= f) (e2 >>= f)
      BOr e1 e2 >>= f  = BOr (e1 >>= f) (e2 >>= f)
      BTrue >>= _      = BTrue  -- TODO think about these cases
      BFalse >>= _     = BFalse -- TODO

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

simplify :: (AtomicProp s, Eq s, Ord s) => B s -> B s
simplify = injectDNF . dnf

--does the set satisfy the boolean formula?
satisfy :: (Ord s) => B s -> Set s -> Bool
satisfy formula set = case formula of
     BTrue          -> True
     BFalse         -> False
     BTerm b'       -> S.member b' set
     BAnd b1 b2     -> satisfy b1 set && satisfy b2 set
     BOr b1 b2      -> satisfy b1 set || satisfy b2 set

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

