{-# LANGUAGE GADTs #-}
module StructuredText.Boolean
      ( B
      , satisfies
      , dnfFalse, dnfTrue, dnfAnd, dnfOr, dnfTerm
      , toList
      ) where

import StructuredText.LTL (AtomicProp (..))
import qualified Data.Set as S
import Data.Set (Set)
import Data.Foldable (fold)

-- | Based on Oleg's http://okmij.org/ftp/Haskell/set-monad.html
data B a where
      BOrd :: Ord a => DNF a -> B a
      BAny :: [[a]] -> B a

type DNF s = Set (Set s)

-- | Version without Ord constraint (so result might not be normalized).
toList' :: B s -> [[s]]
toList' = \ case
      BOrd s -> S.toAscList (S.map S.toAscList s)
      BAny s -> s

-- | Version with Ord constraint.
toList :: Ord s => B s -> [[s]]
toList = \ case
      BOrd s -> S.toAscList (S.map S.toAscList s)
      BAny s -> toList (fromList s)

fromList :: Ord s => [[s]] -> B s
fromList = foldr (dnfOr . BOrd . S.singleton . S.fromList) dnfFalse

collect :: [[B s]] -> B s
collect = foldr (dnfOr . foldr dnfAnd dnfTrue') dnfFalse'

dnfFalse :: Ord s => B s
dnfFalse = BOrd mempty

dnfFalse' :: B s
dnfFalse' = BAny []

dnfTrue :: Ord s => B s
dnfTrue  = BOrd $ S.singleton mempty

dnfTrue' :: B s
dnfTrue'  = BAny [[]]

isFalse :: B s -> Bool
isFalse a = case toList' a of
      [] -> True
      _  -> False

isTrue :: B s -> Bool
isTrue a = case toList' a of
      [[]] -> True
      _    -> False

dnfOr :: B s -> B s -> B s
dnfOr a@(BOrd _) b | isTrue a || isTrue b = dnfTrue
dnfOr a b@(BOrd _) | isTrue a || isTrue b = dnfTrue
dnfOr a b          | isTrue a || isTrue b = dnfTrue'
dnfOr (BOrd a) (BOrd b)          = BOrd $ a <> b
dnfOr a@(BOrd _) (BAny b)        = dnfOr a $ fromList b
dnfOr (BAny a)     b@(BOrd _)    = dnfOr (fromList a) b
dnfOr a          b               = BAny $ toList' a <> toList' b

dnfAnd :: B s -> B s -> B s
dnfAnd a@(BOrd _) b | isFalse a || isFalse b = dnfFalse
dnfAnd a b@(BOrd _) | isFalse a || isFalse b = dnfFalse
dnfAnd a b          | isFalse a || isFalse b = dnfFalse'
dnfAnd (BOrd a)   (BOrd b)          = BOrd (dnfAnd' a b)
      where dnfAnd' :: Ord a => DNF a -> DNF a -> DNF a
            dnfAnd' s = fold . S.map (flip S.map s . (<>))
dnfAnd a@(BOrd _) (BAny b)          = dnfAnd a (fromList b)
dnfAnd (BAny a) b@(BOrd _)          = dnfAnd (fromList a) b
dnfAnd a b                          = BAny $ dnfAnd' (toList' a) (toList' b)
      where dnfAnd' :: [[s]] -> [[s]] -> [[s]]
            dnfAnd' s = foldMap (flip map s . (<>))

dnfTerm :: (AtomicProp a, Ord a) => a -> B a
dnfTerm = \ case
      a | atEval a == Just True  -> dnfTrue
      a | atEval a == Just False -> dnfFalse
      a                          -> BOrd $ S.singleton $ S.singleton a

-- | Does the set satisfy the boolean formula?
satisfies :: Ord s => Set s -> B s -> Bool
satisfies s = \ case
      BOrd b -> any (`S.isSubsetOf` s) b
      BAny b -> satisfies s $ fromList b

instance Eq s => Eq (B s) where
      a == b = toList' a == toList' b
instance Ord s => Ord (B s) where
      a <= b = toList' a <= toList' b
instance Semigroup (B s) where
      (<>) = dnfOr
instance Monoid (B s) where
      mempty = BAny []
instance Functor B where
      fmap f = BAny . map (map f) . toList'
instance Foldable B where
      foldMap f (BOrd b) = foldMap (foldMap f) b
      foldMap f (BAny b) = foldMap (foldMap f) b
instance Applicative B where
      pure    = BAny . pure . pure
      a <*> b = collect $ toList' $ fmap (`fmap` b) a
instance Monad B where
      a >>= b = collect $ toList' $ fmap b a
