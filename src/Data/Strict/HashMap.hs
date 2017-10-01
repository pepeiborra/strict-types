{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A wrapper around 'H.HashMap' from the unordered-containers package,
--   with the aim of fixing the strictness of the type class operations,
--   to obtain a fully strict (observationally) data structure.
module Data.Strict.HashMap
    (HashMap(..)
    , adjust
    , alter
    , delete
    , difference
    , differenceWith
    , elems
    , empty
    , filter
    , filterWithKey
    , Data.Strict.HashMap.fromList
    , fromListWith
    , foldl'
    , foldlWithKey'
    , foldr
    , foldrWithKey
    , insert
    , insertWith
    , intersection
    , intersectionWith
    , intersectionWithKey
    , keys
    , lookup
    , lookupDefault
    , map
    , mapWithKey
    , mapMaybe
    , mapMaybeWithKey
    , member
    , null
    , size
    , singleton
    , Data.Strict.HashMap.toList
    , traverseWithKey
    , union
    , unions
    , unionWith
    , unionWithKey
    , update
    , (!)
    )where

import Prelude hiding (filter, foldr, lookup, map, null)
import Control.DeepSeq
import Data.Bifunctor
import Data.Data
import Data.Functor.Classes
import Data.Hashable
import Data.Hashable.Lifted
import qualified Data.HashMap.Strict as H
import Data.Semigroup
import GHC.Exts
import Type.Strict

newtype HashMap k a = Strict { getStrict :: H.HashMap k a}
  deriving (Data, NFData, Eq, Show, Eq1, Show1, Eq2, Show2, Hashable, Hashable1, Hashable2, Foldable, Semigroup, Monoid)

instance Functor (HashMap k) where
  fmap f = lift1( H.map f)

instance (Eq k, Hashable k, Read k, Read a) => Read (HashMap k a) where
  readsPrec p s = first (Strict . H.map id) <$> readsPrec p s

instance (Eq k, Hashable k, Read k) => Read1 (HashMap k) where
  liftReadsPrec rp rl p s = first (Strict . H.map id) <$> liftReadsPrec rp rl p s

instance Traversable (HashMap k) where
  traverse f = traverseWithKey (const f)

instance (StrictType seen k, StrictType seen v) => StrictType seen (HashMap k v)

instance (Eq k, Hashable k) => IsList (HashMap k v) where
  type Item (HashMap k v) = (k, v)
  fromList = Strict . H.fromList
  toList = H.toList . getStrict

--------------------------------------------------------------------------------------------------------------------------

delete :: (Hashable k2, Eq k2) => k2 -> HashMap k2 a2 -> HashMap k2 a2
delete k = lift1 (H.delete k)
difference :: (Hashable k3, Eq k3) => HashMap k3 a3 -> HashMap k3 w -> HashMap k3 a3
difference = lift2 H.difference
elems :: HashMap k v -> [v]
elems = H.elems . getStrict
empty :: HashMap k a
empty = Strict H.empty
filter :: (a2 -> Bool) -> HashMap k2 a2 -> HashMap k2 a2
filter f = lift1 (H.filter f)
filterWithKey :: (k2 -> a2 -> Bool) -> HashMap k2 a2 -> HashMap k2 a2
filterWithKey f = lift1 (H.filterWithKey f)
foldl' :: (H.HashMap k2 a2 -> v -> H.HashMap k2 a2) -> H.HashMap k2 a2 -> HashMap k v -> HashMap k2 a2
foldl' f a = lift1 (H.foldl' f a)
foldrWithKey :: (k -> v -> H.HashMap k2 a2 -> H.HashMap k2 a2) -> H.HashMap k2 a2 -> HashMap k v -> HashMap k2 a2
foldrWithKey f a = lift1 (H.foldrWithKey f a)
foldr :: (v -> H.HashMap k2 a2 -> H.HashMap k2 a2) -> H.HashMap k2 a2 -> HashMap k v -> HashMap k2 a2
foldr f a = lift1 (H.foldr f a)
foldlWithKey' :: (H.HashMap k2 a2 -> k -> v -> H.HashMap k2 a2) -> H.HashMap k2 a2 -> HashMap k v -> HashMap k2 a2
foldlWithKey' f a = lift1 (H.foldlWithKey' f a)
intersection :: (Hashable k3, Eq k3) => HashMap k3 a3 -> HashMap k3 w -> HashMap k3 a3
intersection = lift2 H.intersection
intersectionWith :: (Hashable k3, Eq k3) => (v1 -> v2 -> a3) -> HashMap k3 v1 -> HashMap k3 v2 -> HashMap k3 a3
intersectionWith f = lift2 (H.intersectionWith f)
intersectionWithKey :: (Hashable k3, Eq k3) => (k3 -> v1 -> v2 -> a3) -> HashMap k3 v1 -> HashMap k3 v2 -> HashMap k3 a3
intersectionWithKey f = lift2 (H.intersectionWithKey f)
keys :: HashMap k v -> [k]
keys = H.keys . getStrict
lookup :: (Hashable k, Eq k) => k -> HashMap k v -> Maybe v
lookup k = H.lookup k . getStrict
lookupDefault :: (Hashable k, Eq k) => c -> k -> HashMap k c -> c
lookupDefault v k = H.lookupDefault v k . getStrict
member :: (Hashable k, Eq k) => k -> HashMap k a -> Bool
member k = H.member k . getStrict
null :: HashMap k v -> Bool
null = H.null . getStrict
size :: HashMap k v -> Int
size = H.size . getStrict
toList :: HashMap k v -> [(k, v)]
toList = H.toList . getStrict
traverseWithKey :: Applicative f => (k -> v1 -> f a) -> HashMap k v1 -> f (HashMap k a)
traverseWithKey f = fmap (map id . Strict) . H.traverseWithKey f . getStrict
union :: (Hashable k3, Eq k3) => HashMap k3 a3 -> HashMap k3 a3 -> HashMap k3 a3
union = lift2 H.union
unions :: (Hashable k, Eq k) => [HashMap k a] -> HashMap k a
unions = Strict . H.unions . fmap getStrict
adjust :: (Hashable k2, Eq k2) => (a2 -> a2) -> k2 -> HashMap k2 a2 -> HashMap k2 a2
adjust f k = lift1 (H.adjust f k)
alter :: (Hashable k2, Eq k2) => (Maybe a2 -> Maybe a2) -> k2 -> HashMap k2 a2 -> HashMap k2 a2
alter f k  = lift1 (H.alter  f k)
differenceWith :: (Hashable k3, Eq k3) => (a3 -> w -> Maybe a3) -> HashMap k3 a3 -> HashMap k3 w -> HashMap k3 a3
differenceWith f = lift2 (H.differenceWith f)
fromList :: (Hashable k, Eq k) => [(k, a)] -> HashMap k a
fromList = Strict . H.fromList
fromListWith :: (Hashable k, Eq k) => (a -> a -> a) -> [(k, a)] -> HashMap k a
fromListWith f = Strict .H.fromListWith f
insert :: (Hashable k2, Eq k2) => k2 -> a2 -> HashMap k2 a2 -> HashMap k2 a2
insert k v = lift1 (H.insert k v)
insertWith :: (Hashable k2, Eq k2) => (a2 -> a2 -> a2) -> k2 -> a2 -> HashMap k2 a2 -> HashMap k2 a2
insertWith f k v = lift1 (H.insertWith f k v)
map :: (v1 -> a2) -> HashMap k2 v1 -> HashMap k2 a2
map f = lift1 (H.map f)
mapWithKey :: (k2 -> v1 -> a2) -> HashMap k2 v1 -> HashMap k2 a2
mapWithKey f = lift1 (H.mapWithKey f)
mapMaybe :: (v1 -> Maybe a2) -> HashMap k2 v1 -> HashMap k2 a2
mapMaybe f = lift1 (H.mapMaybe f)
mapMaybeWithKey :: (k2 -> v1 -> Maybe a2) -> HashMap k2 v1 -> HashMap k2 a2
mapMaybeWithKey f = lift1 (H.mapMaybeWithKey f)
singleton :: Hashable k => k -> a -> HashMap k a
singleton = (Strict.) . H.singleton
unionWith :: (Hashable k3, Eq k3) => (a3 -> a3 -> a3) -> HashMap k3 a3 -> HashMap k3 a3 -> HashMap k3 a3
unionWith f = lift2 (H.unionWith f)
unionWithKey :: (Hashable k3, Eq k3) => (k3 -> a3 -> a3 -> a3) -> HashMap k3 a3 -> HashMap k3 a3 -> HashMap k3 a3
unionWithKey f = lift2 (H.unionWithKey f)
update :: (Hashable k2, Eq k2) => (a2 -> Maybe a2) -> k2 -> HashMap k2 a2 -> HashMap k2 a2
update f k = lift1 (H.update f k)
(!) :: (Hashable k, Eq k) => HashMap k v -> k -> v
Strict m ! k = m H.! k

--------------------------------------

lift1 :: (H.HashMap k1 a1 -> H.HashMap k2 a2) -> HashMap k1 a1 -> HashMap k2 a2
lift1 f = Strict . f . getStrict
lift2 :: (H.HashMap k1 a1 -> H.HashMap k2 a2 -> H.HashMap k3 a3) -> HashMap k1 a1 -> HashMap k2 a2 -> HashMap k3 a3
lift2 f (Strict a) (Strict b) = Strict (f a b)
