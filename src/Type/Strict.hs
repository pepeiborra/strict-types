{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Type constraints and patterns for strict types.
module Type.Strict
  ( Strict
  , pattern IsStrict
  ) where

import GHC.Exts
import GHC.Generics
import GHC.TypeLits

import Data.Array.Storable as St
import Data.Array.Unboxed as U
import Data.ByteString
import Data.Map.Strict
import Data.Hashable
import Data.HashMap.Strict
import Data.HashSet
import Data.Set
import qualified Data.Text
import Data.Vector.Primitive as P
import Data.Vector.Storable as St
import Data.Vector.Unboxed as U
import qualified Foundation as F

type family StrictRep (d :: *) (a :: * -> *) :: Constraint where
  StrictRep d (M1 c (MetaData _ _ _ isNewtype) f) = StrictType isNewtype d f

type family StrictType (isNewtype :: Bool) (typ :: *) (a :: * -> *) :: Constraint where
  StrictType isNewtype typ (C1 (MetaCons c _ _) f) = StrictCons isNewtype typ c f
  StrictType isNewtype d (f :+: g) = (StrictType isNewtype d f, StrictType isNewtype d g)
  StrictType isNewtype d (f :*: g) = (StrictType isNewtype d f, StrictType isNewtype d g)
  StrictType isNewtype d (f :.: g) = (StrictType isNewtype d f, StrictType isNewtype d g)

type family StrictCons (isNewtype :: Bool) (typ :: *) (cons :: Symbol) (a :: * -> *) :: Constraint where
  StrictCons True      d cons (M1 c meta f) = StrictField d f
  StrictCons isNewtype d cons (M1 c meta f) = (StrictSel d cons meta, StrictField d f)
  StrictCons isNewtype d cons field = StrictField d field

type family StrictField  (rec :: *) (a :: * -> *) :: Constraint where
  StrictField rec V1 = ()
  StrictField rec U1 = ()
  StrictField rec (K1 i rec) = ()
  StrictField rec (K1 i c) = Strict c
  StrictField rec (URec a) = Strict a

type family StrictSel (typ :: *) (cons :: Symbol) (m :: Meta) :: Constraint where
  StrictSel d cons (MetaSel mn su ss ds) = IsDecidedStrict d cons mn ds

class IsDecidedStrict (t:: *) (cons :: Symbol) (field :: Maybe Symbol) (a :: DecidedStrictness)
instance IsDecidedStrict t c f DecidedStrict
instance IsDecidedStrict t c f DecidedUnpack
instance TypeError (ShowType t :<>: Text " has an unnamed lazy field in constructor " :<>: Text c) =>
         IsDecidedStrict t c Nothing DecidedLazy
instance TypeError (ShowType t :<>: Text " has a lazy field " :<>: Text f :<>: Text " in constructor " :<>: Text c) =>
         IsDecidedStrict t c (Just f) DecidedLazy

-- | A closed predicate that is satisfied only by strict types.
-- 
--   A type T is strict if
--
--   > ∀x :: T . rnf x = ⊥ <=> rwhnf x = ⊥
--
--   Requires undecidable instances. Mutually recursive groups of types not yet supported.
type family Strict (d :: *) :: Constraint where
  -- Primitives
  Strict Char = ()
  Strict Double = ()
  Strict Int = ()
  Strict Integer = ()
  Strict Word = ()
  -- Strict Data
  Strict ByteString = ()
  Strict Data.Text.Text = ()
  Strict F.String = ()
  Strict (Hashed a) = Strict a
  -- Strict Containers
  Strict (UArray ix v) = ()
  Strict (StorableArray ix v) = ()
  Strict (Map k v) = (Strict k, Strict v)
  Strict (HashMap k v) = (Strict k, Strict v)
  Strict (Set k) = Strict k
  Strict (HashSet k) = Strict k
  Strict (U.Vector a) = ()
  Strict (U.MVector s a) = ()
  Strict (St.Vector a) = ()
  Strict (St.MVector s a) = ()
  Strict (P.Vector a) = ()
  Strict (P.MVector s a) = ()
  -- Generics
  Strict d = StrictRep d (Rep d)

-- | A pattern that matches strict types
pattern IsStrict :: Strict a => a -> a
pattern IsStrict a = a
