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
import GHC.TypeLits (Symbol, TypeError(..), ErrorMessage(..))

import Data.Array.Storable as St
import Data.Array.Unboxed as U
import Data.ByteString
import Data.Kind
import Data.Map.Strict
import Data.Hashable
import Data.HashMap.Strict
import qualified Data.HashSet as C
import qualified Data.Set as C
import qualified Data.Text
import Data.Type.Bool
import Data.Vector.Primitive as P
import Data.Vector.Storable as St
import Data.Vector.Unboxed as U
import qualified Foundation as F

type family StrictRep (rec :: [*]) (a :: * -> *) :: Constraint where
  StrictRep rec (M1 c (MetaData _ _ _ isNewtype) f) = StrictData rec isNewtype f

type family StrictData (rec :: [*]) (isNewtype :: Bool) (a :: * -> *) :: Constraint where
  StrictData rec isNewtype (C1 (MetaCons c _ _) f) = StrictCons rec isNewtype c f
  StrictData rec isNewtype (f :+: g) = (StrictData rec isNewtype f, StrictData rec isNewtype g)
  StrictData rec isNewtype (f :*: g) = (StrictData rec isNewtype f, StrictData rec isNewtype g)
  StrictData rec isNewtype (f :.: g) = (StrictData rec isNewtype f, StrictData rec isNewtype g)

type family StrictCons (rec :: [*]) (isNewtype :: Bool) (cons :: Symbol) (a :: * -> *) :: Constraint where
  StrictCons rec True      cons (M1 c meta f) = StrictField rec f
  StrictCons rec False     cons (M1 c meta f) = (StrictSel rec cons meta, StrictField rec f)
  StrictCons rec isNewtype cons (f :*: g)     = (StrictCons rec isNewtype cons f, StrictCons rec isNewtype cons g)
  StrictCons rec isNewtype cons field         = StrictField rec field

type family StrictField  (rec :: [*]) (a :: * -> *) :: Constraint where
  StrictField rec V1 = ()
  StrictField rec U1 = ()
  StrictField rec (K1 _ t) = StrictCond rec (Elem t rec) t
  StrictField rec (URec t) = StrictCond rec (Elem t rec) t

type family StrictCond rec (cond :: Bool) t :: Constraint where
  StrictCond rec True  t = ()
  StrictCond rec False t = StrictType (t : rec) t

type family StrictSel (typ :: [*]) (cons :: Symbol) (m :: Meta) :: Constraint where
  StrictSel rec cons (MetaSel mn su ss ds) = IsDecidedStrict rec cons mn ds

class IsDecidedStrict (t:: [*]) (cons :: Symbol) (field :: Maybe Symbol) (a :: DecidedStrictness)
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
--   Requires undecidable instances. Experimental (and inefficient) support for mutually recursive groups of types.
type family Strict a :: Constraint where
  Strict d = StrictType '[d] d

type family StrictType (rec :: [*]) d :: Constraint where
  -- Primitives
  StrictType rec Char = ()
  StrictType rec Double = ()
  StrictType rec Int = ()
  StrictType rec Integer = ()
  StrictType rec Word = ()
  -- StrictType rec Data
  StrictType rec ByteString = ()
  StrictType rec Data.Text.Text = ()
  StrictType rec F.String = ()
  StrictType rec (Hashed a) = StrictType rec a
  -- StrictType rec Containers
  StrictType rec (UArray ix v) = ()
  StrictType rec (StorableArray ix v) = ()
  StrictType rec (Map k v) = (StrictType rec k, StrictType rec v)
  StrictType rec (HashMap k v) = (StrictType rec k, StrictType rec v)
  StrictType rec (C.Set k) = StrictType rec k
  StrictType rec (C.HashSet k) = StrictType rec k
  StrictType rec (U.Vector a) = ()
  StrictType rec (U.MVector s a) = ()
  StrictType rec (St.Vector a) = ()
  StrictType rec (St.MVector s a) = ()
  StrictType rec (P.Vector a) = ()
  StrictType rec (P.MVector s a) = ()
  -- Generics
  StrictType rec d = StrictRep (d : rec) (Rep d)

-- | A pattern that matches strict types
pattern IsStrict :: Strict a => a -> a
pattern IsStrict a = a

type family Elem a (aa :: [*]) where
  Elem a '[] = False
  Elem a (a : _ ) = True
  Elem a (_ : aa) = Elem a aa
