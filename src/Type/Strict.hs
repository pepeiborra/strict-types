{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS -Wno-unticked-promoted-constructors #-}
-- | Type constraints and patterns for strict types.
module Type.Strict
  ( pattern Strict
  , pattern Rnf
  , Strict
  , StrictType
  ) where

import GHC.Exts
import GHC.Generics
import GHC.TypeLits (Symbol, TypeError, ErrorMessage(..))

import Control.DeepSeq as D
import Data.Array.Storable as St
import Data.Array.Unboxed as U
import Data.ByteString
import Data.Int
import Data.Hashable
import qualified Data.HashSet as C
import qualified Data.Set as C
import qualified Data.Text
import Data.Vector.Primitive as P
import Data.Vector.Storable as St
import Data.Vector.Unboxed as U
import Data.Word

-- Generic rep strictness checker
type family StrictRep (rec :: [*]) (a :: * -> *) :: Constraint where
  StrictRep rec (M1 c (MetaData _ _ _ isNewtype) f) = StrictData rec isNewtype f

-- Datatype equations
type family StrictData (rec :: [*]) (isNewtype :: Bool) (a :: * -> *) :: Constraint where
  StrictData rec isNewtype (C1 (MetaCons c _ _) f) = StrictCons rec isNewtype c f
  StrictData rec isNewtype (f :+: g) = (StrictData rec isNewtype f, StrictData rec isNewtype g)
  StrictData rec isNewtype (f :*: g) = (StrictData rec isNewtype f, StrictData rec isNewtype g)
  StrictData rec isNewtype (f :.: g) = (StrictData rec isNewtype f, StrictData rec isNewtype g)

-- Constructor equations
type family StrictCons (rec :: [*]) (isNewtype :: Bool) (cons :: Symbol) (a :: * -> *) :: Constraint where
  StrictCons rec True      cons (M1 c meta f) = StrictField rec f
  StrictCons rec False     cons (M1 c meta f) = (StrictSel rec cons meta, StrictField rec f)
  StrictCons rec isNewtype cons (f :*: g)     = (StrictCons rec isNewtype cons f, StrictCons rec isNewtype cons g)
  StrictCons rec isNewtype cons field         = StrictField rec field

-- Field equations
type family StrictField  (rec :: [*]) (a :: * -> *) :: Constraint where
  StrictField rec V1 = ()
  StrictField rec U1 = ()
  StrictField rec (K1 _ t) = StrictCond rec (Elem t rec) t
  StrictField rec (URec t) = StrictCond rec (Elem t rec) t

-- | Field strictness checker. Tries to spot recursion.
type family StrictCond rec (cond :: Bool) t :: Constraint where
  StrictCond rec True  t = ()
  StrictCond rec False t = StrictType (t : rec) t

-- | Field metadata checker
type family StrictSel (typ :: [*]) (cons :: Symbol) (m :: Meta) :: Constraint where
  StrictSel rec cons (MetaSel mn su ss ds) = IsDecidedStrict rec cons mn ds

class IsDecidedStrict (t:: [*]) (cons :: Symbol) (field :: Maybe Symbol) (a :: DecidedStrictness)
instance IsDecidedStrict t c f DecidedStrict
instance IsDecidedStrict t c f DecidedUnpack
instance TypeError (ShowType t :<>: Text " has an unnamed lazy field in constructor " :<>: Text c) =>
         IsDecidedStrict (t : tt) c Nothing DecidedLazy
instance TypeError (ShowType t :<>: Text " has a lazy field " :<>: Text f :<>: Text " in constructor " :<>: Text c) =>
         IsDecidedStrict (t : tt) c (Just f) DecidedLazy

-- | A constraint that is satisfied only by strict types.
--
--   A type T is strict if
--
--   > ∀x :: T . rnf x = ⊥ <=> rwhnf x = ⊥
--
--   Mutually recursive types are experimentally supported but non regular types are not.
type family Strict a :: Constraint where
  Strict d = StrictType '[d] d

-- | An empty class to constrain strict types.
--   An instance is an unchecked promise that a type is fully strict.
--   No instances are needed for generic non-nested datatypes.
--
--   Define an instance as follows:
--
--   > instance StrictType seen Foo
--
class StrictType (seen :: [*]) a
instance StrictType seen Char
instance StrictType seen Double
instance StrictType seen Int
instance StrictType seen Int8
instance StrictType seen Int16
instance StrictType seen Int32
instance StrictType seen Int64
instance StrictType seen Integer
instance StrictType seen Word
instance StrictType seen Word8
instance StrictType seen Word16
instance StrictType seen Word32
instance StrictType seen Word64
  -- Data
instance StrictType seen ByteString
instance StrictType seen Data.Text.Text
instance StrictType seen a => StrictType seen (Hashed a)
  -- rec Containers
instance StrictType seen (UArray ix v)
instance StrictType seen (StorableArray ix v)
instance StrictType seen k => StrictType seen (C.Set k)
instance StrictType seen k => StrictType seen (C.HashSet k)
instance StrictType seen (U.Vector a)
instance StrictType seen (U.MVector s a)
instance StrictType seen (St.Vector a)
instance StrictType seen (St.MVector s a)
instance StrictType seen (P.Vector a)
instance StrictType seen (P.MVector s a)
  -- Generics
instance {-# OVERLAPPABLE #-} StrictRep (d : seen) (Rep d) => StrictType seen d

-- | A pattern that matches 'Strict' types only
pattern Strict :: Strict a => a -> a
pattern Strict a = a

-- | A pattern that matches 'NFData' types only and forces values to rigid normal form
pattern Rnf :: NFData a => a -> a
pattern Rnf a <- (D.force -> !a) where Rnf a = D.force a

type family Elem a (aa :: [*]) where
  Elem a '[] = False
  Elem a (a : _ ) = True
  Elem a (_ : aa) = Elem a aa
