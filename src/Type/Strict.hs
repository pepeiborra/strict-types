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
--   A type T is strict if @forall x :: T . rnf x = \bot <=> rwhnf x = \bot
module Type.Strict
  ( Strict
  , pattern IsStrict
  ) where

import GHC.Exts
import GHC.Generics
import GHC.TypeLits

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

type family StrictField  (d :: *) (a :: * -> *) :: Constraint where
  StrictField d V1 = ()
  StrictField d U1 = ()
  StrictField d (K1 i d) = ()
  StrictField d (K1 i c) = Strict c
  StrictField d (URec a) = Strict a

type family StrictSel (typ :: *) (cons :: Symbol) (m :: Meta) :: Constraint where
  StrictSel d cons (MetaSel mn su ss ds) = IsDecidedStrict d cons mn ds

class IsDecidedStrict (t:: *) (cons :: Symbol) (field :: Maybe Symbol) (a :: DecidedStrictness)
instance IsDecidedStrict t c f DecidedStrict
instance IsDecidedStrict t c f DecidedUnpack
instance TypeError (ShowType t :<>: Text " has an unnamed lazy field in constructor " :<>: Text c) =>
         IsDecidedStrict t c Nothing DecidedLazy
instance TypeError (ShowType t :<>: Text " has a lazy field " :<>: Text f :<>: Text " in constructor " :<>: Text c) =>
         IsDecidedStrict t c (Just f) DecidedLazy

-- | A closed constraint that uses GHC Generics to decide if a type is strict.
--   Requires undecidable instances. Mutually recursive groups of types not yet supported.
type family Strict (d :: *) :: Constraint where
  Strict Int = ()
  Strict Double = ()
  Strict d = StrictRep d (Rep d)

pattern IsStrict :: Strict a => a -> a
pattern IsStrict a = a
