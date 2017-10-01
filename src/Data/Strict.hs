{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
module Data.Strict
    ( Rnf (Rnf, getRnf)
    , Data.Strict.map
    , Data.Strict.traverse
    , (<!>)
    ) where

import Control.Arrow
import Control.DeepSeq
import Data.Hashable
import GHC.Exts
import GHC.Float

-- | A newtype to enforce rigid normal form evaluation.
newtype Rnf a = Rnf_ a
  deriving ( Eq
             , Ord
             , Show
             , Hashable
             , Foldable
             )

-- | A pattern constructor that forces its contents to 'rnf'
pattern Rnf :: NFData a => a -> Rnf a
{-# COMPLETE Rnf #-}
pattern Rnf { getRnf } <- Rnf_ getRnf where Rnf a = Rnf_ (force a)

map :: (NFData a) => (b -> a) -> Rnf b -> Rnf a
map f (Rnf_ b) = Rnf (f b)

traverse :: (NFData a, Applicative f) => (b -> f a) -> Rnf b -> f (Rnf a)
traverse f (Rnf_ a) = Rnf <$> f a

(<!>) :: NFData a => Rnf (t -> a) -> Rnf t -> Rnf a
Rnf_ f <!> Rnf_ x = Rnf (f x)

instance NFData (Rnf a) where rnf _ = ()

instance (NFData a, Read a) => Read(Rnf a) where
  readsPrec p inp = [ (Rnf x, rest) | (x, rest) <- readsPrec p inp ]

instance (NFData a, Monoid a) => Monoid (Rnf a) where
  mempty = Rnf mempty
  mappend (Rnf a) (Rnf b) = Rnf (mappend a b)

instance (NFData a, Bounded a) => Bounded (Rnf a) where
  minBound = Rnf minBound
  maxBound = Rnf maxBound

instance (NFData a, Enum a) => Enum (Rnf a) where
  succ = Rnf . succ . getRnf
  pred = Rnf . pred . getRnf
  fromEnum = fromEnum . getRnf
  toEnum   = Rnf . toEnum
  enumFrom = fmap Rnf . enumFrom . getRnf
  enumFromThen (Rnf f) (Rnf t) = Rnf <$> enumFromThen f t
  enumFromTo (Rnf f) (Rnf t) = Rnf <$> enumFromTo f t
  enumFromThenTo (Rnf f) (Rnf th) (Rnf t) = Rnf <$> enumFromThenTo f th t

instance (NFData a, IsList a) => IsList (Rnf a) where
  type Item (Rnf a) = Item a
  fromList = Rnf . fromList
  toList = toList . getRnf

instance (NFData a, Num a) => Num (Rnf a) where
  Rnf a + Rnf b = Rnf (a + b)
  Rnf a - Rnf b = Rnf (a - b)
  Rnf a * Rnf b = Rnf (a * b)
  negate = Rnf . negate . getRnf
  abs = Rnf . abs . getRnf
  signum = Rnf . abs . getRnf
  fromInteger = Rnf . fromInteger

instance (NFData a, Integral a) => Integral (Rnf a) where
  quot (Rnf a) (Rnf b) = Rnf (quot a b)
  rem (Rnf a) (Rnf b) = Rnf (rem a b)
  div (Rnf a) (Rnf b) = Rnf (div a b)
  mod (Rnf a) (Rnf b) = Rnf (mod a b)
  quotRem (Rnf a) (Rnf b) = (Rnf *** Rnf) (quotRem a b)
  divMod (Rnf a) (Rnf b) = (Rnf *** Rnf) (divMod a b)
  toInteger = toInteger . getRnf

instance (NFData a, Fractional a) => Fractional (Rnf a) where
  Rnf a / Rnf b = Rnf (a / b)
  recip = Rnf . recip . getRnf
  fromRational = Rnf . fromRational

instance (NFData a, Floating a) => Floating (Rnf a) where
  pi = Rnf pi
  Rnf a ** Rnf b = Rnf (a ** b)
  logBase (Rnf a) (Rnf b) = Rnf (logBase a b)
  exp       = Rnf . exp . getRnf
  log       = Rnf . log . getRnf
  sqrt      = Rnf . sqrt . getRnf
  sin       = Rnf . sin . getRnf
  cos       = Rnf . cos . getRnf
  tan       = Rnf . tan . getRnf
  asin      = Rnf . asin . getRnf
  acos      = Rnf . acos . getRnf
  atan      = Rnf . atan . getRnf
  sinh      = Rnf . sinh . getRnf
  cosh      = Rnf . cosh . getRnf
  tanh      = Rnf . tanh . getRnf
  asinh     = Rnf . asinh . getRnf
  acosh     = Rnf . acosh . getRnf
  atanh     = Rnf . atanh . getRnf
  log1p     = Rnf . log1p . getRnf
  expm1     = Rnf . expm1 . getRnf
  log1pexp  = Rnf . log1pexp. getRnf
  log1mexp  = Rnf . log1mexp. getRnf

instance (NFData a, RealFloat a) => RealFloat (Rnf a) where
  floatRadix = floatRadix . getRnf
  floatDigits = floatDigits . getRnf
  floatRange = floatRange . getRnf
  decodeFloat = decodeFloat . getRnf
  encodeFloat i j = Rnf (encodeFloat i j)
  exponent = exponent . getRnf
  significand = Rnf . significand . getRnf
  scaleFloat i = Rnf . scaleFloat i . getRnf
  isNaN = isNaN . getRnf
  isInfinite = isInfinite . getRnf
  isDenormalized = isDenormalized . getRnf
  isNegativeZero = isNegativeZero . getRnf
  isIEEE = isIEEE . getRnf
  atan2 (Rnf a) (Rnf b) = Rnf (atan2 a b)

instance (NFData a, RealFrac a) => RealFrac (Rnf a) where
  properFraction = second Rnf . properFraction . getRnf
  truncate = truncate . getRnf
  round = round . getRnf
  ceiling = ceiling . getRnf
  floor = floor . getRnf

instance (NFData a, Real a) => Real (Rnf a) where
  toRational = toRational . getRnf
