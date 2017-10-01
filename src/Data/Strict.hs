{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
module Data.Strict
    ( Forced (Forced, getForced)
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
newtype Forced a = Forced_ a
  deriving ( Eq
             , Ord
             , Show
             , Hashable
             , Foldable
             )

-- | A pattern constructor that forces its contents to 'rnf'
pattern Forced :: NFData a => a -> Forced a
{-# COMPLETE Forced #-}
pattern Forced { getForced } <- Forced_ getForced where Forced a = Forced_ (force a)

map :: (NFData a) => (b -> a) -> Forced b -> Forced a
map f (Forced_ b) = Forced (f b)

traverse :: (NFData a, Applicative f) => (b -> f a) -> Forced b -> f (Forced a)
traverse f (Forced_ a) = Forced <$> f a

(<!>) :: NFData a => Forced (t -> a) -> Forced t -> Forced a
Forced_ f <!> Forced_ x = Forced (f x)

instance NFData (Forced a) where rnf _ = ()

instance (NFData a, Read a) => Read(Forced a) where
  readsPrec p inp = [ (Forced x, rest) | (x, rest) <- readsPrec p inp ]

instance (NFData a, Monoid a) => Monoid (Forced a) where
  mempty = Forced mempty
  mappend (Forced a) (Forced b) = Forced (mappend a b)

instance (NFData a, Bounded a) => Bounded (Forced a) where
  minBound = Forced minBound
  maxBound = Forced maxBound

instance (NFData a, Enum a) => Enum (Forced a) where
  succ = Forced . succ . getForced
  pred = Forced . pred . getForced
  fromEnum = fromEnum . getForced
  toEnum   = Forced . toEnum
  enumFrom = fmap Forced . enumFrom . getForced
  enumFromThen (Forced f) (Forced t) = Forced <$> enumFromThen f t
  enumFromTo (Forced f) (Forced t) = Forced <$> enumFromTo f t
  enumFromThenTo (Forced f) (Forced th) (Forced t) = Forced <$> enumFromThenTo f th t

instance (NFData a, IsList a) => IsList (Forced a) where
  type Item (Forced a) = Item a
  fromList = Forced . fromList
  toList = toList . getForced

instance (NFData a, Num a) => Num (Forced a) where
  Forced a + Forced b = Forced (a + b)
  Forced a - Forced b = Forced (a - b)
  Forced a * Forced b = Forced (a * b)
  negate = Forced . negate . getForced
  abs = Forced . abs . getForced
  signum = Forced . abs . getForced
  fromInteger = Forced . fromInteger

instance (NFData a, Integral a) => Integral (Forced a) where
  quot (Forced a) (Forced b) = Forced (quot a b)
  rem (Forced a) (Forced b) = Forced (rem a b)
  div (Forced a) (Forced b) = Forced (div a b)
  mod (Forced a) (Forced b) = Forced (mod a b)
  quotRem (Forced a) (Forced b) = (Forced *** Forced) (quotRem a b)
  divMod (Forced a) (Forced b) = (Forced *** Forced) (divMod a b)
  toInteger = toInteger . getForced

instance (NFData a, Fractional a) => Fractional (Forced a) where
  Forced a / Forced b = Forced (a / b)
  recip = Forced . recip . getForced
  fromRational = Forced . fromRational

instance (NFData a, Floating a) => Floating (Forced a) where
  pi = Forced pi
  Forced a ** Forced b = Forced (a ** b)
  logBase (Forced a) (Forced b) = Forced (logBase a b)
  exp       = Forced . exp . getForced
  log       = Forced . log . getForced
  sqrt      = Forced . sqrt . getForced
  sin       = Forced . sin . getForced
  cos       = Forced . cos . getForced
  tan       = Forced . tan . getForced
  asin      = Forced . asin . getForced
  acos      = Forced . acos . getForced
  atan      = Forced . atan . getForced
  sinh      = Forced . sinh . getForced
  cosh      = Forced . cosh . getForced
  tanh      = Forced . tanh . getForced
  asinh     = Forced . asinh . getForced
  acosh     = Forced . acosh . getForced
  atanh     = Forced . atanh . getForced
  log1p     = Forced . log1p . getForced
  expm1     = Forced . expm1 . getForced
  log1pexp  = Forced . log1pexp. getForced
  log1mexp  = Forced . log1mexp. getForced

instance (NFData a, RealFloat a) => RealFloat (Forced a) where
  floatRadix = floatRadix . getForced
  floatDigits = floatDigits . getForced
  floatRange = floatRange . getForced
  decodeFloat = decodeFloat . getForced
  encodeFloat i j = Forced (encodeFloat i j)
  exponent = exponent . getForced
  significand = Forced . significand . getForced
  scaleFloat i = Forced . scaleFloat i . getForced
  isNaN = isNaN . getForced
  isInfinite = isInfinite . getForced
  isDenormalized = isDenormalized . getForced
  isNegativeZero = isNegativeZero . getForced
  isIEEE = isIEEE . getForced
  atan2 (Forced a) (Forced b) = Forced (atan2 a b)

instance (NFData a, RealFrac a) => RealFrac (Forced a) where
  properFraction = second Forced . properFraction . getForced
  truncate = truncate . getForced
  round = round . getForced
  ceiling = ceiling . getForced
  floor = floor . getForced

instance (NFData a, Real a) => Real (Forced a) where
  toRational = toRational . getForced
