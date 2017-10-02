[![Travis Build Status](https://travis-ci.org/pepeiborra/strict-types.svg)](https://travis-ci.org/pepeiborra/strict-types)
[![Hackage](https://img.shields.io/hackage/v/strict-types.svg)](https://hackage.haskell.org/package/strict-types)
[![Stackage Nightly](http://stackage.org/package/strict-types/badge/nightly)](http://stackage.org/nightly/package/strict-types)

strict-types
===================

This package provides two pattern synonyms `Strict` and `Rnf` to constrain value strictness.

##### Use `Strict` when you can and `Rnf` when you must. #####

The Rnf pattern
----------------
The `Rnf` pattern matches every value of a type with an `NFData` instance, forcing it to rigid normal form before binding it.
```
> let !(Rnf x) = [trace "One" 1, trace "Two" 2]
One
Two
```
The bang pattern is needed to force the `Rnf x` closure to weak head normal form (whnf).

`Rnf` is very handy to avoid space leaks when working with non-strict data, but forcing structured data with `rnf` has a cost even if the data is already forced, and it should be avoided in inner loops. Too few `Rnf` patterns and one risks a space leak, too many and one ends up with squared complexity factors.

The Strict pattern
-----------------------
If our datatypes are strict, then their weak head normal form is already fully evaluated and `Rnf` is morally just `seq`. In practice, it depends on what the `NFData` instance does. One would hope it would be implemented as a noop, but in practice this is not always the case for first order types, and can never be for higher kinded types which cannot make any assumptions about the strictness of their type parameters.

But not all is lost! We can ask the typechecker to inspect the GHC Generics representation for a type to check if it is strict, and avoid calling `rnf` if that is the case. This is what the `Strict` pattern synonym does:
```
> let !(Strict x) = [1, 2]
<interactive>:1:7: error:
    • [Int] has an unnamed lazy field in constructor :
    • In the pattern: Strict y
      In the pattern: !(Strict y)
      In a pattern binding:
        !(Strict y) = [trace "1" 1, trace "2" (2 :: Int)]
```
Lists are not strict, so the expression above does not type check.

If we define our own strict list datatype with a `Generic` instance, the type checker can certify the property that `!(Strict x)` is equivalent to `!(Rnf x)`:
```
> data StrictList a = Nil | Cons !a (StrictList a) deriving Generic ; infixr :!
> let !(Strict y) = trace "one" 1 :! trace "two" 2 :! Nil
two
one
```

The StrictType class
--------------------------
Not all strict types derive `Generic`. For such cases where the type checker is unable to see the strictness information we can "promise" that a type is deep strict by adding an instance of the `StrictType` class. 

Caveats
-----------
Non regular recursive types, also known as nested datatypes, will cause the type checker to loop and run out of fuel when trying to prove deep strictness.

FAQ
----------
* What about the `Strict` and `StrictData` pragmas ?

The `Strict` pragma adds an implicit bang pattern on every binding, but it doesn't force values to normal form.

The `StrictData` pragma adds an implicit bang pattern on every field of a data type guaranteeing that first order types are strict, but does not help with higher kinded types.
