[![Travis Build Status](https://travis-ci.org/pepeiborra/strict-types.svg)](https://travis-ci.org/pepeiborra/strict-types)
[![Hackage](https://img.shields.io/hackage/v/strict-types.svg)](https://hackage.haskell.org/package/strict-types)
[![Stackage Nightly](http://stackage.org/package/strict-types/badge/nightly)](http://stackage.org/nightly/package/strict-types)

strict-types
===================

This package provides a type-level predicate `Strict` and a value level pattern `IsStrict` to constrain generic types.

```
> IsStrict (False)
False
> IsStrict (Identity False)
Identity False
> IsStrict (Identity (Just False))

<interactive>:1:1: error:
    • Maybe Bool has an unnamed lazy field in constructor Just
    • In the expression: IsStrict (Identity (Just False))
      In an equation for ‘it’: it = IsStrict (Identity (Just False))
```
