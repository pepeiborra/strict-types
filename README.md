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
