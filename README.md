# multi-instance

[![](https://travis-ci.org/chris-martin/multi-instance.svg)](https://travis-ci.org/chris-martin/multi-instance)

Alternative versions of common typeclasses, augmented with a phantom type
parameter `x`. The purpose of this is to deal with the case where a type has
more than one candidate instance for the original, unaugmented class.

## Example: Integer sum and product

The canonical example of this predicament is selecting the monoid instance for a
type which forms a ring (and thus has at least two strong candidates for
selection as *the* monoid), such as `Integer`. This therefore gives rise to the
`Sum` and `Product` newtype wrappers, corresponding to the additive and
multiplicative monoids respectively.

The traditional `fold`-based summation of a list of integers looks like this:

```haskell
>>> import Data.Foldable (fold)
>>> import Data.Monoid (Sum (..))
>>> getSum (fold [Sum 2, Sum 3, Sum 5]) :: Integer
10
```

By replacing `fold` with `multi'fold`, whose constraint is `MultiMonoid` rather
than `Data.Monoid.Monoid`, we can write the same thing without the newtype
wrapper:

```haskell
>>> :set -XFlexibleContexts -XTypeApplications
>>> multi'fold @Addition [2, 3, 5] :: Integer
10
```
