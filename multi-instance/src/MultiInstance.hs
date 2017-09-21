{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{- |

The 'MultiInstance' module provides alternative versions of common typeclasses,
augmented with a phantom type parameter @x@. The purpose of this is to deal with
the case where a type has more than one candidate instance for the original,
unaugmented class.

= Example: Integer sum and product

The canonical example of this predicament is selecting the monoid instance for a
type which forms a ring (and thus has at least two strong candidates for
selection as /the/ monoid), such as 'Integer'. This therefore gives rise to the
'Data.Functor.Sum' and 'Data.Functor.Product' newtype wrappers, corresponding to
the additive and multiplicative monoids respectively.

The traditional 'Data.Foldable.fold'-based summation of a list of integers
looks like this:

>>> import Data.Foldable (fold)
>>> import Data.Monoid (Sum (..))
>>> getSum (fold [Sum 2, Sum 3, Sum 5]) :: Integer
10

By replacing 'Data.Foldable.fold' with 'multi'fold', whose constraint is
'MultiMonoid' rather than 'Data.Monoid.Monoid', we can write the same thing
without the newtype wrapper:

>>> :set -XFlexibleContexts -XTypeApplications
>>> multi'fold @Addition [2, 3, 5] :: Integer
10

-}

module MultiInstance
  (
  -- * Semigroup
    MultiSemigroup (multi'append, multi'sconcat, multi'stimes)
  -- * Monoid
  , MultiMonoid (multi'empty, multi'mconcat)
  -- * Default
  , Default
  -- * Conjunction and disjunction
  , Conjunction, Disjunction
  -- * Addition and multiplication
  , Addition, Multiplication, multi'sum, multi'product
  -- * Boolean /and/ and /or/
  , And, Or, multi'and, multi'or, multi'any, multi'all
  -- * Min and max
  , Min, Max, MinMaybe, MaxMaybe
  -- * First and last
  , First, Last
  -- * Arrow composition
  , ArrowComposition
  -- * Dual
  , MultiDual
  -- * Monoidal folds
  , multi'fold, multi'foldMap
  -- * Looking for elements
  , multi'find
  ) where

import Control.Arrow      (Kleisli)
import Control.Category   (id, (.))
import Control.Monad      (Monad)
import Data.Bool          (Bool (..), otherwise, (&&), (||))
import Data.Eq            (Eq (..))
import Data.Foldable      (Foldable)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe         (Maybe (..))
import Data.Ord           (Ord (..))
import Numeric.Natural    (Natural)
import Prelude            (Int, Integer, Integral, Num (..),
                           errorWithoutStackTrace, even, pred, quot)

import qualified Data.Foldable
import qualified Data.Monoid
import qualified Data.Semigroup


--------------------------------------------------------------------------------
--  Semigroup
--------------------------------------------------------------------------------

-- | Akin to the 'Data.Semigroup.Semigroup' class, but with the addition of the
-- phantom type parameter @x@ which lets you specify /which/ semigroup to use.
--
-- For example, the integers form a semigroup via either 'Addition' or
-- 'Multiplication':
--
-- >>> :set -XFlexibleContexts -XTypeApplications
-- >>> multi'append @Addition 6 7 :: Integer
-- 13
-- >>> multi'append @Multiplication 6 7 :: Integer
-- 42
-- >>> multi'stimes @Addition (3 :: Natural) (4 :: Integer)
-- 12
-- >>> multi'stimes @Multiplication (3 :: Natural) (4 :: Integer)
-- 64
class MultiSemigroup x a where

  -- | An associative operation.
  --
  -- /Akin to 'Data.Semigroup.<>'./
  multi'append :: a -> a -> a

  -- | Reduce a non-empty list with 'multi'append'.
  --
  -- /Akin to 'Data.Semigroup.sconcat'./
  multi'sconcat :: NonEmpty a -> a
  multi'sconcat (a :| as) = go a as where
    go b (c:cs) = multi'append @x b (go c cs)
    go b []     = b

  -- | Repeat a value @n@ times.
  --
  -- /Akin to 'Data.Semigroup.stimes'./
  multi'stimes :: Integral b => b -> a -> a
  multi'stimes y0 x0
    | y0 <= 0   = errorWithoutStackTrace
                    "multi'stimes: positive multiplier expected"
    | otherwise = f x0 y0
    where
      f x y
        | even y    = f (multi'append @x x x) (y `quot` 2)
        | y == 1    = x
        | otherwise = g (multi'append @x x x) (pred y `quot` 2) x
      g x y z
        | even y    = g (multi'append @x x x) (y `quot` 2) z
        | y == 1    = multi'append @x x z
        | otherwise = g (multi'append @x x x) (pred y `quot` 2)
                        (multi'append @x x z)


--------------------------------------------------------------------------------
--  Monoid
--------------------------------------------------------------------------------

-- | Akin to the 'Data.Monoid.Monoid' class, but with the addition of the
-- phantom type parameter @x@ which lets you specify /which/ monoid to use.
--
-- For example, the integers form a monoid via either 'Addition' or
-- 'Multiplication':
--
-- >>> :set -XFlexibleContexts -XTypeApplications
-- >>> multi'fold @Addition [] :: Integer
-- 0
-- >>> multi'fold @Addition [2, 3, 5] :: Integer
-- 10
-- >>> multi'fold @Multiplication [] :: Integer
-- 1
-- >>> multi'fold @Multiplication [2, 3, 5] :: Integer
-- 30
class MultiSemigroup x a => MultiMonoid x a where

  -- | Identity of 'multi'append'.
  --
  -- /Akin to 'Data.Monoid.mempty'./
  multi'empty :: a

  -- | Fold a list using the monoid.
  --
  -- /Akin to 'Data.Monoid.mconcat'./
  multi'mconcat :: [a] -> a
  multi'mconcat = Data.Foldable.foldr (multi'append @x) (multi'empty @x)


--------------------------------------------------------------------------------
--  Foldable
--------------------------------------------------------------------------------

-- | Combine the elements of a structure using a monoid.
--
-- /Akin to 'Data.Foldable.fold'./
multi'fold :: forall x t m. (MultiMonoid x m, Foldable t) => t m -> m
multi'fold = multi'foldMap @x id

-- | Map each element of the structure to a monoid, and combine the results.
--
-- /Akin to 'Data.Foldable.foldMap'./
multi'foldMap :: forall x t m a. (MultiMonoid x m, Foldable t)
              => (a -> m) -> t a -> m
multi'foldMap f = Data.Foldable.foldr (multi'append @x . f) (multi'empty @x)

-- | The sum of the numbers in a structure.
--
-- /Equivalent to @'multi'fold' \@'Addition'@./
--
-- /Akin to 'Data.Foldable.sum'./
multi'sum :: (Foldable t, MultiMonoid Addition a) => t a -> a
multi'sum = multi'fold @Addition

-- | The product of the numbers of a structure.
--
-- /Equivalent to @'multi'fold' \@'Multiplication'@./
--
-- /Akin to 'Data.Foldable.product'./
multi'product :: (Foldable t, MultiMonoid Multiplication a) => t a -> a
multi'product = multi'fold @Multiplication

-- | The conjunction of a container of Bools.
--
-- /Equivalent to @'multi'fold' \@'And'@./
--
-- /Akin to 'Data.Foldable.and'./
multi'and :: (Foldable t, MultiMonoid And a) => t a -> a
multi'and = multi'fold @And

-- | The disjunction of a container of Bools.
--
-- /Equivalent to @'multi'fold' \@'Or'@./
--
-- /Akin to 'Data.Foldable.or'./
multi'or :: (Foldable t, MultiMonoid Or a) => t a -> a
multi'or = multi'fold @Or

-- | Determines whether any element of the structure satisfies the predicate.
--
-- /Equivalent to @'multi'foldMap' \@'Or'@./
--
-- /Akin to 'Data.Foldable.any'./
multi'any :: (Foldable t, MultiMonoid Or b) => (a -> b) -> t a -> b
multi'any = multi'foldMap @Or

-- | Determines whether all elements of the structure satisfy the predicate.
--
-- /Equivalent to @'multi'foldMap' \@'And'@./
--
-- /Akin to 'Data.Foldable.all'./
multi'all :: Foldable t => (a -> Bool) -> t a -> Bool
multi'all = multi'foldMap @And

-- | Takes a predicate and a structure and returns the leftmost element of the
-- structure matching the predicate, or 'Nothing' if there is no such element.
--
-- /Akin to 'Data.Foldable.find'./
multi'find :: Foldable t => (a -> Bool) -> t a -> Maybe a
multi'find p = multi'foldMap @First (\x -> if p x then Just x else Nothing)


--------------------------------------------------------------------------------
--  Unit
--------------------------------------------------------------------------------

instance MultiSemigroup x ()
  where multi'append _ _ = ()

instance MultiMonoid x ()
  where multi'empty = ()


--------------------------------------------------------------------------------
--  Default
--------------------------------------------------------------------------------

data Default

instance Data.Semigroup.Semigroup a => MultiSemigroup Default a
  where multi'append = (Data.Semigroup.<>)

instance (Data.Semigroup.Semigroup a, Data.Monoid.Monoid a) =>
    MultiMonoid Default a
  where multi'empty = Data.Monoid.mempty


--------------------------------------------------------------------------------
--  Conjunction and disjunction
--------------------------------------------------------------------------------

data Conjunction

data Disjunction


--------------------------------------------------------------------------------
--  Boolean /and/ and /or/
--------------------------------------------------------------------------------

type And = Conjunction

type Or = Disjunction

instance MultiSemigroup And Bool
  where multi'append = (&&)
instance MultiMonoid And Bool
  where multi'empty = True

instance MultiSemigroup Or Bool
  where multi'append = (||)
instance MultiMonoid Or Bool
  where multi'empty = False


--------------------------------------------------------------------------------
--  Addition and multiplication
--------------------------------------------------------------------------------

type Addition = Disjunction

type Multiplication = Conjunction

instance MultiSemigroup Addition Int
  where multi'append = (+)
instance MultiSemigroup Addition Integer
  where multi'append = (+)
instance MultiSemigroup Addition Natural
  where multi'append = (+)

instance MultiMonoid Addition Int
  where multi'empty = 0
instance MultiMonoid Addition Integer
  where multi'empty = 0
instance MultiMonoid Addition Natural
  where multi'empty = 0

instance MultiSemigroup Multiplication Int
  where multi'append = (*)
instance MultiSemigroup Multiplication Integer
  where multi'append = (*)
instance MultiSemigroup Multiplication Natural
  where multi'append = (*)

instance MultiMonoid Multiplication Int
  where multi'empty = 1
instance MultiMonoid Multiplication Integer
  where multi'empty = 1
instance MultiMonoid Multiplication Natural
  where multi'empty = 1


--------------------------------------------------------------------------------
--  Min and Max
--------------------------------------------------------------------------------

data Min

data Max

instance Ord a => MultiSemigroup Min a
  where multi'append = min
instance Ord a => MultiSemigroup Max a
  where multi'append = max

data MinMaybe

data MaxMaybe

instance Ord a => MultiSemigroup MinMaybe (Maybe a)
  where multi'append Nothing x = x
        multi'append x Nothing = x
        multi'append (Just x) (Just y) = Just (min x y)
instance Ord a => MultiMonoid MinMaybe (Maybe a)
  where multi'empty = Nothing

instance Ord a => MultiSemigroup MaxMaybe (Maybe a)
  where multi'append Nothing x = x
        multi'append x Nothing = x
        multi'append (Just x) (Just y) = Just (max x y)
instance Ord a => MultiMonoid MaxMaybe (Maybe a)
  where multi'empty = Nothing


--------------------------------------------------------------------------------
--  First and last
--------------------------------------------------------------------------------

data First

data Last

instance MultiSemigroup First (Maybe a)
  where multi'append x@(Just _) _ = x
        multi'append _          x = x

instance MultiMonoid First (Maybe a)
  where multi'empty = Nothing

instance MultiSemigroup Last (Maybe a)
  where multi'append _ x@(Just _) = x
        multi'append x          _ = x

instance MultiMonoid Last (Maybe a)
  where multi'empty = Nothing


--------------------------------------------------------------------------------
--  Arrow composition
--------------------------------------------------------------------------------

data ArrowComposition

instance MultiSemigroup ArrowComposition (a -> a)
  where multi'append = (.)
instance MultiMonoid ArrowComposition (a -> a)
  where multi'empty  = id

instance Monad m => MultiSemigroup ArrowComposition (Kleisli m a a)
  where multi'append = (.)
instance Monad m => MultiMonoid ArrowComposition (Kleisli m a a)
  where multi'empty  = id


--------------------------------------------------------------------------------
--  List
--------------------------------------------------------------------------------

instance MultiSemigroup Addition [a]
  where multi'append = multi'append @Default
instance MultiMonoid Addition [a]
  where multi'empty = multi'empty @Default

instance MultiSemigroup Addition (NonEmpty a)
  where multi'append = multi'append @Default


--------------------------------------------------------------------------------
--  Dual
--------------------------------------------------------------------------------

data MultiDual a

instance MultiSemigroup x a => MultiSemigroup (MultiDual x) a
  where multi'append a b = multi'append @x b a
instance MultiMonoid x a => MultiMonoid (MultiDual x) a
  where multi'empty = multi'empty @x
