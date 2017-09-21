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

module Multi
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
  , multi'maximum, multi'minimum, multi'maximumBy, multi'minimumBy
  -- * First and last
  , First, Last
  -- * Arrow composition
  , ArrowComposition
  -- * Dual
  , MultiDual
  -- * Monoidal folds
  , multi'fold, multi'foldMap
  -- * Right-associative folds
  , multi'foldr, multi'foldr', multi'foldr1
  -- * Left-associative folds
  , multi'foldl, multi'foldl', multi'foldl1
  -- * List-producing folds
  , multi'toList, multi'concatMap
  -- * Length
  , multi'null, multi'length
  -- * Looking for elements
  , multi'elem, multi'notElem, multi'find
  -- * Applicative sequencing
  , multi'traverse_, multi'for_, multi'sequence_
  -- * Alternative sequencing
  , multi'asum
  ) where

import Control.Applicative (Alternative (..), Applicative (..))
import Control.Arrow       (Kleisli)
import Control.Category    (id, (.))
import Control.Monad       (Monad)
import Data.Bool           (Bool (..), not, otherwise, (&&), (||))
import Data.Eq             (Eq (..))
import Data.Foldable       (Foldable)
import Data.Function       (flip)
import Data.List.NonEmpty  (NonEmpty (..))
import Data.Maybe          (Maybe (..), fromMaybe)
import Data.Ord            (Ord (..), Ordering (..))
import Data.Semigroup      (Semigroup (..))
import GHC.Exts            (build)
import Numeric.Natural     (Natural)
import Prelude             (Int, Integer, Integral, Num (..),
                            errorWithoutStackTrace, even, pred, quot, ($!))

import qualified Data.Foldable as Foldable
import qualified Data.List     as List


--------------------------------------------------------------------------------
--  Semigroup
--------------------------------------------------------------------------------

class MultiSemigroup x a where

  -- | An associative operation. Akin to '<>'.
  multi'append :: a -> a -> a

  -- | Reduce a non-empty list with '<>'. Akin to 'sconcat'.
  multi'sconcat :: NonEmpty a -> a
  multi'sconcat (a :| as) = go a as where
    go b (c:cs) = multi'append @x b (go c cs)
    go b []     = b

  -- | Repeat a value @n@ times. Akin to 'stimes'.
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

class MultiSemigroup x a => MultiMonoid x a where

  -- | Identity of 'multi'append'. Akin to 'mempty'.
  multi'empty :: a

  -- | Fold a list using the monoid. Akin to 'mconcat'.
  multi'mconcat :: [a] -> a
  multi'mconcat = multi'foldr (multi'append @x) (multi'empty @x)


--------------------------------------------------------------------------------
--  Foldable
--------------------------------------------------------------------------------

-- | Combine the elements of a structure using a monoid. Akin to
-- 'Foldable.fold'.
multi'fold :: forall x t m. (MultiMonoid x m, Foldable t) => t m -> m
multi'fold = multi'foldMap @x id

-- | Map each element of the structure to a monoid, and combine the results.
-- Akin to 'Foldable.foldMap'.
multi'foldMap :: forall x t m a. (MultiMonoid x m, Foldable t)
              => (a -> m) -> t a -> m
multi'foldMap f = multi'foldr (multi'append @x . f) (multi'empty @x)

-- | Right-associative fold of a structure. Akin to 'Foldable.foldr'.
multi'foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
multi'foldr f z t = multi'foldMap @ArrowComposition f t z

-- | Right-associative fold of a structure, but with strict application of
-- the operator. Akin to 'Foldable.foldr''.
multi'foldr' :: Foldable t => (a -> b -> b) -> b -> t a -> b
multi'foldr' f z0 xs = multi'foldl f' id xs z0
  where f' k x z = k $! f x z

-- | Left-associative fold of a structure. Akin to 'Foldable.foldl'.
multi'foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
multi'foldl f z t = multi'foldMap @(MultiDual ArrowComposition) (flip f) t z

-- | Left-associative fold of a structure but with strict application of
-- the operator. Akin to 'Foldable.foldl''.
multi'foldl' :: Foldable t => (b -> a -> b) -> b -> t a -> b
multi'foldl' f z0 xs = multi'foldr f' id xs z0
  where f' x k z = k $! f z x

-- | A variant of 'multi'foldr' that has no base case, and thus may only be
-- applied to non-empty structures. Akin to 'Foldable.foldr1'.
multi'foldr1 :: Foldable t => (a -> a -> a) -> t a -> a
multi'foldr1 f xs =
  fromMaybe (errorWithoutStackTrace "foldr1: empty structure")
            (multi'foldr mf Nothing xs)
  where
    mf x m = Just (case m of
                     Nothing -> x
                     Just y  -> f x y)

-- | A variant of 'multi'foldl' that has no base case, and thus may only be
-- applied to non-empty structures. Akin to 'Foldable.foldl1'.
multi'foldl1 :: Foldable t => (a -> a -> a) -> t a -> a
multi'foldl1 f xs =
  fromMaybe (errorWithoutStackTrace "foldl1: empty structure")
            (multi'foldl mf Nothing xs)
  where
    mf m y = Just (case m of
                     Nothing -> y
                     Just x  -> f x y)

-- | List of elements of a structure, from left to right. Akin to
-- 'Foldable.toList'.
multi'toList :: Foldable t => t a -> [a]
multi'toList t = build (\ c n -> multi'foldr c n t)

-- | Test whether the structure is empty. Akin to 'Foldable.null'.
multi'null :: Foldable t => t a -> Bool
multi'null = multi'foldr (\_ _ -> False) True

-- | Returns the size/length of a finite structure as an 'Int'. Akin to
-- 'Foldable.length'.
multi'length :: Foldable t => t a -> Int
multi'length = multi'foldl' (\c _ -> c + 1) 0

-- | Does the element occur in the structure? Akin to 'Foldable.elem'.
multi'elem :: Foldable t => Eq a => a -> t a -> Bool
multi'elem = multi'any . (==)

-- | The largest element of a non-empty structure. Akin to 'Foldable.maximum'.
multi'maximum :: forall t a. (Foldable t, Ord a) => t a -> a
multi'maximum =
  fromMaybe (errorWithoutStackTrace "maximum: empty structure") .
  (multi'foldMap @MaxMaybe) (Just @a)

-- | The least element of a non-empty structure. Akin to 'Foldable.minimum'.
multi'minimum :: forall t a. (Foldable t, Ord a) => t a -> a
multi'minimum =
  fromMaybe (errorWithoutStackTrace "minimum: empty structure") .
  (multi'foldMap @MinMaybe) (Just @a)

-- | The sum of the numbers in a structure. Akin to 'Foldable.sum'.
multi'sum :: (Foldable t, MultiMonoid Addition a) => t a -> a
multi'sum = multi'fold @Addition

-- | The product of the numbers of a structure. Akin to 'Foldable.product'
multi'product :: (Foldable t, MultiMonoid Multiplication a) => t a -> a
multi'product = multi'fold @Multiplication

-- | Map each element of a structure to an action, evaluate these
-- actions from left to right, and ignore the results. Akin to
-- 'Foldable.traverse_'.
multi'traverse_ :: forall t f a b. (Foldable t, Applicative f)
                => (a -> f b) -> t a -> f ()
multi'traverse_ f = multi'foldr ((*>) . f) (pure ())

-- | 'multi'traverse_' with its arguments flipped. Akin to 'Foldable.for_'.
multi'for_ :: forall t f a b. (Foldable t, Applicative f)
           => t a -> (a -> f b) -> f ()
multi'for_ = flip multi'traverse_

-- | Evaluate each action in the structure from left to right, and ignore the
-- results. Akin to 'Foldable.sequenceA_'.
multi'sequence_ :: forall t f a. (Foldable t, Applicative f)
                => t (f a) -> f ()
multi'sequence_ = multi'foldr (*>) (pure ())

-- | The sum of a collection of actions, generalizing 'multi'concat'. Akin to
-- 'Foldable.asum'.
multi'asum :: forall t f a. (Foldable t, Alternative f) => t (f a) -> f a
multi'asum = multi'foldr (<|>) empty

-- | Map a function over all the elements of a container and concatenate
-- the resulting lists. Akin to 'Foldable.concatMap'.
multi'concatMap :: forall t a b. Foldable t => (a -> [b]) -> t a -> [b]
multi'concatMap f xs =
  build (\c n -> multi'foldr (\x b -> multi'foldr c b (f x)) n xs)

-- | The conjunction of a container of Bools. Akin to 'Foldable.and'.
multi'and :: Foldable t => t Bool -> Bool
multi'and = multi'fold @And

-- | The disjunction of a container of Bools. Akin to 'Foldable.or'.
multi'or :: Foldable t => t Bool -> Bool
multi'or = multi'fold @Or

-- | Determines whether any element of the structure satisfies the predicate.
-- Akin to 'Foldable.any'.
multi'any :: Foldable t => (a -> Bool) -> t a -> Bool
multi'any = multi'foldMap @Or

-- | Determines whether all elements of the structure satisfy the predicate.
-- Akin to 'Foldable.all'.
multi'all :: Foldable t => (a -> Bool) -> t a -> Bool
multi'all = multi'foldMap @And

-- | The largest element of a non-empty structure with respect to the given
-- comparison function. Akin to 'Foldable.maximumBy'.
multi'maximumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
multi'maximumBy cmp = multi'foldr1 max'
  where max' x y = case cmp x y of GT -> x; _ -> y

-- | The least element of a non-empty structure with respect to the given
-- comparison function. Akin to 'Foldable.minimumBy'.
multi'minimumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
multi'minimumBy cmp = multi'foldr1 min'
  where min' x y = case cmp x y of GT -> y; _ -> x

-- | The negation of 'multi'elem'. Akin to 'Foldable.notElem'.
multi'notElem :: (Foldable t, Eq a) => a -> t a -> Bool
multi'notElem x = not . multi'elem x

-- | The 'find' function takes a predicate and a structure and returns
-- the leftmost element of the structure matching the predicate, or
-- 'Nothing' if there is no such element. Akin to 'Foldable.find'.
multi'find :: Foldable t => (a -> Bool) -> t a -> Maybe a
multi'find p = multi'foldMap @First (\x -> if p x then Just x else Nothing)


--------------------------------------------------------------------------------
--  Unit
--------------------------------------------------------------------------------

instance MultiSemigroup x ()
  where multi'append _ _ = ()


--------------------------------------------------------------------------------
--  Default
--------------------------------------------------------------------------------

data Default

instance Semigroup a => MultiSemigroup Default a
  where multi'append = (<>)


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
  where multi'append = (List.++)
instance MultiMonoid Addition [a]
  where multi'empty = []


--------------------------------------------------------------------------------
--  Dual
--------------------------------------------------------------------------------

data MultiDual a

instance MultiSemigroup x a => MultiSemigroup (MultiDual x) a
  where multi'append a b = multi'append @x b a
instance MultiMonoid x a => MultiMonoid (MultiDual x) a
  where multi'empty = multi'empty @x
