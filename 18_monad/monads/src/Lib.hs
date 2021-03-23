module Lib
    ( CountMe
    , Nope
    , BahEither
    , Identity
    , List
    ) where

import Test.QuickCheck
import Test.QuickCheck.Checkers


data CountMe a =
  CountMe Integer a
  deriving (Eq, Show)

instance Functor CountMe where
  fmap f (CountMe i a) = CountMe (i + 1) (f a)

instance Applicative CountMe where
  pure = CountMe 0
  CountMe n f <*> CountMe n' a = CountMe (n + n') (f a)

instance Monad CountMe where
  return = pure
  CountMe n a >>= f =
    let CountMe _ b = f a
    in CountMe (n + 1) b

instance Arbitrary a
  => Arbitrary (CountMe a) where
  arbitrary = CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where
  (=-=) = eq

-- Chapter exercises
--1)
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  NopeDotJpg  <*>  _ = NopeDotJpg

instance Monad Nope where
  return = pure
  NopeDotJpg  >>= _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg 
  
instance EqProp (Nope a) where 
  (=-=) = eq

-- 2. 
data BahEither b a =
    PLeft a
  | PRight b deriving (Eq, Show)

instance Functor (BahEither b) where
  fmap f (PLeft x) = PLeft (f x)
  fmap _ (PRight x) = PRight x

instance Applicative (BahEither b) where
  pure = PLeft
  (PLeft f)   <*> (PLeft y) = PLeft (f y)
  (PRight x)  <*> _          = PRight x
  _           <*> (PRight x) = PRight x

instance Monad (BahEither b) where
  PLeft x >>= f = f x
  PRight x >>= _ = PRight x

instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither b a) where
  arbitrary = oneof [ PLeft <$> arbitrary
                    , PRight <$> arbitrary ]

instance (Eq a, Eq b) => EqProp (BahEither b a) where
  (=-=) = eq

-- 3. Write a Monad instance for Identity:
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
  return = pure
  Identity x >>= f = f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

--4.
data List a =
    Nil
  | Cons a (List a) deriving (Eq, Show)

instance Semigroup (List a) where
  Nil <> xs = xs
  xs <> Nil = xs
  (Cons x xs) <> yys = Cons x (xs <> yys)

instance Monoid (List a) where
  mempty = Nil

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _   = Nil
  _  <*> Nil = Nil
  (Cons f fs) <*> xs = (f <$> xs) <> (fs <*> xs)

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  Cons x xs >>= f = f x <> (xs >>= f)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = oneof [ Cons <$> arbitrary <*> arbitrary
                    , return Nil]

instance Eq a => EqProp (List a) where
  (=-=) = eq




