module ValidationApp where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation' err a =
    Fail err
  | Succ a
  deriving (Eq, Show)

-- same as Either
instance Functor (Validation' e) where
  fmap _ (Fail x) = Fail x
  fmap f (Succ x) = Succ (f x)

-- This is different
instance Monoid e => Applicative (Validation' e) where
  pure = Succ
  Succ x <*> Succ y = Succ (x y)
  Succ _ <*> Fail x = Fail x
  Fail x <*> Succ _ = Fail x
  Fail x <*> Fail y = Fail (x <> y)

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation' e a) where
  arbitrary = oneof [ Fail <$> arbitrary 
                    , Succ <$> arbitrary]

instance (Eq e, Eq a) => EqProp (Validation' e a) where
  (=-=) = eq


main :: IO ()
main =
  let val = undefined :: Validation' String ([Char], [Char], [Char])
  in quickBatch (applicative val)

-- validationToEither :: Validation e a -> Either e a
-- validationToEither (Fail err) = Left err
-- validationToEither (Succ a) = Right a

-- eitherToValidation :: Either e a -> Validation e a
-- eitherToValidation (Left err) = Fail err
-- eitherToValidation (Right a) = Succ a

-- eitherToValidation . validationToEither == id
-- validationToEither . eitherToValidation == id

data Errors =
    DividedByZero
  | StackOverflow
  | MooglesChewedWires
  deriving (Eq, Show)

-- Succ = Succ (+1) <*> Succ 1
-- Succ == Succ 2

-- Fail = Succ (+1) <*> Fail [StackOverflow]
-- Fail == Fail [StackOverflow]

-- Fail' = Fail [StackOverflow] <*> Succ (+1)
-- Fail' == Fail [StackOverflow]

-- Fails = Fail [MooglesChewedWires] <*> Fail [StackOverflow]
-- Fails == Fail [MooglesChewedWires
--             , StackOverflow]

