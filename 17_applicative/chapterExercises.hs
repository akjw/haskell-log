import Data.List (elemIndex)
import Control.Applicative

-- Exercises: Lookups
-- 1. 
added :: Maybe Integer
added =
  fmap (+3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

-- 2. 
y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- 3.
a :: Maybe Int
a = elemIndex 3 [1, 2, 3, 4, 5]

b :: Maybe Int
b = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = liftA2 max' a b -- == max' <$> a <*> b

-- 4.
xs = [1, 2, 3]
ys = [4, 5, 6]

c :: Maybe Integer
c = lookup 3 $ zip xs ys

d :: Maybe Integer
d = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> liftA2 (,) c d

-- Exercise: Identity instance
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (Identity g) <*> (Identity x) = Identity (g x)

-- Exercise: Constant instance
newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  -- fmap f (Constant a b) = Constant a (f b) 
  -- something like this would not work, 
  -- since type variable b is phantom & type variable a is 
  -- part of the structure --> fn application cannot occur
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty -- partially applied with mempty
  (Constant a) <*> (Constant b) = Constant (a <> b) 
  -- since fn app is discarded, mappend is all that's left

-- Exercise: Fixer upper
-- 1. 
justHello = const <$> Just "Hello" <*> (pure "World" :: Maybe String)
-- 2. 
quatuple = (,,,) <$> Just 90 <*> Just 10<*> Just "Tierness" <*> (pure [1, 2, 3] :: Maybe [Int])