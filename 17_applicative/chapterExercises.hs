import Data.List (elemIndex)
import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

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

-- Chapter Exercises
-- 1. Type []
-- Methods
-- pure :: a ->  [a]
-- (<*>) :: [(a -> b)] -> [a] -> [b]

-- 2. Type IO
-- Methods
-- pure :: a -> IO a
-- (<*>) :: IO (a -> b) -> IO a -> IO b

-- 3. Type(,) a
-- Methods
-- pure :: Monoid a => b -> (a, b)
-- (<*>) :: (a, (b -> c)) -> (a, b) -> (a, c)

-- 4. Type (->) e
-- Methods
-- pure :: a -> (e -> a)
-- (<*>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b)

-- 1) 
data Pair a = Pair a a deriving Show

instance Semigroup a => Semigroup (Pair a) where
  (<>) (Pair a b) (Pair c d) = Pair (a <> c) (b <> d)

instance Monoid a => Monoid (Pair a) where
  mempty = Pair mempty mempty
  mappend = (<>)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure x = Pair x x
  Pair x x' <*> Pair y y' = Pair (x y) (x' y')

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

-- 2. 
data Two a b = Two a b

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two c d) = Two (a <> c) (b <> d)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure x = Two mempty x
  (Two a b) <*> (Two c d) = Two (a <> c) (b d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Arbitrary a b) where
  (=-=) = eq

-- 3. 
data Three a b c = Three a b c

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure x = Three mempty mempty x
  (Three a b c) <*> (Three x y z) = Three (a <> x) (b <> y) (c z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

-- 4. 
data Three' a b = Three' a b b

instance Functor (Three a) where
  fmap f (Three a b b') = Three a (f b) (f b')

instance Monoid a => Applicative (Three a) where
  pure x = Three mempty x x
  (Three a b b2) <*> (Three x y y2) = Three (a <> x) (b <> y) (b2 y2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three a b) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three a b) where
  (=-=) = eq

-- 5. 
data Four a b c d = Four a b c d

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure x = Four mempty mempty mempty x
  (Four a b c d) <*> (Four w x y z) = Four (a <> w) (b <> x) (c <> y) (d z)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq


-- 6.
data Four' a b = Four' a a a b

instance Functor (Four' a) where
  fmap f (Four' a a2 a3 b) = Four' a a2 a3 (f b)

instance Monoid a => Applicative (Four' a) where
  pure x = Four' mempty mempty mempty x
  (Four' a a2 a3 b) <*> (Four' x x2 x3 y) = Four' (a <> x) (a2 <> x2) (a3 <> x3) (b y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

-- Combinations
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

allCombos :: [(Char,Char,Char)]
allCombos =  combos stops vowels stops