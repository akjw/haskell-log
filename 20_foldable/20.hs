{- HLINT Ignore -}
import Data.Monoid
import Data.Foldable

-- fold: does not take a fn; requires monoidal instance to be specified for elements that have multiples instances 
-- xs :: [Product Integer]
-- xs = [1, 2, 3, 4, 5]
-- fold xs = Product {getProduct = 120}

-- sometimes compiler can infer appropriate instance:
-- Prelude> fold ["hello", " julie"]
-- "hello julie"

-- foldMap: takes a fn, but the fn argument must explicitly map a Monoid to every element

-- Prelude> foldMap Sum [1, 2, 3, 4]
-- Sum {getSum = 10}
-- Prelude> foldMap Product [1, 2, 3, 4]
-- Product {getProduct = 24}
-- Prelude> foldMap All [True, False, True]
-- All {getAll = False}
-- Prelude> foldMap Any [(3 == 4), (9 > 5)]
-- Any {getAny = True}
-- Prelude> xs = [Just 1, Nothing, Just 5]
-- Prelude> foldMap First xs

-- can also have a fn diff from monoid being used
-- in which case the fn is mapped to each value first, then monoid instance used to reduce
-- to a summary value:
-- Prelude> xs = map Sum [1..3]
-- Prelude> foldMap (*5) xs
-- Sum {getSum = 30}
-- 5 + 10 + 15
-- 30

-- Declaring monoid instance doesn't have an impact if there's only one value (no folding occurs):
-- Prelude> fm = foldMap (*5)
-- Prelude> fm (Just 100) :: Product Integer
-- Product {getProduct = 500}
-- Prelude> fm (Just 5) :: Sum Integer
-- Sum {getSum = 25}

-- But mempty value from monoid instance will be used for empty structures:
-- Prelude> fm Nothing :: Sum Integer
-- Sum {getSum = 0}
-- Prelude> fm Nothing :: Product Integer
-- Product {getProduct = 1}


data Identity a =
  Identity a
  deriving (Eq, Show)

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

data Optional a =
  Nada 
  | Yep a
  deriving (Eq, Show)

instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Yep x) = f x z
  foldl _ z Nada = z
  foldl f z (Yep x) = f z x
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

-- | The largest element
-- of a non-empty structure.
-- maximum :: Ord a => t a -> a
-- | The least element
-- of a non-empty structure.
-- minimum :: Ord a => t a -> a

-- Left & Nothing are considered empty values for the above fns

-- Exercises

--1)
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

--2)
product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

--3)
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
-- elem x = foldr (\el acc -> (el == x) || acc) False 
elem' x = getAny . foldMap (\el -> Any (el == x))

--4)
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr fmin Nothing
  where 
    fmin x Nothing = Just x
    fmin x (Just y)  = Just (min x y)

--5)
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr fmax Nothing
  where 
    fmax x Nothing = Just x
    fmax x (Just y) = Just (max x y)

-- 6. 
null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True
-- if t a is empty, returns acc; otherwise fn is applied

-- 7. 
length' :: (Foldable t) => t a -> Int
length' = foldr (\_ acc -> acc + 1) 0
 
-- 8. Some say this is all Foldable amounts to:
toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

-- 9. Hint: use foldMap:
-- | Combine the elements
-- of a structure using a monoid.
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

-- 10. Define foldMap in terms of foldr:
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr ((<>) . f) mempty

-- Instances
-- 1. 
data Constant a b = Constant b deriving (Eq, Show)

-- with reference to instances for Identity & Optional:
instance Foldable (Constant a) where
  -- foldr :: (b -> c -> c) -> c -> Constant a b -> c
  foldr f z (Constant b) = f b z
  foldMap f (Constant b) = f b

-- 2. 
data Two a b = Two a b deriving (Eq, Show)

instance Foldable (Two a) where
  -- foldr :: (b -> c -> c) -> c -> Two a b -> c
  foldr f z (Two _ b) = f b z
  foldMap f (Two _ b) = f b

-- 3. 
data Three a b c = Three a b c deriving (Eq, Show)

instance Foldable (Three a b) where
  -- foldr :: (c -> d -> d) -> d -> Three a b c -> d
  foldr f z (Three _ _ c) = f c z
  foldMap f (Three _ _ c) = f c

-- 4. 
data Three' a b = Three' a b b deriving (Eq, Show)

instance Foldable (Three' a) where
  -- foldr :: (b -> c -> c) -> c -> Three' a b -> c
  foldr f z (Three' _ b b') = f b (f b' z) -- or: (f b . f c) z
  foldMap f (Three' _ b b') = f b <> f b'

-- 5. 
data Four' a b = Four' a b b b deriving (Eq, Show)

instance Foldable (Four' a) where
  -- foldr :: (b -> c -> c) -> c -> Four' a b -> c
  foldr f z (Four' _ b b' b'') = (f b . f b' . f b'') z -- or: f b $ f b' $ f b'' z
  foldMap f (Four' _ b b' b'') = f b <> f b' <> f b''
  

-- 6.
filterF :: ( Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\a -> if f a then pure a else mempty)