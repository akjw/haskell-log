{- HLINT Ignore -}
import Data.Monoid
import Data.Foldable


-- instance Foldable Optional where
--   foldr _ z Nada = z
--   foldr f z (Yep x) = f x z
--   foldl _ z Nada = z
--   foldl f z (Yep x) = f z x
--   foldMap _ Nada = mempty
--   foldMap f (Yep a) = f a

-- | The largest element
-- of a non-empty structure.
-- maximum :: Ord a => t a -> a
-- | The least element
-- of a non-empty structure.
-- minimum :: Ord a => t a -> a

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
-- minimum = getAlt . foldMap (Alt . Just)
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
  foldr f z (Constant b) = f b z
  foldMap f (Constant b) = f b

-- 2. 
data Two a b = Two a b deriving (Eq, Show)

instance Foldable (Two a) where
  foldr f z (Two _ b) = f b z
  foldMap f (Two _ b) = f b

-- 3. 
data Three a b c = Three a b c deriving (Eq, Show)

instance Foldable (Three a b) where
  foldr f z (Three _ _ c) = f c z
  foldMap f (Three _ _ c) = f c

-- 4. 
data Three' a b = Three' a b b deriving (Eq, Show)

instance Foldable (Three' a) where
  foldMap f (Three' _ b c) = f b <> f c
  foldr f z (Three' _ b c) = f b (f c z) -- or: (f b . f c) z

-- 5. 
data Four' a b = Four' a b b b deriving (Eq, Show)

instance Foldable (Four' a) where
  foldMap f (Four' _ a b c) = f a <> f b <> f c
  foldr f z (Four' _ a b c) = (f a . f b . f c) z -- or: f a $ f b $ f c z

-- 6.
filterF :: ( Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\a -> case f a of True -> pure a; False -> mempty)