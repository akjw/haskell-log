import Test.QuickCheck
import Test.QuickCheck.Checkers

-- 1)

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where 
  fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
  foldr f z (Identity a) = f a z
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq



-- 2)
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  -- fmap :: (b -> c) -> Constant a b -> Constant a c
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  -- foldMap :: Monoid m => (b -> m) -> Constant a b -> m
  foldMap _ (Constant _) = mempty
  -- foldr :: (b -> c -> c) -> c -> Constant a b -> c
  foldr _ z (Constant _) = z

instance Traversable (Constant a) where
  -- traverse :: Applicative f => (b -> f c) -> Constant a b -> f (Constant a c)
  traverse _ (Constant x) = pure (Constant x)

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance (Eq a, Eq b) => EqProp (Constant a b) where
  (=-=) = eq

-- 3)
data Optional a =
    Nada
  | Yep a
  deriving (Eq, Ord, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary =  oneof [ return Nada
                    , Yep <$> arbitrary]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq


-- 4)
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Ord, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
  foldMap _ Nil = mempty 
  foldMap f (Cons x xs) = f x <> foldMap f xs

instance Traversable List where
  -- traverse :: Applicative f => (a -> f b) -> List a -> f (List b)
  -- Cons :: a -> List a -> List a
  -- fmap :: (a -> (List a -> List a)) -> f a -> f (List a -> List a)
  -- (<*>) :: f (List a -> List a) -> f (List a) -> f (List a)
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs
                          -- Cons <$> f x <*> traverse f (Cons a Nil)
                          -- Cons <$> f x <*> Cons <$> f a <*> pure Nil
                          -- Just (Cons x) <*> Just (Cons a) <*> Just Nil
                          -- Just (Cons x (Cons a Nil))


instance Arbitrary a => Arbitrary (List a) where
  arbitrary = oneof [ return Nil,
                      Cons <$> arbitrary <*> arbitrary ]

instance Eq a => EqProp (List a) where
  (=-=) = eq

-- 5)
data Three a b c = Three a b c deriving (Eq, Ord, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq


-- 6)
data Pair a b = Pair a b deriving (Eq, Ord, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
  foldMap f (Pair _ b) = f b

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq


-- 7)
data Big a b = Big a b b deriving (Eq, Ord, Show)

instance Functor (Big a) where
  fmap f (Big a b b') = Big a (f b) (f b')

instance Foldable (Big a) where
  foldMap f (Big _ b b') = f b <> f b'

instance Traversable (Big a) where
  -- traverse :: (b -> f c) -> Big a b -> f (Big a c)
  traverse f (Big a b b') = Big a <$> f b <*> f b'
                          -- Big :: a -> b -> b -> Big a b, f b :: f c, f b' :: f c

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq


-- 8)
data Bigger a b = Bigger a b b b deriving (Eq, Ord, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')

instance Foldable (Bigger a) where
  foldMap f (Bigger _ b b' b'') = f b <> f b' <> f b''

instance Traversable (Bigger a) where
  traverse f (Bigger a b b' b'') = Bigger a <$> f b <*> f b' <*> f b''

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

-- 9)

data S n a = S (n a) a deriving (Eq, Show)

instance ( Functor n
  , Arbitrary (n a)
  , Arbitrary a )
  => Arbitrary (S n a) where
arbitrary = S <$> arbitrary <*> arbitrary

instance ( Applicative n
  , Testable (n Property)
  , Eq a
  , Eq (n a)
  , EqProp a)
  => EqProp (S n a) where
(=-=) = eq

instance Functor n => Functor (S n) where
  fmap f (S na a) = S (fmap f na) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S na a) = foldMap f na <> f a

instance Traversable n => Traversable (S n) where
  -- traverse :: Applicative f => (b -> f c) -> S a b -> f (S a c)
  traverse f (S na a) = S <$> traverse f na <*> f a

-- 10)
data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf x) = Leaf (f x)
  fmap Node l m r = Node (fmap f l) (f m) (fmap f r)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node l m r) = foldMap f l <> f m <> foldMap f r

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node l m r) = Node <$> traverse f l <*> f m <*> traverse f r

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = oneof [ return Empty
                    , Leaf <$> arbitrary
                    , Node <$> arbitrary <*> arbitrary <*> arbitrary ]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq