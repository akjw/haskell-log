-- Exercises: Be kind
-- 1)  *
-- 2) b :: * -> * ; T :: * -> *
-- 3) c :: * -> * -> *

data FixMePls a = FixMe
              | Pls a
              deriving (Eq, Show)

instance Functor FixMePls where
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls (f a)

-- instance Functor FixMePls where -- this throws an error: Functors work w / higher-kinded types; 
--   fmap =                        -- i.e. lift over structure (leaves structure untouched)
--     error                       -- but FixMePls has kind * 
--     "it doesn't matter, it won't compile"

-- fmap :: Functor f => (a -> b) -> f a -> f b
-- If we replace f with FixMePls:
-- (a -> b) -> FixMePls a -> FixMePls b
-- But FixMePls doesn’t take type arguments

data WhoCares a =
  ItDoesnt
  | Matter a
  | WhatThisIsCalled
  deriving (Eq, Show)

instance Functor WhoCares where
  fmap _ ItDoesnt = ItDoesnt
  fmap _ WhatThisIsCalled = WhatThisIsCalled
  fmap f (Matter a) = Matter (f a)

data CountingGood a =
  Heisenberg Int a
  deriving (Eq, Show)
-- Totes cool
instance Functor CountingGood where
  fmap f (Heisenberg n a) = Heisenberg n (f a) -- treat Int (n) as part of structure to be lifted over

-- Exercises: Heavy lifting
-- 1) a = (+1) $ read "[1]" :: [Int]
a :: [Int]
-- a = (+1) <$> read "[1]" :: [Int] 
a = fmap (+1) (read "[1]" :: [Int])

--2) b = (++ "lol") (Just ["Hi,", "Hello"])
b :: Maybe [[Char]]
b = (fmap. fmap) (++ "lol") (Just ["Hi,", "Hello"])

--3) c = (*2) (\x -> x - 2)
c :: Integer -> Integer
c = (*2) . (\x -> x - 2)

--4) d = ((return '1' ++) . show) (\x -> [x, 1..3])
d = ((return '1' ++) . show) . (\x -> [x, 1..3])

--5) 
e :: IO Integer
-- e = let ioi = readIO "1" :: IO Integer
--         changed = read ("123"++) show ioi
--     in (*3) changed
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . ("123"++) . show) ioi
    in fmap (*3) changed

-- data Two a b =
--   Two a b
--   deriving (Eq, Show)

data Or a b =
  First a
  | Second b
  deriving (Eq, Show)

-- instance Functor (Two a) where
--   fmap f (Two a b) = Two a (f b)

instance Functor (Or a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

-- quickcheck properties
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b)
                                        -> (b -> c)
                                        -> f a
                                        -> Bool
functorCompose f g x = fmap g (fmap f x) == fmap (g . f) x

-- Exercises: Instances of Func

-- 1. 
newtype Identity a = Identity a
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

-- 2. 
data Pair a = Pair a a
instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

-- 3. 
data Two a b = Two a b
instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

-- 4. 
data Three a b c = Three a b c
instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

-- 5. 
data Three' a b = Three' a b b
instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

-- 6. 
data Four a b c d = Four a b c d
instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

-- 7. 
data Four' a b = Four' a a a b
instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

-- Ignoring possibilities
incIfJust :: Num a => Maybe a -> Maybe a
incIfJust (Just n) = Just $ n + 1
incIfJust Nothing = Nothing

showIfJust :: Show a
  => Maybe a
  -> Maybe String
showIfJust (Just s) = Just $ show s
showIfJust Nothing = Nothing

incMaybe :: Num a => Maybe a -> Maybe a
incMaybe m = fmap (+1) m

showMaybe :: Show a
  => Maybe a
  -> Maybe String
showMaybe s = fmap show s

-- with eta reduction (don't name arguments):
incMaybe'' :: Num a => Maybe a -> Maybe a
incMaybe'' = fmap (+1)

showMaybe'' :: Show a
  => Maybe a
  -> Maybe String
showMaybe'' = fmap show

-- even more generic version: abstracting beyond just Maybe:
-- "lifted" because they're lifted
-- over some structure f
liftedInc :: (Functor f, Num b)
  => f b -> f b
liftedInc = fmap (+1)

liftedShow :: (Functor f, Show a)
  => f a -> f String
liftedShow = fmap show

-- Prelude> liftedInc (Just 1)
-- Just 2
-- Prelude> liftedInc Nothing
-- Nothing
-- Prelude> liftedShow (Just 1)
-- Just "1"
-- Prelude> liftedShow Nothing
-- Nothing

-- Exercise: Possibly
data Possibly a =
    LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers a) = Yeppers (f a)

-- Either
incIfRight :: Num a
  => Either e a
  -> Either e a
incIfRight (Right n) = Right $ n + 1
incIfRight (Left e) = Left e

showIfRight :: Show a
  => Either e a
  -> Either e String
showIfRight (Right s) = Right $ show s
showIfRight (Left e) = Left e

-- do away with need for handling error case w/ fmap
-- eta-reduced form
incEither' :: Num a
  => Either e a
  -> Either e a
incEither' = fmap (+1)

showEither' :: Show a
  => Either e a
  -> Either e String
showEither' = fmap show

-- generic version
-- f ~ Either e
liftedInc' :: (Functor f, Num b)
  => f b -> f b
liftedInc' = fmap (+1)

liftedShow' :: (Functor f, Show a)
  => f a -> f String
liftedShow' = fmap show

data Sum a b =
    First' a
  | Second' b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First' a) = First' a
  fmap f (Second' b) = Second' (f b)

-- First / Left cannot be lifted

