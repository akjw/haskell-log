{- HLINT Ignore -}
{-# LANGUAGE InstanceSigs #-}
import Control.Applicative
import Data.Char


boop = (*2)
doop = (+10)

-- Functorial context 
-- result of one fn passed to the other
bip :: Integer -> Integer
bip = boop . doop

bloop :: Integer -> Integer
bloop = fmap boop doop
-- fmap boop doop x == (*2) ((+10) x)

-- Applicative context: (lift over partially applied fns)
-- argument is passed to both boop & doop in parallel, after which the results are added 
bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop
-- ((+) <$> (*2) <*> (+10)) 3
-- (3*2) + (3+10)
-- 6 + 13
-- 19


-- ((+) <$> (*2)) 5 3
-- Keeping in mind that this is
-- the (.) operator under the hood
-- ((+) . (*2)) 5 3
-- f . g = \x -> f (g x)
-- ((+) . (*2)) == \x -> (+) (2 * x)

-- ((+) . (*2)) 5 3
-- (\x -> (+) (2 * x)) 5 3
-- (\5 -> (+) (2 * 5)) 3
-- ((+) 10) 3

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop






cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupledDo :: [Char] -> ([Char], [Char])
tupledDo = do 
  c <- cap 
  r <- rev 
  return (c, r)

tupledBind :: String -> (String, String)
tupledBind = 
  cap >>= (\a 
    -> rev >>= \b 
      -> return (a, b))

-- functor instance for fns is basically fn composition
-- instance Functor ((->) r) where
--   fmap = (.)

-- ((->) r)
-- is
-- r ->

  -- r is part of the structure being lifted over 

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  -- fmap :: (a -> b)
  --     -> Reader r a
  --     -> Reader r b
  fmap f (Reader ra) =
    Reader $ \r -> f (ra r)
-- same as (.)
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f (g x)

-- \r -> f (ra r)
-- ra :: r -> a, f :: a -> b, ra r :: a, f (ra r) :: b

-- \x -> f (g x)

-- another version of an instance:
-- instance Functor (Reader r) where
--   fmap :: (a -> b)
--       -> Reader r a
--       -> Reader r b
--   fmap f (Reader ra) =
--     Reader $ (f . ra) 
    -- compose & apply first, then feed result back to Reader constructor

ask :: Reader a a
ask = Reader id

-- Applicative of fns
-- Applicative f =>
-- f ~ (->) r

-- pure :: a -> f a
-- pure :: a -> (r -> a)

-- (<*>) :: f (a -> b) -> f a -> f b
-- (<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)

newtype HumanName =
  HumanName String
  deriving (Eq, Show)

newtype DogName =
  DogName String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)

data Person =
  Person {
  humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog =
  Dog {
  dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

pers :: Person
pers =
  Person (HumanName "Big Bird")
  (DogName "Barkley")
  (Address "Sesame Street")

chris :: Person
chris =
  Person (HumanName "Chris Allen")
  (DogName "Papu")
  (Address "Austin")

-- without Reader
getDog :: Person -> Dog
getDog p =
  Dog (dogName p) (address p)

-- with Reader
getDogR :: Person -> Dog
getDogR =
  Dog <$> dogName <*> address

(<$->>) :: (a -> b)
  -> (r -> a)
  -> (r -> b)
(<$->>) = (<$>)

-- <$->> :: (DogName -> (Address -> Dog))
--     -> (Person -> DogName)
--     -> (Person -> (Address -> Dog))

(<*->>) :: (r -> a -> b)
  -> (r -> a)
  -> (r -> b)
(<*->>) = (<*>)

-- <*->> :: (Person -> (Address -> Dog))
--     -> (Person -> Address)
--     -> (Person -> Dog)

-- with Reader
getDogR' :: Person -> Dog
getDogR' =
  Dog <$->> dogName <*->> address

-- liftA2 :: Applicative f =>
--   (a -> b -> c)
--   -> f a -> f b -> f c

-- with Reader, alternative
getDogR1 :: Person -> Dog
getDogR1 =
  liftA2 Dog dogName address

-- Reading comprehension

-- 1) 

myLiftA2 :: Applicative f =>
  (a -> b -> c)
  -> f a -> f b -> f c
myLiftA2 f a b = f <$> a <*> b

-- 2 )

asks :: (r -> a) -> Reader r a
asks f = Reader f

-- 3)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a

  (<*>) :: Reader r (a -> b)
    -> Reader r a
    -> Reader r b
  (Reader rab) <*> (Reader ra) =
    Reader $ \r -> rab r (ra r)
-- type needed = r -> b
-- rab :: r -> a -> b
-- ra :: r -> a
-- rab r :: a -> b
-- ra r :: a
-- rab r (ra r) :: b
-- \r -> rab r (ra r) :: r -> b

-- Monad of fns

foo :: (Functor f, Num a) => f a -> f a
foo r = fmap (+1) r

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

-- combines the above
froot :: Num a => [a] -> ([a], Int)
froot r = (map (+1) r, length r)

-- rewrite bar with 1 arg
barOne :: Foldable t => t a -> (t a, Int)
barOne r = (r, length r)

-- more compact combined version
barPlus :: (Functor t, Num a, Foldable t) => t a -> (t a, Int)
barPlus r = (foo r, length r)

-- even more compact
frooty :: Num a => [a] -> ([a], Int)
frooty r = bar (foo r) r

-- make it more Reader-y
frooty' :: Num a => [a] -> ([a], Int)
frooty' = \r -> bar (foo r) r

-- abstracted; generalized:
fooBind :: (r -> a)
  -> (a -> r -> b)
  -> (r -> b)
fooBind m k = \r -> k (m r) r

-- Note similarity with bind
-- (>>=) :: Monad m =>
--   m a -> (a -> (m b)) -> m b
--   (r -> a) -> (a -> (r -> b)) -> (r -> b)

-- Monad instance

-- return :: Monad m => a -> m a
-- return :: a -> r -> a

getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dog name addy

-- Reader Monad
-- 1)

-- Don't forget InstanceSigs.
instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a
        -> (a -> Reader r b)
        -> Reader r b
  (Reader ra) >>= aRb =
    Reader $ \r -> runReader (aRb (ra r)) r
-- type needed = r -> b
-- aRb :: (a -> Reader r b)
-- ra :: r -> a
-- ra r :: a
-- aRb (ra r) -> Reader r b
-- runReader :: Reader r b -> r -> b
-- runReader (aRb (ra r)) r :: b
-- \r -> runReader (aRb (ra r)) r :: r -> b

-- getDogRM with Reader
getDogRM' :: Person -> Dog
getDogRM' =
  dogName >>= \name ->
    address >>= \ addy ->
      return $ Dog name addy