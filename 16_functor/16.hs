{-# HLINT ignore #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

import GHC.Arr

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

data Wrap f a =
  Wrap (f a)
  deriving (Eq, Show)

instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)

-- getLine :: IO String
-- read :: Read a => String -> a

getInt :: IO Int
getInt = fmap read getLine

-- Prelude> fmap (+1) getInt
-- 10
-- 11
-- Prelude> fmap (++ " and me too!") getLine
-- hello
-- "hello and me too!"

meTooIsm :: IO String
meTooIsm = do
  input <- getLine
  return (input ++ "and me too!")

bumpIt :: IO Int
bumpIt = do
  intVal <- getInt
  return (intVal + 1)

type Nat f g = forall a . f a -> g a

-- This'll work
maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- This will not work, not allowed
-- degenerateMtl :: Nat Maybe []
-- degenerateMtl Nothing = []
-- degenerateMtl (Just a) = [a + 1]

data Tuple a b =
  Tuple a b
  deriving (Eq, Show)

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

-- this works, goofy as it looks
instance Functor (Flip Tuple a) where
  fmap f (Flip (Tuple a b)) =
    Flip $ Tuple (f a) b

-- Chapter exercises
-- 1. data Bool = False | True
-- No, nothing to lift over

-- 2. data BoolAndSomethingElse a = False' a | True' a
-- yes

-- 3. data BoolAndMaybeSomethingElse a = Falsish | Truish a
-- yes

-- 4. newtype Mu f = InF { outF :: f (Mu f) }
-- no, because kind is (* -> *) -> *

-- 5. data D = D (Array Word Word) Int Int
-- No, nothing to lift over

--1)
data Sum' a b =
    One a
  | Too b

instance Functor (Sum' e) where
  fmap _ (One a) = One a
  fmap f (Too b) = Too (f b)

--2)
data Company a b c =
  DeepBlue a c
  | Something b

instance Functor (Company e e') where
  fmap _ (Something b) = Something b
  fmap f (DeepBlue a c) = DeepBlue a (f c)

--3)
data More b a =
  L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More b) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- write functor instances
--1)
data Quant a b =
    Finance
  | Desk a
  | Bloor b

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

--2)
data K a b =
  K a

instance Functor (K a) where
  fmap _ (K a) = K a

--3)
newtype Flip' f a b =
  Flip' (f b a)
  deriving (Eq, Show)
  
newtype K' a b =
  K' a

instance Functor (Flip' K' a) where
   fmap f (Flip' (K' a)) =
    Flip' $ K' (f a)

--4)
data EvilGoateeConst a b =
  GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

--5)
data LiftItOut f a =
  LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

--6)
data Parappa f g a =
  DaWrappa (f a) (g a)

instance (Functor f , Functor g) => Functor (Parappa f g) where
  fmap fn (DaWrappa fa ga) = DaWrappa (fmap fn fa) (fmap fn ga)

--7)
data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap fn (IgnoringSomething fa gb) = IgnoringSomething fa (fmap fn gb)

--8)
data Notorious g o a t =
  Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap fn (Notorious go ga gt) = Notorious go ga (fmap fn gt)

--9)
data List a =
    Nil
  | Cons a (List a)

instance Functor List where 
  fmap _ Nil = Nil
  fmap f (Cons a lsa) = Cons (f a) (fmap f lsa)

--10)
data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)

instance Functor GoatLord where 
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats gla gla' gla'') = MoreGoats (fmap f gla) (fmap f gla') (fmap f gla'')

--11)
data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)


instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print str a) = Print str (f a)
  fmap f (Read str2a) = Read (fmap f str2a)