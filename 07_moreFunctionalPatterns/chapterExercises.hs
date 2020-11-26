-- Grab bag

--1) a, b, c, d are all equivalent due to currying
--2) d) Num a => a -> a-> a
--3) 
--a)
addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = \n -> n + 1

--b)
addFive = \x -> \y -> (if x > y then y else x) + 5

--c)
mflip f x y = f y x

-- Variety pack

--1) 
-- a) k :: (a, b) -> a
-- b) k2 :: [Char] 
-- c) k1, k3

--2) 
f :: (a, b, c)
  -> (d, e, f)
  -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))

-- Case practice

--1)
functionC x y =
  case (x > y) of
    True -> x
    False -> y

--2)
ifEvenAdd2 n =
  case even n of
    True -> (n + 2)
    False -> n

--3)
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

-- Artful dodgy
dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2

--2) 11
--3) 22
--4) 21
--5) 12
--6) 11
--7) 21
--8) 21
--9) 22
--10) 31
--11) 23

-- Guard duty
--1) All arguments will return 'F' if otherwise is the top-most guard
--2) If | y >= 0.7 = 'C' is made the first guard instead, 90 will return 'C', not 'A', since the second guard will never be evaluated
--3) b
--4) Lists with elements that have Eq instance
--5) pal :: Eq a => [a] -> Bool
--6) c
--7) must have instance of Ord and Num
--8) numbers :: (Ord a, Num a, Num p) =>  a -> p

numbers x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1

-- MCQ
--1) d
--2) b
--3) d
--4) b
--5) a

-- Let's write code
--1a)
tensDigit :: Integral a => a -> a
tensDigit = (flip mod 10) . fst . (flip divMod 10)
--1b) yes, they have the same type
--1c)
hunsD :: Integral a => a -> a
hunsD = (flip mod 10) . fst . (flip divMod 100)
--2) 
foldBool :: a -> a -> Bool -> a
foldBool x y boo =
  case boo of
    True -> y
    False -> x 

foldBool' :: a -> a -> Bool -> a
foldBool' x y boo
  | boo = y
  | otherwise = x

--3)
g :: (a -> b) -> (a, c) -> (b, c)
g aToB (a, c) = ((aToB a), c)