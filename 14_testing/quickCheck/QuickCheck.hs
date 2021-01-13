module QuickCheck where
import Test.QuickCheck 
import Data.List (sort)
import Data.Char (toUpper)

-- stack ghci

--1)
-- for a function
half :: Fractional a => a -> a
half x = x / 2
-- this property should hold
halfIdentity :: Double -> Double
halfIdentity = (*2) . half


prop_halfIdentity :: Double -> Bool
prop_halfIdentity x = halfIdentity x == x

halfIdentityQc :: IO ()
halfIdentityQc = quickCheck prop_halfIdentity

--2)
-- for any list you apply sort to,
-- this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, _) = (Just y, x >= y)

-- listOrdered [2, 1] =
--   snd $ foldr go (Nothing, True) [1, 2]
--       -- go 1 (Nothing, True) = (Just 1, True)
--       -- go 2  (Just 1, True) = (Just y, 1 >= 2) = (Just y, False)
--   False

prop_listOrdered :: Ord a => [a] -> Bool
prop_listOrdered xs = listOrdered (sort xs)

prop_listOrderedInt :: [Int] -> Bool
prop_listOrderedInt = prop_listOrdered

prop_listOrderedDb :: [Double] -> Bool
prop_listOrderedDb = prop_listOrdered

prop_listOrderedChar :: [Char] -> Bool
prop_listOrderedChar = prop_listOrdered

listOrderedQc :: IO ()
listOrderedQc = do 
    quickCheck prop_listOrderedInt
    quickCheck prop_listOrderedDb
    quickCheck prop_listOrderedChar

--3)
plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z =
  x + (y + z) == (x + y) + z

plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y =
  x + y == y + x

prop_plusAssociative :: Int -> Int -> Int -> Bool
prop_plusAssociative = plusAssociative

prop_plusCommutative :: Int -> Int -> Bool
prop_plusCommutative = plusCommutative

plusAssocComQc :: IO ()
plusAssocComQc = do
            quickCheck prop_plusAssociative
            quickCheck prop_plusCommutative

--4)
multAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
multAssociative x y z =
  x * (y * z) == (x * y) * z

multCommmutative :: (Eq a, Num a) => a -> a -> Bool
multCommmutative x y =
  (x * y) == (y * x)

prop_multAssociative :: Integer -> Integer -> Integer -> Bool
prop_multAssociative = multAssociative

prop_multCommutative :: Integer -> Integer -> Bool
prop_multCommutative = multCommmutative

multAssocComQc :: IO ()
multAssocComQc = do
            quickCheck prop_multAssociative
            quickCheck prop_multCommutative

--5)
-- quot rem
prop_quotRem :: Integral a => a -> a -> Bool
prop_quotRem _ 0 = True
prop_quotRem x y = (quot x y) * y + (rem x y) == x

prop_divMod :: Integral a => a -> a -> Bool
prop_divMod _ 0 = True
prop_divMod x y =(div x y) * y + (mod x y) == x

--6)
-- (^) is neither associative nor commutative 

prop_expAssociative :: (Integral b1, Integral b2, Num a, Eq a) => a -> b2 -> b1 -> Bool
prop_expAssociative x y z = (x ^ y) ^ z == x ^ (y ^ z)

prop_expCommutative :: Integral b => b -> b -> Bool
prop_expCommutative x y = x ^ y == y ^ x

expPropQc :: IO ()
expPropQc = do
  quickCheck (prop_expAssociative :: Int -> Int -> Int -> Bool)
  quickCheck (prop_expCommutative :: Int -> Int -> Bool)

--7)
prop_eqReversal :: Eq a => [a] -> Bool
prop_eqReversal ls =  ((reverse . reverse) ls) == id ls

eqReversalQc :: IO ()
eqReversalQc = do
  quickCheck (prop_eqReversal :: [Int] -> Bool)
  quickCheck (prop_eqReversal :: [Char] -> Bool)

--8)
-- f $ a = f a
-- f . g = \x -> f (g x)
prop_fnApp :: Eq a => (t -> a) -> t -> Bool
prop_fnApp f x = (f $ x) == f x

prop_fnComp :: Eq a1 => (b -> a1) -> (a2 -> b) -> a2 -> Bool
prop_fnComp f g x = (f . g) x == (\y -> f (g y)) x

--9)

-- foldr (:) == (++) is False
prop_foldrConsEqPlus :: Eq a => [a] -> [a] -> Bool
prop_foldrConsEqPlus xs ys = foldr (:) xs ys == (++) xs ys

-- foldr (++) [] == concat
prop_foldrPlusEqConcat :: (Eq a, Foldable t) => t [a] -> Bool
prop_foldrPlusEqConcat xs = foldr (++) [] xs == concat xs

--10)
prop_takeLength :: Int -> [a] -> Bool
prop_takeLength  n xs = length (take n xs) == n

-- Fails if n > length of list
takeLengthQc :: IO ()
takeLengthQc = do
  quickCheck (prop_takeLength ::  Int ->  [Int] -> Bool)
  quickCheck (prop_takeLength ::  Int ->  [Char] -> Bool)
  quickCheck (prop_takeLength ::  Int ->  [Double] -> Bool)

--11)

prop_readShow :: (Eq a, Read a, Show a) => a -> Bool
prop_readShow x = (read (show x)) == x

readShowQc :: IO ()
readShowQc  = do
      quickCheck (prop_readShow :: Int    -> Bool)
      quickCheck (prop_readShow :: Char   -> Bool)
      quickCheck (prop_readShow :: Double -> Bool)
      quickCheck (prop_readShow :: String -> Bool)

-- for a function
square :: Num a => a -> a
square x = x * x
-- Why does this property not hold?
-- Examine the type of sqrt.
squareIdentity :: (Eq a, Floating a) => a -> Bool
squareIdentity x = (square . sqrt) x == x

-- fails because  sqrt gives an approximated answer (not perfectly precise)
squareQc :: IO ()
squareQc  = do
  quickCheck (squareIdentity :: Double -> Bool)
  quickCheck (squareIdentity :: Float -> Bool)

-- Idempotence
-- helper fns
twice :: (b -> b) -> b -> b
twice f = f . f

fourTimes :: (b -> b) -> b -> b
fourTimes = twice . twice

--1) 
prop_capIdempotence :: String -> Bool
prop_capIdempotence x =
  (capitalizeWord x
  == twice capitalizeWord x)
  &&
  (capitalizeWord x
  == fourTimes capitalizeWord x)

capIdempotenceQc :: IO ()
capIdempotenceQc = quickCheck prop_capIdempotence

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (' ':xs) = ' ' : capitalizeWord xs
capitalizeWord (x:xs) = toUpper x:xs


--2)
prop_sortIdempotence :: Ord a => [a] -> Bool
prop_sortIdempotence x =
  (sort x
  == twice sort x)
  &&
  (sort x
  == fourTimes sort x)

sortIdempotenceQc :: IO ()
sortIdempotenceQc = do
  quickCheck (prop_sortIdempotence :: [Int] -> Bool)
  quickCheck (prop_sortIdempotence :: [Double] -> Bool)
  quickCheck (prop_sortIdempotence :: [Float] -> Bool)
  quickCheck (prop_sortIdempotence :: [Char] -> Bool)

-- Make a Gen random generator for the datatype
--1) Equal probabilities for each:
data Fool =
    Fulse
  | Frue
  deriving (Eq, Show)

genFool :: Gen Fool
genFool = oneof [return $ Fulse, return $ Frue]
-- genFool = frequency [(1, return Fulse), (1, return Frue)]

--2) 2/3s chance of Fulse, 1/3 chance of Frue:
genFoolThirds  :: Gen Fool
genFoolThirds = frequency [(2, return Fulse), (1, return Frue)]


