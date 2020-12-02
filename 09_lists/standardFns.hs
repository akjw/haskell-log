--1)
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

--2)
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || (myAny f xs)

--3)
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem el (x:xs) = (x == el) || (myElem el xs)

-- using any
anyElem :: Eq a => a -> [a] -> Bool
anyElem el list = any (==el) list

--4)
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (++) (myReverse xs) [x]
-- myReverse list = myReverse (tail list) ++ [head list]
-- myReverse (x:xs) = (myReverse xs) ++ [x]


--5)
squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ (squish xs)

--6) 
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) =  f x ++ (squishMap f xs)

-- reuse squish
-- squishMap f xs = squish (map f xs)

--7)
squishAgain :: [[a]] -> [a]
squishAgain =  squishMap (\y -> y) 

--8)
myMaximumBy :: (a ->  a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "list cannot be empty!"
myMaximumBy _ [x] = x
myMaximumBy f (x:xs) 
    | f x (myMaximumBy f xs) == GT = x
    | otherwise = myMaximumBy f xs

-- Using max value tracker:
-- myMaximumBy' :: (a -> a -> Ordering) -> [a] -> a
-- myMaximumBy' _ []  = error "list cannot be empty!"
-- myMaximumBy' f list = go f list (head list)
--   where
--     go f (x: []) max = max 
--     go f (x:xs) max 
--       | f x max == GT = go f xs x
--       | otherwise = go f xs max

-- Selecting pairs & recursively reducing list values to compare
-- myMaximumBy1 :: (a -> a -> Ordering)
--             -> [a] -> a
-- myMaximumBy1 _ [] = error "list cannot be empty!"
-- myMaximumBy1 _ (x:[]) = x
-- myMaximumBy1 f (x1:x2:xs)
--   | f x1 x2 == GT = myMaximumBy1 f (x1:xs)
--   | otherwise = myMaximumBy1 f (x2:xs)

--9)
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "list cannot be empty!"
myMinimumBy _ [x] = x
myMinimumBy f (x:xs) 
    | f x (myMinimumBy f xs) == LT = x
    | otherwise = myMinimumBy f xs

--10)
myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare

