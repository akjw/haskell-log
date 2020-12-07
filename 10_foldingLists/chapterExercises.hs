-- Understanding folds

--1) b & c
--2) 
{-
foldl (flip (*)) 1 [1..3]
(((1 `fm` 1) `fm` 2) `fm` 3)
((1 `fm` 2) `fc` 3)
2 `fm` 3 
6
-}

--3) c
--4) a
--5) 
  --a) foldr (++) "" ["woot", "WOOT", "woot"]
  --b) foldr max ' ' "fear is the little death"
  --c) foldr (&&) True [False, True]
  --d) No. If True is the base case, True || _ will always return True
  --e) foldr ((++) . show) " " [1..5]
  --f) foldl const 'a' [1..5]
  --g) foldl const 0 "tacos"
  --h) foldr (flip const) 0 "burritos"
  --i) foldr (flip const) 'z' [1..5]

-- Warm-up and review
--1)
  --a)
  {-
  stops = "pbtdkg"
  vowels = "aeiou"

  threeTup = [(x, y, z) | x <- stops, y <- vowels, z <- stops]
  -}

  --b) threeTupP = [(x, y, z) | x <- stops, y <- vowels, z <- stops, x == 'p']

  --c) 
  -- nouns = ["doggo", "catto", "gecko"]
  -- verbs = ["licks", "scratches", "cuddles"]
  -- threeTup' = [(x, y, z) | x <- nouns, y <- verbs, z <- nouns]

--2) 
{-
seekritFunc :: String -> Int
seekritFunc x = 
  div (sum (map length (words x)))
      (length (words x))

The function gives the average length of words in the list created by (words x).
-}

--3)
-- seekritFracFunc :: Fractional a => String -> a
-- seekritFracFunc x = (fromIntegral (sum (map length (words x)))) / (fromIntegral (length (words x)))

-- Rewriting functions using folds
--1)
myOr :: [Bool] -> Bool
myOr = foldr (||) False 

--2)
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b -> f a || b) False

--3) 
myElem :: Eq a => a -> [a] -> Bool
myElem el = foldr (\a b -> a == el || b) False 

myElem' :: Eq a => a -> [a] -> Bool
myElem' el = any (== el)

--4)
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

--5)
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> (f a) : b) []

--6)
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if (f a) then a:b else b) []

--7) 
squish :: [[a]] -> [a]
squish = foldr (++) []

--8)
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a b -> (f a) ++ b) []

--9) 
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

--10) 
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f ls = foldr (\a b -> if (f a b == GT) then a else b) (last ls) ls
-- myMaximumBy f ls = foldl (\b a -> if (f b a == GT) then b else a) (head ls) ls

--11)
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f ls = foldr (\a b -> if (f a b == LT) then a else b) (last ls) ls



