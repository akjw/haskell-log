-- Review of types
--1) d
--2) b
--3) d
--4) b

-- Reviewing currying
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

--1) "woops mrow woohoo!"
--2) "1 mrow haha"
--3) "woops mrow 2 mrow haha"
--4) "woops mrow blue mrow haha"
--5) 
-- (appedCatty "blue") == "woops mrow blue"
-- (cattyConny "green" "woops mrow blue") == "green mrow woops mrow blue"
-- (frappe "pink") == "pink mrow haha"
-- cattyConny "pink mrow haha" "green mrow woops mrow blue"
-- "pink mrow haha mrow green mrow woops mrow blue"
--6) "are mrow pugs mrow awesome"

-- Recursion
--1)
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n   d count
         | n < d = (count, n) -- base case
         | otherwise =
              go (n - d) d (count + 1)
-- dividedBy 15 2
--   go 15 2 0
--   go 13 2 1
--   go (13 - 2) 2 (1 + 1)
--   go 11 2 2
--   go (11 - 2) 2 (2 + 1)
--   go 9 2 3
--   go (9 - 2) 2 (3 + 1)
--   go 7 2 4
--   go (7 - 2) 2 (4 + 1)
--   go 5 2 5
--   go (5 - 2) 2 (5 + 1)
--   go 3 2 6
--   go (3 - 2) 2 (6 + 1)
--   go 1 2 7
  -- n < d
  -- return result
  -- (7, 1)


--2)
recursiveSum :: (Eq a, Num a) => a -> a
recursiveSum 1 = 1
recursiveSum n = n + recursiveSum (n - 1)
--3)
multRecursive :: (Integral a) => a -> a -> a
multRecursive x 0 = 0
multRecursive x y = x + multRecursive x (y - 1) 

-- Fixing dividedby
data DividedResult = Result Integer | DividedByZero deriving Show
dividedByRevised :: Integer -> Integer -> DividedResult
dividedByRevised _ 0 = DividedByZero
dividedByRevised num denom = go (abs num) (abs denom) 0 ((signum num) * (signum denom))
-- use signum to give appropriate sign
  where go n d count sign
         | n < d = Result (sign * count) -- base case
         | otherwise =
              go (n - d) d (count + 1) sign


-- McCarthy91
mc91 :: (Num a, Ord a) => a -> a
mc91 n 
  | n <= 100 = mc91 (mc91 (n + 11))
  | otherwise = n - 10