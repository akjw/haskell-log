awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]

-- 1) length :: [a] -> Int
-- 2) 
--   a) 5
--   b) 3
--   c) 2
--   d) 5
-- 3) 6 / length [1, 2, 3] does not compile because length returns an Int and '/' must be applied to a Fractional
-- 4) use 'div 6 (length [1, 2, 3])' instead. div has an Int instance
-- 5) Bool, True
-- 6) 
--   a) Num; no result
--   b) Bool, False
-- 7)
--   a) True
--   b) will not compile; lists can only contain one type
--   c) 5
--   d) False, since ('b' < 'a') == False
--   e) will not compile; 9 will not evaluate to a Bool
-- 8) 
-- 9)
-- 10)

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x =
  if x == reverse x
    then True
  else
    False

myAbs :: Integer -> Integer
myAbs x =
  if x < 0
    then abs x
  else
    x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))

-- Correcting Syntax
-- 1)
fa xs = x w 1
  where w = length xs
        x = (+)
-- use where to declare x
-- use lowercase f 

-- 2)
-- identity fn
identity x = x

-- 3)
firstInTup (a, b) = a

-- Match function names to types
-- 1) c
-- 2) b
-- 3) a
-- 4) d

