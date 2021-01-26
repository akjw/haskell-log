import Data.Char ( toLower )

applyFnToAll _ []     = []
applyFnToAll f (x:[])  = f x : []
applyFnToAll f (x:xs) = f x : applyFnToAll f xs 

-- filter (\(x:xs) -> x == 'a') ["apple","banana","avocado"]

remove test [] = []
remove test (x:xs) = if test x
                     then remove test xs
                     else x:remove test xs

sumOfSquares xs = foldl (+) 0 (map (^2) xs)


rcons x y = y:x
myReverse xs = foldl rcons [] xs

myFoldl f acc [] = acc
myFoldl f acc (x:xs) = myFoldl f newAcc xs
  where newAcc = f acc x

myFoldr f init [] = init
myFoldr f init (x:xs) = f x rightResult
  where rightResult = myFoldr f init xs

-- myFoldr (+) 0 [1, 2, 3] 
-- (+) 1 (myFoldr (+) 0 [2, 3])
-- (+) 1 ((+) 2 myFoldr (+) 0 [3])
-- (+) 1 ((+) 2 ((+) 3 myFoldr (+) 0 []))
-- (+) 1 ((+) 2 ((+) 3 0))
-- (+) 1 ((+) 2 3)
-- (+) 1 5
-- 6

myElem x xs = length (filter (== x) xs) > 0

myPalindrome xs = ls == reverse ls
  where ls = filter (/= ' ') $ map toLower xs

harmony n = sum (take n (map (1/) [1..]))


harmonic n = sum (take n seriesValues)
  where seriesPairs = zip (cycle [1.0]) [1.0,2.0 .. ]
        seriesValues = map (\pair -> (fst pair)/(snd pair)) seriesPairs