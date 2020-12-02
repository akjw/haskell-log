-- Thy fearful symmetry

--1) 
myWords :: String -> [String]
myWords [] = []
myWords (' ': xs) = myWords xs
myWords xs = x : myWords leftoverX
    where x = takeWhile (/= ' ') xs
          leftoverX = dropWhile (/= ' ') xs

--2)
--poemLines.hs

--3)
modularize :: Char -> String -> [String]
modularize c [] = []
modularize c (x:xs)
   | x == c = modularize c xs
   | otherwise = takeWhile (/= c) (x:xs) : (modularize c $ dropWhile (/= c) (x:xs))
  --  | otherwise = takeWhile (/= c) (x:xs) : (modularize c . dropWhile (/= c)) (x:xs)
  --  | otherwise = takeWhile (/= c) (x:xs) : modularize c (dropWhile (/= c) (x:xs))
   
myWords' :: String -> [String]
myWords' = modularize ' '

myLines' :: String -> [String]
myLines' = modularize '\n'

-- Comprehend thy lists
mySqr = [x^2 | x <- [1..10]]
-- [1,4,9,16,25,36,49,64,81,100]


-- [x | x <- mySqr, rem x 2 == 0]
-- [4, 16, 36, 64, 100]

-- [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]
-- [(1,64),(1,81),(1,100),(4,64),(4,81)
-- ,(4,100),(9,64),(9,81),(9,100),(16,64)
-- ,(16,81),(16,100),(25,64),(25,81),(25,100)
-- ,(36,64),(36,81),(36,100),(49,64),(49,81),(49,100)]

-- take 5 [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]
-- [(1,64),(1,81),(1,100),(4,64),(4,81)]

-- List Comprehension with Strings
-- myString xs = [x | x <- xs, elem x "aeiou"]
-- extracts vowels from a string

-- Square Cube
-- mySqr = [x^2 | x <- [1..5]]
-- myCube = [y^3 | y <- [1..5]]

--1)
-- myTups = [(x, y) | x <- mySqr, y <- myCube]

--2)
-- myTups = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

--3)
-- myLength = sum [length x | x <- myTups]

-- Bottom madness
--1) bottom (complete evaluation is forced)
--2) [1]
--3) bottom (sum is strict for values)
--4) 3
--5) bottom (since length cannot count undefined spine values)
--6) [2]
--7) bottom (complete evaluation is forced)
--8) [1]
--9) [1, 3]
--10) bottom (complete evaluation is forced)

-- Intermission: Is it in normal form?
-- I was confused about the use of "_" here
-- because I wasn't sure about the context of these expressions 
-- i.e. "_" as used in pattern-matching, to ignore? Or as used in 
-- :sprint, for unevaluated? 
-- This explanation was helpful: https://stackoverflow.com/questions/46004882/is-the-expression-b-in-normal-form-in-weak-head-normal-form


--1) NF
--2) WHNF
--3) Neither (outermost component is a function; 
--           arguments fully applied but not evaluated)
--4) Neither
--5) Neither
--6) Neither
--7) WHNF (outermost component is (,) is a data constructor)


-- More bottoms
--1) bottom (take 1 forces first cons to be evaluated)
--2) [2]
--3) bottom (second cons is forced)
--4) returns list of Bool values indicating if x in xs is an element of "aeiou"
--5) 
--a) [1,4,9,16,25,36,49,64,81,100]
--b) [1, 10, 20] ** minimum returns min value from list
--c) [15, 15, 15]
--6) map (\x -> bool "not vowel" "vowel" $ elem x "aeiou") ['a', 'b', 'c']


-- Filtering
--1) filter (\x -> (rem x 3 )== 0) [1..30]
-- OR: [x | x <- [1..30], (rem x 3)==0]
--2) length $ [x | x <- [1..30], (rem x 3)==0]
-- OR: (length . filter (\x -> (rem x 3 )== 0)) [1..30]
--3) myFilter xs = [x | x <- (words xs), not (elem x ["the", "a", "an"])]

-- Data.CHar
--2) getUpper xs = filter isUpper xs
--3) capitalize (x:xs) = toUpper x : xs
--4) 
-- capsAll :: String -> String
-- capsAll [] = []
-- capsAll (x:xs) = toUpper x: capsAll xs
--5)
-- capsFirst :: String -> Char
-- capFirst [] = []
-- capsFirst xs = toUpper $ head xs
--6) 
-- cfComposed xs = (toUpper . head) xs
-- cdPointfree = toUpper. head
