module Cipher where

import Data.Char
-- cipher :: Int -> [Char] -> [Char]
-- cipher _ [] = []
-- cipher n str = encode n $ toNums str

cipher' :: Int -> String -> String
cipher' _ [] = []
cipher' n str = encode' n $ map toNum str


-- toNums :: String -> [Int]
-- toNums [] = []
-- toNums (x:xs) = (ord x - ord 'a') : toNums xs

toNum :: Char -> Int
toNum x = ord x - ord 'a'

-- encode :: Int -> [Int] -> String
-- encode n nums = toChars $ map (\x -> mod (x + n) 26) nums

encode' :: Int -> [Int] -> String
encode' n nums = map toChar $ map (\x -> mod (x + n) 26) nums

-- toChars :: [Int] -> String
-- toChars [] = []
-- toChars (x:xs) = (chr (x + ord 'a')) : toChars xs

toChar :: Int -> Char
toChar x = chr (x + ord 'a')

unCaesar :: Int -> String -> String
unCaesar _ [] = []
unCaesar n str = encode' (-n) $ map toNum str