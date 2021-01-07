import Data.Char

runCipher :: IO ()
runCipher = do
  putStrLn "Enter keyword: "
  keyword <- getLine
  putStrLn "Enter string: "
  str  <- getLine
  putStrLn $ "Vigenère: " ++ show (cipher keyword str)

cipher :: [Char] -> String -> [Char]
cipher keyword str = map toChar $ encode (cycleOffsetList keyword str) str

toNum :: Char -> Int
toNum x = ord x - ord 'A'

toChar :: Int -> Char
toChar 32 = chr 32
toChar x = chr (x + ord 'A')

-- Get list of offset values (match to length of string)
cycleOffsetList :: [Char] -> String -> [Int]
cycleOffsetList keyword str = take (length $ concat $ words str) . cycle $ map toNum keyword
-- cycleOffsetList keyword str = take (length $ concat $ words str) . cycle . concat $ map getLetterOffset keyword

encode :: [Int] -> [Char] -> [Int]
-- encode keylist str
encode [] _       = []
encode _ []       = []
encode (x:xs) (' ':ys) = 32 : encode (x:xs) ys
encode (x:xs) (y:ys) = (mod (x + (toNum y)) 26) : encode xs ys


-- Letter offset pairs
-- pairs :: [(Int, Char)]
-- pairs = zip [0..25] ['A'..'Z']


-- Get offset for each 
-- getLetterOffset :: Char -> [Int]
-- getLetterOffset letter = [fst (i, c) | (i, c) <- pairs, c == toUpper letter]
