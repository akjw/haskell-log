import Control.Monad
import Control.Monad.Trans.State 
import qualified Data.DList as DL


fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5 == 0 = "Buzz"
           | n `mod` 3 == 0 = "Fizz"
           | otherwise = show n

-- fizzbuzzList :: [Integer] -> [String]
-- fizzbuzzList list =
--   execState (mapM_ addResult list) []

-- addResult :: Integer -> State [String] ()
-- addResult n = do
--   xs <- get
--   let result = fizzBuzz n
--   put (result : xs)

-- Not ideal: reversing list + won't terminate on infinite list
-- main :: IO ()
-- main =
--   mapM_ putStrLn $
--   reverse $ fizzbuzzList [1..100]

-- difference list allows appending in constant time
-- fizzbuzzList :: [Integer] -> [String]
-- fizzbuzzList list =
--   let dlist =
--         execState (mapM_ addResult list)
--                   DL.empty
--   -- convert back to normal list
--   in DL.apply dlist [] 

-- using DList's foldable instance to convert to list:
fizzbuzzList :: [Integer] -> DL.DList String
fizzbuzzList list =
  execState (mapM_ addResult list) DL.empty

addResult' :: Integer -> State (DL.DList String) ()
addResult' n = do
  xs <- get
  let result = fizzBuzz n
  -- snoc appends to the end, unlike
  -- cons which adds to the front
  put (DL.snoc xs result)

main :: IO ()
main =
  mapM_ putStrLn $ fizzbuzzList [1..100]

-- use consing to build result list (addResult), but output in the right order by enumerating the sequence backward. 
fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo first last = fizzbuzzList $ enumFromThenTo last (last - 1) first

