import System.Environment
import Control.Monad

-- uses mapM to call getLine three times, and then uses
-- mapM_ to print out the values’ input. 
-- need to throw away an argument when using mapM with getLine
exampleMain :: IO ()
exampleMain = do
  vals <- mapM (\_ -> getLine) [1..3]
  mapM_ putStrLn vals

main' :: IO ()
main' = do
  args <- getArgs -- expects user to enter numbers to sum as arguments
                  -- eg. ./sum  2 5 6
  mapM_ putStrLn args -- prints all numbers entered

main :: IO ()
main = do
  args <- getArgs -- expects user to enter number of arguments 
                  -- eg. ./sum 2 (give me the sum of 2 numbers)
  let linesToRead = if length args > 0
                    then read (head args) 
                    else 0
  numbers <- replicateM linesToRead getLine -- replicateM 2 getLine (call getLine twice)
  let ints = map read numbers :: [Int]
  print (sum ints)

-- replicateM Takes an IO action, an Int n
-- repeats the IO action n times
-- returns the results in an IO list
myReplicateM n func = mapM (\_ -> func) [1 .. n]

