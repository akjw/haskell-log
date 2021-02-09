import System.Environment
import Control.Monad


-- uses mapM to call getLine three times, and then uses
-- mapM_ to print out the values’ input. 
-- need to throw away an argument when using mapM with getLine
exampleMain :: IO ()
exampleMain = do
  vals <- mapM (\_ -> getLine) [1..3]
  mapM_ putStrLn vals

main :: IO ()
main = do
  args <- getArgs
  let linesToRead = if length args > 0
                    then read (head args)
                    else 0 :: Int
  print linesToRead

main1 :: IO ()
main1 = do
  args <- getArgs
  let linesToRead = if length args > 0
                      then read (head args)
                      else 0
  numbers <- replicateM linesToRead getLine
  let ints = map read numbers :: [Int]
  print (sum ints)

-- replicateM Takes an IO action, an Int n
-- repeats the IO action n times
-- returns the results in an IO list
myReplicateM n func = mapM (\_ -> func) [1 .. n]

