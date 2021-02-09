-- import System.Random
import qualified Data.Map as Map

minDie :: Int
minDie = 1

maxDie :: Int
maxDie = 6

-- main :: IO ()
-- main = do
-- dieRoll <- randomRIO (minDie,maxDie)
-- putStrLn (show dieRoll)

areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi * (size/2)^2

type Pizza = (Double,Double)

costPerInch :: Pizza -> Double
costPerInch (size, cost) = cost / areaGivenDiameter size

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 = if costP1 < costP2
                      then p1
                      else p2
  where costP1 = costPerInch p1
        costP2 = costPerInch p2

describePizza :: Pizza -> String
describePizza (size,cost) = "The " ++ show size ++ " pizza " ++
                            "is cheaper at " ++
                            show costSqInch ++
                            " per square inch"
  where costSqInch = costPerInch (size,cost)

main :: IO ()
main = do
  putStrLn "What is the size of pizza 1"
  size1 <- getLine
  putStrLn "What is the cost of pizza 1"
  cost1 <- getLine
  putStrLn "What is the size of pizza 2"
  size2 <- getLine
  putStrLn "What is the cost of pizza 2"
  cost2 <- getLine
  let pizza1 = (read size1, read cost1)
  let pizza2 = (read size2, read cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  putStrLn (describePizza betterPizza)

costData :: Map.Map Int Double
costData = Map.fromList [(1,18.0),(2,16.0)]

sizeData :: Map.Map Int Double
sizeData = Map.fromList [(1,20.0),(2,15.0)]

maybeMain :: Maybe String
maybeMain = do
  size1 <- Map.lookup 1 sizeData
  cost1 <- Map.lookup 1 costData
  size2 <- Map.lookup 2 sizeData
  cost2 <- Map.lookup 2 costData
  let pizza1 = (size1,cost1)
  let pizza2 = (size2,cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  return (describePizza betterPizza)

shini :: Map.Map Int String
shini = Map.fromList [(1, "Shini")]

helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

main1 :: Maybe String
main1 = do
  name <- Map.lookup 1 shini
  let statement = helloPerson name
  return statement

fib :: (Eq a, Num a, Num p) => a -> p
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

getFib :: IO ()
getFib = do
  putStrLn "Enter an integer n: "
  n <- getLine
  let num = fib (read n)
  putStrLn (show num)
