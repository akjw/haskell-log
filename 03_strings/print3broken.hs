module Print3Broken where

printSecond :: IO ()
printSecond = do
 putStrLn greeting

greeting :: String
greeting = "Yarrrrrr"

main :: IO ()
main = do
 putStrLn greeting
 printSecond