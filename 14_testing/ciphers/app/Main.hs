module Main where
import Lib

main :: IO ()
main = do
  putStrLn "Enter shift number: "
  n <- getLine
  putStrLn "Enter string: "
  str  <- getLine
  putStrLn $ "Caesar: " ++ show (caesar str (read n))
  putStrLn "Enter keyword: "
  keyword <- getLine
  putStrLn "Enter string: "
  str  <- getLine
  if (length str) > (length keyword)   
        then  putStrLn $ "Vigenère: " ++ show (vigenere str keyword)
        else do  
            main  

