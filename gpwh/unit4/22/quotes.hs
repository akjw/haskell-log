quotes :: [String]
quotes = ["hello"
          ,"it"
          ,"is"
          ,"me"
          ,"mario"]

lookupQuote :: [String] -> [String]
lookupQuote [] = []
lookupQuote ("n":_) = []
lookupQuote (x:xs) = if (read x - 1) `elem` [0..(length quotes - 1)] -- validation for the contrary user 
                     then quote : (lookupQuote xs)
                     else []
  where quote = quotes !! (read x - 1)
             

main :: IO ()
main = do
  putStrLn "Enter number between 1-5; n to exit: "
  userInput <- getContents
  mapM_ putStrLn (lookupQuote (lines userInput))
