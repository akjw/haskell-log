calc :: [String] -> Int
calc (a:"+":b:xs) = read a + read b
calc (a:"*":b:xs) = read a * read b
calc o = error ("Invalid input: " ++ show o) -- only triggered with ctrl-d

main :: IO ()
main = do
  userInput <- getContents
  let values = lines userInput
  print (calc values)
