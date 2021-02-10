-- was initially convinced I had to use something akin to read but for operators
-- but this can be solved with good ol' pattern matching
calc :: [String] -> Int
calc (a:"+":b:_) = read a + read b
calc (a:"*":b:_) = read a * read b
calc o = error ("Invalid input: " ++ show o) -- only triggered with ctrl-d

main :: IO ()
main = do
  userInput <- getContents
  let values = lines userInput
  print (calc values)
