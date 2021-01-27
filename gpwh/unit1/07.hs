take' n ls = go n ls []
  where 
    go _ [] _ = []
    go 0 _ new = new
    go n ls new   = go (n-1) ls ((ls !! (n-1):new))


take1 _ [] = []
take1 n ls = case (n == 0) of
  True -> []
  False -> (take1 (n-1) ls) ++ [ls !! (n-1)]

myTail []     =  []
myTail (_:xs) = xs


myGCD a b = if remainder == 0
            then b
            else myGCD b remainder
  where remainder = a `mod` b

myGCD' a b = case (remainder == 0) of
              True -> b
              False -> myGCD' b remainder
  where remainder = a `mod` b

