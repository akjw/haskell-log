-- filter :: (a -> Bool) -> [a] -> [a]

-- map :: (a -> b) -> [a] -> [b]

tail' :: [a] -> [a]
tail' [] = []
tail' (_:xs) = xs


head' :: [a] -> Maybe a
head' [] = Nothing
head' (x:_) = Just x

myFoldl :: (t -> a -> t) -> t -> [a] -> t
myFoldl _ init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
  where newInit = f init x  