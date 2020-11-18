module BuildingFunctions where

exclaim :: String -> String
exclaim str = str ++ "!"

fifth :: String -> Char
fifth str = str !! 4

drop9 :: String -> String
drop9 str = drop 9 str

thirdLetter :: String -> Char
thirdLetter x = x !! 2

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome" !! (x - 1)

rvrs :: String -> String
rvrs x = drop 9 x ++ take 4 (drop 5 x) ++ take 5 x

rvrs1 :: String -> String
rvrs1 x = awe ++ iz ++ cur
 where awe = drop 9 x
       iz  = take 4 (drop 5 x)
       cur = take 5 x