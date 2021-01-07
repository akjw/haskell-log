-- Determine the kinds
--1)
-- a :: *  (since a would be a concrete type provided to id)
--2)
-- a :: *
-- f :: * -> * (f is a typeclass, not a concrete type)

-- String processing
--1)
import Data.Char
notThe :: String -> Maybe String
notThe x = case x=="the" of
  True -> Nothing
  False -> Just x


replaceThe :: String -> String
replaceThe str = unwords . f $ words str
    where 
      f [] = []
      f (x:xs) 
        | notThe x == Nothing = "a" : f xs
        | notThe x == Just x = x : f xs

--2)

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel str = go (words str) 0 False
    where 
      go [] count True = count
      go [] _ False    = 0
      go (x:xs) count hasVowelLeadingWord
        | isVowel (head x) = go xs count True
        | x == "the" = go xs (count+1) hasVowelLeadingWord
        | otherwise = go xs count hasVowelLeadingWord

isVowel :: Char -> Bool
isVowel x = elem x "aeiou"

--3)
countVowels :: String -> Integer
countVowels str = go (concat $ words str) 0
    where 
      go [] count = count
      go (x:xs) count
        | isVowel $ toLower x = go xs count+1
        | otherwise = go xs count

countVowels' :: String -> Int
countVowels' str = length . -- count num of vowels
                   filter isVowel -- test for vowelhood & return vowels of str
                   $ map toLower str -- convert to lowercase

-- Validate the word
newtype Word' = Word' String deriving (Eq, Show)
vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord str = go str 0 0
  where 
    go [] cNum vNum =     
      case cNum > vNum of
        True -> Just (Word' str)
        False -> Nothing
    go (x:xs) cNum vNum 
       | isVowel $ toLower x = go xs cNum (vNum+1)
       | otherwise = go xs (cNum+1) vNum
 
 -- it's only natural
data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

-- natToInteger (Succ (Succ (Succ Zero)))
-- = 1 + natToInteger (Succ (Succ Zero))
-- = 1 + 1 + natToInteger (Succ Zero)
-- = 1 + 1 + 1 + natToInteger Zero
-- = 1 + 1 + 1 + 0
-- = 3

integerToNat :: Integer -> Maybe Nat
integerToNat x = 
  case x < 0 of
    True -> Nothing
    False -> Just (go x)
    where 
      go 0 = Zero
      go y = Succ $ go (y-1)

-- integerToNat 3 
-- = Just (go 3)                  -- Just (Succ (Succ (Succ Zero)))
-- = Succ $ go 2                  -- Succ (Succ (Succ Zero))
--          Succ $ go 1           -- Succ (Succ Zero)
--                 Succ $ go 0    -- Zero
  

-- Small library for Maybe
--1)

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False


isNothing :: Maybe a -> Bool
isNothing (Just _) = False
isNothing Nothing = True

--2)

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ f (Just x) = f x
mayybee def _ Nothing = def

--3)
fromMaybe :: a -> Maybe a -> a
fromMaybe x = mayybee x id 

--4)
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe xs = Just (head xs)


maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList Nothing = []

--5)
catMaybes :: Eq a => [Maybe a] -> [a]
catMaybes xs = map filterOut $ filter (/= Nothing) xs
  where filterOut (Just a) = a

catMaybes' :: [Maybe a] -> [a]
catMaybes' ls = go ls []
  where 
    go [] acc = acc
    go (x:xs) acc = case x of
                     Nothing -> go xs acc
                     Just a -> go xs (acc ++ [a])
--6) 
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe ls = go ls []
  where 
    go [] acc = Just acc
    go (x:xs) acc = case x of
                     Nothing -> Nothing
                     Just a -> go xs (acc ++ [a])

-- Small library for Either
--1)
-- lefts' :: [Either a b] -> [a]
-- lefts' ls = go ls []
--   where 
--     go [] acc = acc
--     go (x:xs) acc = case x of
--                      Right _ -> go xs acc
--                      Left a -> go xs (acc ++ [a])

lefts' :: [Either a b] -> [a]
lefts' = foldr (\x acc -> case x of Left a -> [a] ++ acc; Right _ -> acc) []

--2)
rights' :: [Either a b] -> [b]
rights' = foldr (\x acc -> case x of Right a -> [a] ++ acc; Left _ -> acc) []
                   

--3)
-- partitionEithers' :: [Either a b] -> ([a], [b])
-- partitionEithers' xs = (lefts, rights)
--   where
--     lefts = lefts' xs
--     rights = rights' xs

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr f ([],[]) 
  where
    f (Left x) (lefts, rights) = (x:lefts, rights)
    f (Right x) (lefts, rights) = (lefts, x:rights)

--4) 
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right x) = Just (f x)
eitherMaybe' _ (Left _) = Nothing

--5)
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' _ bToC (Right x) = bToC x
either' aToC _ (Left x) = aToC x

--6)
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f x = either' (const Nothing) (Just . f) x

-- Write your own iterate and unfoldr
--1)
myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

--2)
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case f x of
                  Nothing -> []
                  Just (a, b) -> a : myUnfoldr f b

-- from hackage source code:
-- >>> unfoldr (\b -> if b == 0 then Nothing else Just (b, b-1)) 10
-- [10,9,8,7,6,5,4,3,2,1]

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\x -> Just(x, f x)) x 

-- Finally something other than a list!
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
                    deriving (Eq, Ord, Show)

--1) 
unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f a = case f a of
                Nothing -> Leaf
                Just (x, y, z) -> Node (unfold f x) y (unfold f z)


treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\x -> case n > x of False -> Nothing; True -> Just (x+1, x, x+1)) 0

  --                                                                     treeBuild 3
  --                                                                     unfold f 0
  --                                                                     case 3 > 0 of True
  --                                                                     Just (1, 0, 1) 
  --                                                                              0
  --                              Node (unfold f 1)                                                                 (unfold f 1)
  --                              case 3 > 1 of True                                                                case 3 > 1 of True
  --                              Just (2, 1, 2)                                                                    ust (2, 1, 2)
  --                                       1                                                                                1
  --           Node (unfold f 2)                         (unfold f 2)                       Node (unfold f 2)                        (unfold f 2)
  --           case 3 > 2 of True                        case 3 > 2 of True                      case 3 > 2 of True                   case 3 > 2 of True 
  --           Just (3, 2, 3)                            Just (3, 2, 3)                          Just (3, 2, 3)                       Just (3, 2, 3)  
  --                    2                                         2                                       2                                     2
  --  Node (unfold f 3)    (unfold f 3)          Node (unfold f 3)  (unfold f 3)        Node (unfold f 3)  (unfold f 3)      Node (unfold f 3)  (unfold f 3)
  --  case 3 > 3 of False  (...)                      (...)         (...)                     (...)        (...)                  (...)         (...)
  --  Nothing ->           Nothing ->       
  --  Leaf                   Leaf                    Leaf            Leaf                      Leaf          Leaf                   Leaf         Leaf   
                                                
                      
           
          
 
      