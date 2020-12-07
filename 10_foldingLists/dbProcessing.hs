import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = 
  [ DbDate (UTCTime
           (fromGregorian 1911 5 1)
    (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbNumber 19001
  , DbString "Hello, world!"
  , DbDate (UTCTime
         (fromGregorian 1921 5 1)
    (secondsToDiffTime 34123))
   ]

--1)
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr f [] 
    where f (DbDate u) ls = [u] ++ ls
          f _          ls = ls

--2)
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr f [] 
    where f (DbNumber i) ls = [i] ++ ls
          f _            ls = ls

--3)
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr (\a b -> case a of {DbDate u -> max u b; _ -> b}) (UTCTime (fromGregorian 0 1 1) (secondsToDiffTime 0)) 


-- mostRecent = maximum . filterDbDate

-- mostRecent db = go compare (filterDbDate db) (head $ filterDbDate db)
--     where go compare [] mr = mr
--           go compare (x:xs) mr
--              | compare x mr == GT = go compare xs x
--              | otherwise = go compare xs mr


--4)
sumDb :: [DatabaseItem] -> Integer
sumDb = foldr (\a b -> case a of {DbNumber n -> n + b; _ -> b}) 0

-- sumDb = sum . filterDbNumber

-- sumDb = foldr sumNums 0
--   where sumNums (DbNumber n) z = (+) n z 
--         sumNums _ z         = z

--5)
avgDb :: [DatabaseItem] -> Double
avgDb db = sum / len
 where sum = fromIntegral . sumDb $ db
       len = fromIntegral . length . filterDbNumber $ db

