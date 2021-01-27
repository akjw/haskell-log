data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Show,Eq,Ord,Enum)

-- instance Show SixSidedDie where
--   show S1 = "one"
--   show S2 = "two"
--   show S3 = "three"
--   show S4 = "four"
--   show S5 = "five"
--   show S6 = "six"

-- instance Eq SixSidedDie where
--   (==) S6 S6 = True
--   (==) S5 S5 = True
--   (==) S4 S4 = True
--   (==) S3 S3 = True
--   (==) S2 S2 = True
--   (==) S1 S1 = True
--   (==) _ _ = False

-- instance Ord SixSidedDie where
--   compare S6 S6 = EQ
--   compare S6 _ = GT
--   compare _ S6 = LT
--   compare S5 S5 = EQ
--   compare S5 _ = GT
--   compare _ S5 = LT
--   compare S4 S4 = EQ
--   compare _ S4 = LT
--   compare S4 _ = GT

-- instance Enum SixSidedDie where
--   toEnum 0 = S1
--   toEnum 1 = S2
--   toEnum 2 = S3
--   toEnum 3 = S4
--   toEnum 4 = S5
--   toEnum 5 = S6
--   toEnum _ = error "No such value"
--   fromEnum S1 = 0
--   fromEnum S2 = 1
--   fromEnum S3 = 2
--   fromEnum S4 = 3
--   fromEnum S5 = 4
--   fromEnum S6 = 5

data TwoSidedDie = One | Two deriving (Enum)

instance Eq TwoSidedDie where
  (==) side1 side2 = (fromEnum side1) == (fromEnum side2)

instance Ord TwoSidedDie where
  compare side1 side2 = compare (fromEnum side1) (fromEnum side2)

data Test1 = AA | ZZ deriving (Eq, Ord)
data Test2 = ZZZ | AAA deriving (Eq, Ord)

data Name = Name (String, String) deriving (Show, Eq)

instance Ord Name where
  compare (Name (f1,l1)) (Name (f2,l2)) = compare (l1,f1) (l2,f2)

names :: [Name]
names = [Name ("Emil","Cioran")
        , Name ("Eugene","Thacker")
        , Name ("Friedrich","Nietzsche")]

data FiveSidedDie = F1 | F2 | F3 | F4 | F5 deriving (Eq,Enum)

instance Show FiveSidedDie where
  show F1 = "F1"
  show F2 = "F2"
  show F3 = "F3"
  show F4 = "F4"
  show F5 = "F5"


class (Eq a, Enum a) => Roll a where
  roll :: Int -> a

instance Roll FiveSidedDie where
  roll n = toEnum (mod n 5)

rollMe :: Int -> FiveSidedDie
rollMe = roll