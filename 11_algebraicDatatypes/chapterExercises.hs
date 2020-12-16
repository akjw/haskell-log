{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
import Data.Int
-- Exercises: Dog types
-- 1) Type constructor
-- 2) Doggies :: * -> *
-- 3) Doggies String :: *
-- 4) Husky 10 :: Num a => Doggies a
-- 5) Husky (10 :: Integer) :: Doggies Integer
-- 6) Mastiff "Scooby Doo" :: Doggies String
-- 7) Both
-- 8) DogueDeBordeaux :: doge -> DogueDeBordeaux doge
-- 9) DogueDeBordeaux "doggie!" :: DogueDeBordeaux String

-- Exercises: Cardinality

-- 1) 1
-- 2) 3
-- 3) 32767 + 32768 + 1 = 65536
-- 4) Integer is unbounded; min-max of Int is -9223372036854775808 and 9223372036854775807
-- 5) 8 in Int8 stands for 8 bits, 1 bit is binary (0-1), 2^8=256

-- Exercises: For example
data Example = MakeExample deriving Show
data Eg = MakeEg Int deriving Show
-- 1) MakeExample :: Example; querying type of Example throws error since Example is a type constructor, but query kind will work
-- 2) GHCi will show location of type declaration; there is an instance of Show
-- 3) MakeEg :: Int -> Eg

-- Exercises: Logic goats
class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

--1) 
newtype IntString = IntString (Int, String) 

instance TooMany IntString where
  tooMany (IntString (i, _)) = tooMany i

-- required FlexibleInstances
instance TooMany (Int, String) where
  tooMany (i, _) = tooMany i

-- 2)
instance TooMany (Int, Int) where
  tooMany (i1, i2) = tooMany (i1 + i2)

-- 3) 
instance (Num a, TooMany a) => TooMany (a, a) where 
  tooMany (n1, n2) = tooMany (n1 + n2)

-- Exercises: Pity the Bool
--1) 
data BigSmall =
    Big Bool
  | Small Bool
  deriving (Eq, Show)

-- Big Bool | Small Bool 
-- Big True | Big False | Small True | Small False 
-- Big True + Big False + Small True + Small False
-- 1 + 1 + 1 + 1 == 4

-- 2)

data NumberOrBool =
    Numba Int8
  | BoolyBool Bool
  deriving (Eq, Show)
myNumba = Numba (-128)

-- cardinality of NumberOrBool:
-- 256 + 2 = 258
-- creating Numba with numeric literal >127:
-- GHCi: Literal 128 is out of the Int8 range -128..127
-- literal < (-128)
-- same warning as above

-- Exercises: How does your garden grow?
--1) 
data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lilac
                deriving Show

type Gardener = String

data Garden =
   Garden Gardener FlowerType
   deriving Show

-- sum of products normal form of Garden
data Garden' = Gardenia' Gardener 
             | Daisy' Gardener
             | Rose' Gardener
             | Lilac' Gardener

-- Exercises: The Quad
data Quad =
    One
  | Two
  | Three
  | Four
  deriving (Eq, Show)

--1) 
eQuad :: Either Quad Quad
eQuad = undefined
-- 4 + 4 = 8

--2) 
prodQuad :: (Quad, Quad)
prodQuad = undefined
--4 * 4 = 16

--3) 
funcQuad :: Quad -> Quad
funcQuad = undefined
-- 4 ^ 4 = 256

--4) 
prodTBool :: (Bool, Bool, Bool)
prodTBool = undefined
-- 2 * 2 * 2 = 8

--5) 
gTwo :: Bool -> Bool -> Bool
gTwo = undefined
-- (2 ^ 2) ^ 2
-- OR: 2 ^ (2 * 2) = 16

--6) 
fTwo :: Bool -> Quad -> Quad
fTwo = undefined
-- (4 ^ 4) ^ 2 
-- OR: 4 ^ (4 * 2) = 65536

-- Chapter exercises: MCQ
-- 1) a
data Weekday =
        Monday
      | Tuesday
      | Wednesday
      | Thursday
      | Friday

-- 2) c
-- 3) b
-- 4) c





