--1)
data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn int) (TisAn int') = int == int'

--2)
data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two int1 int2) (Two int1' int2') =
    int1 == int1' && int2 == int2'

--3)
data StringOrInt = 
    TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt int) (TisAnInt int') = int == int'
  (==) (TisAString str) (TisAString str') = str == str'
  (==) _ _ = False

--4)
data Pair a =
  Pair a a

instance Eq a => Eq (Pair a)where
  (==) (Pair x y) (Pair x' y') = x == x' && y == y'

--5)
data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x y) (Tuple x' y') = x == x' && y == y'

--6)
data Which a = 
    ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne x) (ThisOne x') = x == x'
  (==) (ThatOne y) (ThatOne y') = y == y'
  (==) _ _ = False

--7)
data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x) (Hello x') = x == x'
  (==) (Goodbye y) (Goodbye y') = y == y'
  (==) _ _ = False