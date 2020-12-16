-- data Goats = Goats Int deriving (Eq, Show)

-- tooManyGoats :: Int -> Bool
-- tooManyGoats n = n > 42

-- newtype Goats =
--   Goats Int deriving (Eq, Show)
newtype Cows =
  Cows Int deriving (Eq, Show)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42


class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats =
  Goats Int deriving (Eq, Show)

-- with newtype (with different instance of TooMany compared to the type Int it contains)
instance TooMany Goats where
  tooMany (Goats n) = n > 43

-- deriving instance of TooMany from type Int it contains
-- without pragma
-- instance TooMany Goats where
--   tooMany (Goats n) = tooMany n

-- with pragma
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- class TooMany a where
--   tooMany :: a -> Bool
-- instance TooMany Int where
--   tooMany n = n > 42
-- newtype Goats =
--   Goats Int deriving (Eq, Show, TooMany)