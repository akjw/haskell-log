{- HLINT IGNORE -}
import Control.Applicative

-- every type with applicative instance must also have functor instance
-- pure fn lifts something into applicative structure
-- basically embeds value of any type into a structure
-- Prelude> pure 1 :: [Int]
-- [1]
-- Prelude> pure 1 :: Maybe Int
-- Just 1
-- Prelude> pure 1 :: Either a Int
-- Right 1
-- Prelude> pure 1 :: ([a], Int)
-- ([],1)

-- Applicative law:
-- fmap f x = pure f <*> x

-- Applicative = monoid for structure + fn app for values

-- Just (*2) <*> Just 2
-- =
-- Just 4
-- Just (*2) <*> Nothing
-- =
-- Nothing

-- First value of 2-tuple's Applicative instance needs monoid to combine
-- second value does not; produced through fn app

-- Prelude> (Product 3, (+9))<*>(Product 2, 8)
-- (Product {getProduct = 6},17)

-- [(+1), (*2)] <*> [2, 4]
-- [ (+1) 2 , (+1) 4 , (*2) 2 , (*2) 4 ]

-- Prelude> (,) <$> [1, 2] <*> [3, 4]
-- fmap (,)
-- [(1, ), (2, )] <*> [3, 4]
-- [(1,3),(1,4),(2,3),(2,4)]

liftA2 f x y = f <$> x <*> y


f x =
  lookup x [ (3, "hello")
  , (4, "julie")
  , (5, "kbai")]
g y =
  lookup y [ (7, "sup?")
  , (8, "chris")
  , (9, "aloha")]
h z =
  lookup z [(2, 3), (5, 6), (7, 8)]
m x =
  lookup x [(4, 10), (8, 13), (1, 9001)]

-- Prelude> xs = [1, 2, 3]
-- Prelude> xs' = [9, 9, 9]
-- Prelude> const <$> xs <*> xs'
-- [1,1,1,2,2,2,3,3,3]
-- Prelude> mkId = Identity
-- Prelude> const <$> mkId xs <*> mkId xs'
-- Identity [1,2,3]

-- Identity gives structure so that const can be lifted over entire lists

-- newtype Constant a b =
-- Constant { getConstant :: a }
-- Prelude> pure 1 :: Constant String Int --Int is discarded
-- Constant {getConstant = ""}

validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if (length s) > maxLen
  then Nothing
  else Just s

newtype Name =
  Name String deriving (Eq, Show)

newtype Address =
  Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s
-- mkName "babe" = fmap Name $ Just "babe"
-- mkName "babe" = Just (Name "babe")

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a


data Person =
  Person Name Address
  deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a =
  Person <$> mkName n <*> mkAddress a
  -- Just (Person (Name "babe")) <*> Just (Address "farm") =
  --   Just (Person (Name "babe") (Address "farm"))

  -- fmap Person (Just (Name "babe")) =
  --   Just (Person (Name "babe"))
  -- -- Person is awaiting another argument
  -- :t Just (Person (Name "babe"))
  --     :: Maybe (Address -> Person)
  -- :t Just (Address "farm") :: Maybe Address
  -- -- We want to apply the partially
  -- -- applied (Person "babe") inside the
  -- -- Just to the "farm" inside the Just.
  -- Just (Person (Name "babe"))
  -- <*> Just (Address "farm")

-- Prelude> s = "old macdonald's"
-- Prelude> addy = mkAddress s
-- Prelude> b = mkName "Babe"
-- Prelude> person = fmap Person b
-- Prelude> Person <$> mkName "Babe" <*> addy
-- Just (Person (Name "Babe")
-- (Address "old macdonald's"))

data Cow = Cow {
    name :: String
  , age :: Int
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

cowFromString' :: String
  -> Int
  -> Int
  -> Maybe Cow
cowFromString' name' age' weight' =
  Cow <$> noEmpty name' -- cow1
  <*> noNegative age'   -- cow2
  <*> noNegative weight' -- cow3
  -- Broken down:
  -- cow1 :: Maybe (Int -> Int -> Cow)
  -- cow1 = Cow <$> noEmpty "Bess"

  -- cow2 :: Maybe (Int -> Cow)
  -- cow2 = cow1 <*> noNegative 1

  -- cow3 :: Maybe Cow
  -- cow3 = cow2 <*> noNegative 2

  -- with liftA3:
  -- liftA3 Cow (noEmpty name')
  -- (noNegative age')
  -- (noNegative weight')

  -- Broken down:
  -- cow1 :: Applicative f => f String -> f Int -> f Int -> f Cow
  -- cow1 = liftA3 Cow

  -- cow2 :: Maybe Int -> Maybe Int -> Maybe Cow
  -- cow2 = cow1 (noEmpty "blah")

  -- cow3 :: Maybe Int -> Maybe Cow
  -- cow3 = cow2 (noNegative 1)

  -- cow4 :: Maybe Cow
  -- cow4 = cow3 (noNegative 2)

  -- Applicative laws

--1) Identity 
-- pure id <*> v = v
-- pure id <*> [1..5]
-- pure id <*> Just "Hello Applicative"
-- pure id <*> Nothing
-- pure id <*> Left "Error'ish"
-- pure id <*> Right 8001
-- -- ((->) a) has an instance
-- pure id <*> (+1) $ 2

--2) Composition
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

-- pure (.)
-- <*> [(+1)]
-- <*> [(*2)]
-- <*> [1, 2, 3]
-- ==
-- [(+1)] <*> ([(*2)] <*> [1, 2, 3])

-- pure (.)
-- <*> Just (+1)
-- <*> Just (*2)
-- <*> Just 1
-- == 
-- Just (+1) <*> (Just (*2) <*> Just 1)

--3) Homomorphism
-- effect of applying fn embedded in structure to value that is embedded in structure == applying fn to value without affecting any outside structure:
-- pure f <*> pure x = pure (f x)

--4) Interchange
-- u <*> pure y = pure ($ y) <*> u
-- u represents a fn embedded in some structure
-- pure ($ 2) <*> Just (+ 2)

-- mPure :: a -> Maybe a
-- mPure = pure

-- embed :: Num a => Maybe ((a -> b) -> b)
-- embed = mPure ($ 2)

-- mApply :: Maybe ((a -> b) -> b)
--   -> Maybe (a -> b)
--   -> Maybe b
-- mApply = (<*>)

-- myResult = pure ($ 2) `mApply` Just (+2)
-- myResult == Just 4

-- (Just (+2) <*> pure 2)
-- == (pure ($ 2) <*> Just (+2))

-- [(+1), (*2)] <*> pure 1
-- pure ($ 1) <*> [(+1), (*2)]
-- Just (+3) <*> pure 1
-- pure ($ 1) <*> Just (+3)