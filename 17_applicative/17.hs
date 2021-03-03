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
fmap f x = pure f <*> x

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
