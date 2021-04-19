{- HLINT Ignore -}
import Data.Char ( isDigit, digitToInt )

--------------------------------------------------------------------------
-- Generic fns ( to work with any monad; many found in Control.Monad)
--------------------------------------------------------------------------

-- vanilla version
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

-- generic monadic version
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f [] = return []
mapM' f (x:xs) = do y <- f x
                    ys <- mapM' f xs
                    return (y:ys)

conv :: Char -> Maybe Int
conv c | isDigit c = Just (digitToInt c)
       | otherwise = Nothing

-- > mapM' conv "1234"
-- Just [1, 2, 3, 4]

-- vanilla version
concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]

-- generic monadic version; flattens monadic structures
join' :: Monad m => m (m a) -> m a
join' mmx = do mx <- mmx -- generator
               x  <- mx  -- generator
               return x  -- package results

-- > join' [[1,2], [3,4], [5, 6]] 
-- [1, 2, 3, 4, 5, 6]
-- > join' (Just (Just 1))
-- Just 1

--------------------------------------------------------------------------
-- Monad Laws
--------------------------------------------------------------------------
-- ~ identity properties
return x >>= f = f x

-- :: m a -> (a -> m a) -> m a
mx >>= return  = m x

-- associativity property (do notation relies on this to work properly)
(mx >>= f) >>= g = mx >>= (f >>= g) -- this results in type error

(mx >>= f) >>= g = mx >>= (\x -> (f x >>= g)) -- works with lambda


--------------------------------------------------------------------------
-- Effectful Programming
--------------------------------------------------------------------------
-- TYPE               -- EFFECT
a -> Maybe b          -- Exceptions
a -> [b]              -- Non-determinism
a -> ST b             -- Internal state
a -> IO b             -- Input / Output

--------------------------------------------------------------------------
-- What's the point of monads?
--------------------------------------------------------------------------

--1) Supports pure programming with effects
--2) Use of monads is explicit in types (i.e. easy to infer effects from types)
--3) Can generalize functions to any effect

-- Exercise
data Expr a = Var a | Val Int | Add (Expr a) (Expr a)

-- represent x + 1
Add (Var 'x') (Val 1) :: Expr Char 

Add (Var "x") (Val 1) :: Expr String

instance Monad Expr where
  -- return :: a -> Expr a
  return x = Var x
  -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
  (Var v) >>= f = f v
  -- v :: a, Var v :: Expr a, f :: a -> Expr b
  (Val n) >>= f = Val n
  -- Val n :: Expr a, f :: a -> Expr b
  (Add x y) >>= f = Add (x >>= f) (y >>= f)
  -- Add x y :: Expr a, x, y :: Expr a, f :: a -> Expr b 


