{- HLINT Ignore -}

--------------------------------------------------------------------------
-- INTRO
--------------------------------------------------------------------------
-- Programming with effects 

-- choosing to be pure vs impure:
-- pure = programs are mathematical fns (no interaction with outside world)
-- impure = programs can have side effects (IO, state, exceptions)
-- how to combine the benefits of the 2 approaches? --> monads

-- Abstracting programming patterns

inc :: [Int] -> [Int]
inc [] = []
inc (n:ns) = n+1 : inc ns

sqr :: [Int] -> [Int]
sqr [] = []
sqr (n:ns) = n^2 : sqr ns

-- inc & sqr have the same programming pattern (doing smth to every element in a list)

-- abstracted out = map

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

-- inc = map (+1) 
-- sqr = map (^2)

--------------------------------------------------------------------------
-- Generalizing further
--------------------------------------------------------------------------
-- mapping over structures other than a list

-- a parametrized type f is a member of the functor class
-- if it has the fn fmap 
-- we know f is paramterized bc we have f a & f b
class Functor f where 
  fmap :: (a -> b) -> f a -> f b 

-- List Functor
-- [] here is the parameterized type of list, not yet given an argument
instance Functor [] where
  -- fmap :: (a -> b) -> [a] -> [b]
  fmap = map

-- Maybe Functor
data Maybe a = Nothing | Just a 

instance Functor Maybe where
  -- fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap g Nothing = Nothing
  fmap g (Just x) = Just (g x)

-- > fmap (+1) Nothing
-- Nothing (fmap propagates failure for us)
-- > fmap (*2) (Just 3)
-- Just 6
-- > fmap not (Just False)
-- Just True

-- Binary Tree Functor
data Tree a = Leaf a | Node (Tree a) (Tree a)

t :: Tree Int
t = Node (Leaf 1) (Leaf 2)

instance Functor Tree where 
  -- fmap :: (a -> b) -> Tree a -> Tree b
  -- g :: a -> b, x :: a, l, r :: Tree a
  fmap g (Leaf x) = Leaf (g x)
  -- (fmap g l), (fmap g r) :: Tree b 
  fmap g (Node l r) = Node (fmap g l) (fmap g r)


-- > fmap length (Leaf "abc")
-- Leaf 3
-- > fmap even (Node (Leaf 1) (Leaf 2))
-- Node (Leaf False) (Leaf True)

--------------------------------------------------------------------------
-- Why use functors?
--------------------------------------------------------------------------
--1) Can use the same name, fmap, for fns that are essentially the same
--2) Can define generic fns that work for any functorial type 
    -- e.g. inc :: Functor f => f int -> f Int
    --      inc = fmap (+1)
-- inc [1, 2, 3] = [2, 3, 4]
-- inc (Just 1) = just 2
-- inc (Node (Leaf 1) (Leaf 2)) = Node (Leaf 2) (Leaf 3)


