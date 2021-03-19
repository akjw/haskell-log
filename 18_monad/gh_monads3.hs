{- HLINT Ignore -}

--------------------------------------------------------------------------
-- REVIEW
--------------------------------------------------------------------------

-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b
--   return :: a -> m a
--   return = pure 

-- State Monad
-- type State = ...

-- newtype ST a = S (State -> a, State)

-- app (S st) s = st s

-- pure fn:
-- Char -> Int 

-- impure (internal manipulation of State is abstracted & hidden):
-- Char -> ST Int
-- Char -> State -> (Int, State)

instance Monad ST where
  -- return :: a -> ST a
  return x = S (\s -> (x, s)) 
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S (\s -> let (x, s') = app st s
                      in app (f x) s')

--------------------------------------------------------------------------
-- EX: Relabelling trees
--------------------------------------------------------------------------

data Tree a = Leaf a | Node (Tree a) (Tree a)

t :: Tree Char
t = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

-- relabelling function goes down to each leaf and replaces existing label with fresh, unique numbers
-- non-monadic style:
    -- input Int = fresh label        -- output Int = next fresh label
rlabel :: Tree a -> Int -> (Tree Int, Int)
-- replace existing x with fresh label n; +1 to give next fresh label & ensure uniqueness of each label
rlabel (Leaf x) n = (Leaf n, n + 1) 
rlabel (Node l r) n = (Node l' r', n'')
                      where
                        (l', n') = rlabel l n   -- relabel left subtree
                        (r', n'') = rlabel r n' -- relabel right subtree
-- this is pretty tedious because n needs to be threaded throughout the fn

-- to rewrite in monadic style, it is crucial to note that Int -> (Tree Int, Int) is a state transformer: takes in input state (Int), gives back result value (Tree Int) & output state (Int, which is next fresh label)

-- fresh gives back in monadic style fresh labels; state transformer whose result is an Int
fresh :: ST Int 
fresh = S (\n -> (n, n + 1))

-- monadic style; no need for threading integer through fn:
mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf x) = do 
                    n <- fresh 
                    return (Leaf n)
mlabel (Node l r) = do
                      l' <- mlabel l
                      r' <- mlabel r
                      return (Node l' r')
                      
-- if you don't want any state involved:   
-- top-level labelling fn
label :: Tree a -> Tree Int
label t = fst (app (mlabel t) 0)