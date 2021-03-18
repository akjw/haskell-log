{- HLINT Ignore -}

--------------------------------------------------------------------------
-- REVIEW
--------------------------------------------------------------------------

-- bind operator captures a form of sequencing for computations that may fail
-- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
-- Nothing  >>= f = Nothing
-- (Just x) >>= f = f x

--------------------------------------------------------------------------
-- WHAT IS A MONAD?
--------------------------------------------------------------------------

-- Monads are a class of applicatives that support bind operator 

class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  return :: a -> m a 
  -- return = pure (default definition; Monads must also be applicatives)

-- EX 1: Maybe Monad
instance Monad Maybe where
  -- (>>=) :: Maybe a -> (a -> Maybe b) Maybe b
  Nothing >>= f = Nothing -- propagate failure
  Just x  >>= f = f x
-- definition above is what makes do notation work 

-- EX 2: Lists Monad
instance Monad [] where
  -- (>>=) :: [a] -> (a -> [b]) -> [b]
  [] >>= f = []
  xs >>= f =  concat $ map f xs -- [[b]] -> [b]
          -- = [y | x <xs, y <- f x]

-- > pairs [1, 2] [3, 4]
-- [(1,3), (1,4), (2,3), (2,4)]

-- w/ do notation:
pairs :: [a] -> [b] -> [(a, b)]
pairs xs ys = do x <- xs -- choose 1 val from xs in all possible ways
                 y <- ys -- choose 1 val from ys in all possible ways
                 return (x,y) -- embed into list
-- do notation has advantage over list comp since it works for any monad, not just lists

-- w/ list comprehension:
pairs' xs ys = [(x, y) | x <- xs, y <- ys]

-- EX 3: State Monad

-- to write programs that manipulate some kind of state

type State = ...

-- state transformer;
-- type ST a = State -> (a, State) -- a is result value of type a
-- take in input state, get result val of type a & possibly modified state

-- Char -> ST Int
-- expanded:
-- Char -> State -> (Int, State)

-- change to data declaration so that instance declarations can work
-- data ST a = S (State -> (a, State))
newtype ST a = S (State -> (a, State)) -- reduce runtime overhead; S is basically a dummy constructor

-- this gets rid of the dummy constructor S
app :: ST a -> State -> (a, State)
app (S st) s = st s 

instance Functor ST where 
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap f st = S (\s -> 
                let (x, s') = app st s
                --  (x, s') :: (a, state)
                in ((f x) s'))

instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S (\s -> (x,s))
  -- (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = S (\s ->
                  let (f,s’) = app stf s
                      (x,s’’) = app stx s’ 
                  in (f x, s’’))

instance Monad ST where 
  -- return :: a -> ST a
  return x = S (\s -> (x, s))
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S (\s -> 
                let (x, s') = app st s
                --  (x, s') :: (a, state)
                in app ((f x) s'))
                -- result: (x', s'')