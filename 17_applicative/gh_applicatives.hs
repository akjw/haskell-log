{- HLINT Ignore -}

--------------------------------------------------------------------------
-- REVIEW
--------------------------------------------------------------------------

-- functors generalize mapping to larger family of data structures


-- Applicatives: 
-- generalize fmap to fns with more than 1 argument

-- fmap0 :: a -> f a
-- fmap1 :: (a -> b) -> f a -> f b
-- fmap2 :: (a -> b -> c) -> f a -> f b -> f c
-- fmap3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d

-- > fmap (+) (Just 1) (Just 2)
-- Just 2


--------------------------------------------------------------------------
-- But how to generalize fmap to fns with more than 1 argument?
--------------------------------------------------------------------------

-- Approach 1: Declare new class to handle each case
class Functor2 f where
  fmap2 :: (a -> b -> c) -> f a -> f b -> f c

-- but this is tedious & hard to scale

-- Approach 2: use 2 specific fns to generalize fmap however you want

-- converts value of type a into data structure 
pure :: a -> f a
-- generalized form of fn application
(<*>) :: f (a -> b) -> f a -> f b -- this operator brackets to the left
-- (((g x) y) z)

-- Applicative style: applying pure of fn g to 3 arguments x, y, z
-- pure g <*> x <*> y <*> z 

-- proof that generalized fmaps can be built with just pure & (<*>):
fmap0 :: a -> f a
fmap0 = pure

fmap1 :: (a -> b) -> f a -> f b
fmap1 g x = pure g <*> x

fmap2' :: (a -> b -> c) -> f a -> f b -> f c
fmap2' g x y = (pure g <*> x) <*> y -- brackets to the left

-- Applicative functors: functor that supports pure & <*>:
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

-- EX: Maybe Applicative
instance Applicative Maybe where
  -- pure :: a -> Maybe a 
  pure = Just
  -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b 
  Nothing  <*> mx = Nothing
  -- g :: a -> b, mx :: Maybe a 
  (Just g) <*> mx = fmap g mx

-- > pure (+1) <*> Just 1 
-- Just 2
-- > pure (+) <*> Just 1 <*> Just 2
-- Just 3
-- > pure (+) <*> Nothing <*> Just 2
-- Nothing

-- EX: Lists
instance Applicative [] where
  -- pure :: a -> [a]
  pure x = [x]
  -- (<*>) :: [(a -> b)] -> [a] -> [b]
  -- take all possible fns and apply them to all possible arguments in all possible ways
  gs <*> xs = [g x | g <- gs, x <- xs]

-- > pure (+1) <*> [1, 2, 3]
-- [2, 3, 4]
-- > pure (+!) <*> [1] <*> [2]
-- [3]
-- > pure (*) <*> [1, 2] <*> [3, 4]
-- [3, 4, 6, 8]

-- applicative sty;e for lists supports a form of non-deterministic programming where pure fns can be applied to multi-valued arguments to get multi-value results 