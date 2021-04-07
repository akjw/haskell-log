{- HLINT Ignore -}

-- class (Functor t, Foldable t) => Traversable t where
--   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
--   traverse f = sequenceA . fmap f

-- traverse fn maps each element of a structure to an action, evaluates from lef to right, then collects the results 

-- fmap :: Functor f => (a -> b) -> f a -> f b
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

-- myData :: [String]
-- myFunc :: String -> IO Record

-- wrong :: [IO Record]
-- wrong = fmap myFunc myData

-- Right :: IO [Record]
-- Right = traverse myFunc myData

-- Evaluate each action in the structure from left to right,
-- then collect results:
  -- sequenceA :: Applicative f => t (f a) -> f (t a)
  -- sequenceA = traverse id
    -- flips 2 contexts or structures; doesn't transform value inside structure, unlike traverse. i.e. only flips context

-- fmap Just [1, 2, 3] = [Just 1,Just 2,Just 3]
-- sequenceA $ fmap Just [1, 2, 3] = Just [1,2,3]

-- xs = [Just 1, Just 2, Just 3]
-- sequenceA xs = Just [1,2,3]

-- xsn = [Just 1, Just 2, Nothing]
-- sequenceA xsn = Nothing

-- fmap sum $ sequenceA xs = Just 6
-- fmap product (sequenceA xsn) = Nothing

-- use catMaybe (from Data.Maybe) to sum list of Maybe values, ignore Nothing values:

  -- xs = [Just 1, Just 2, Just 3]
  -- catMaybes xs = [1,2,3]

  -- xsn = [Just 1, Just 2, Nothing]
  -- catMaybes xsn = [1,2]

  -- xsn' = xs ++ [Nothing]
  -- sum $ catMaybes xsn' = 6
  -- fmap sum $ sequenceA xsn' = Nothing

-- sequence A . fmap f can always be replaced by traverse 

-- Uses of traversable?
  -- to flip 2 type constructors 
  -- to map smth, then flip 

-- sequence is useful for flipping your types around
-- sequence :: (Monad m, Traversable t) => t (m a) -> m (t a)


-- Traversable Instances:

-- Either

data Either' a b =
  Lef a
  | Righ b deriving (Eq, Ord, Show)
  
instance Functor (Either' a) where
  fmap _ (Lef x) = Lef x
  fmap f (Righ y) = Righ (f y)

instance Applicative (Either' e) where
  pure = Righ
  Lef e <*> _ = Lef e
  Righ f <*> r = fmap f r

instance Foldable (Either' a) where
  foldMap _ (Lef _) = mempty
  foldMap f (Righ y) = f y
  foldr _ z (Lef _) = z
  foldr f z (Righ y) = f y z

instance Traversable (Either' a) where
  traverse _ (Lef x) = pure (Lef x)
  traverse f (Righ y) = Righ <$> f y

-- Tuple
-- instance Functor ((,) a) where
--   fmap f (x,y) = (x, f y)

-- instance Monoid a => Applicative ((,) a) where
--   pure x = (mempty, x)
--   (u, f) <*> (v, x) = (u `mappend` v, f x)

-- instance Foldable ((,) a) where
--   foldMap f (_, y) = f y
--   foldr f z (_, y) = f y z

-- instance Traversable ((,) a) where
--   traverse f (x, y) = (,) x <$> f y
