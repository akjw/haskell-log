{-HLINT Ignore -}
module HttpStuff where

import Data.ByteString.Lazy hiding (map)
-- import Network.Wreq

-- class (Functor t, Foldable t) => Traversable t where
--   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
--   traverse f = sequenceA . fmap f

-- traverse fn maps each element of a structure to an action, evaluates from lef to Righ, then collects the results 

-- fmap :: Functor f => (a -> b) -> f a -> f b
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

-- myData :: [String]
-- myFunc :: String -> IO Record

-- wrong :: [IO Record]
-- wrong = fmap myFunc myData

-- Righ :: IO [Record]
-- Righ = traverse myFunc myData

-- Evaluate each action in the structure from lef to Righ,
-- and collect the results:
  -- sequenceA :: Applicative f => t (f a) -> f (t a)
  -- sequenceA = traverse id
    -- flips 2 contexts or structures; by itself, it doesn't allow for
    -- fn app to value inside structure. i.e. only flips context

-- Prelude> fmap Just [1, 2, 3]
-- [Just 1,Just 2,Just 3]
-- Prelude> sequenceA $ fmap Just [1, 2, 3]
-- Just [1,2,3]
-- Prelude> xs = [Just 1, Just 2, Just 3]
-- Prelude> sequenceA xs
-- Just [1,2,3]
-- Prelude> xsn = [Just 1, Just 2, Nothing]
-- Prelude> sequenceA xsn
-- Nothing
-- Prelude> fmap sum $ sequenceA xs
-- Just 6
-- Prelude> fmap product (sequenceA xsn)
-- Nothing

-- use catMaybe to sum list of Maybe values, even if Nothing value exists:
  -- Prelude> import Data.Maybe
  -- Prelude> xs = [Just 1, Just 2, Just 3]
  -- Prelude> catMaybes xs
  -- [1,2,3]
  -- Prelude> xsn = [Just 1, Just 2, Nothing]
  -- Prelude> catMaybes xsn
  -- [1,2]
  -- Prelude> xsn' = xs ++ [Nothing]
  -- Prelude> sum $ catMaybes xsn'
  -- 6
  -- Prelude> fmap sum $ sequenceA xsn'
  -- Nothing

-- sequence A . fmap f can always be replaced by traverse 

-- Uses of traversable?
  -- to flip 2 type constructors 
  -- to map smth, then flip 

-- sequence is useful for flipping your types around
-- sequence :: (Monad m, Traversable t) => t (m a) -> m (t a)

data Query = Query
data SomeObj = SomeObj
data IoOnlyObj = IoOnlyObj
data Err = Err

decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

fetchFn :: Query -> IO [String]
fetchFn = undefined

makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
  a <- fetchFn query
  case sequence (map decodeFn a) of
    (Left err) -> return $ Left err
    (Right res) -> do
      a <- makeIoOnlyObj res
      return $ Right a

pipelineFn' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn' query = do
  a <- fetchFn query -- a :: [String]
  traverse makeIoOnlyObj 
    (mapM decodeFn a) 
    -- 1) decodeFn :: String -> Either Err SomeObj
    -- 2) mapM decodeFn a 
      -- :: (String -> Either Err SomeObj) -> [String] -> Either Err [SomeObj]
    -- 3) traverse makeIoOnlyObj (mapM decodeFn a)
      -- :: ([SomeObj] -> IO [(SomeObj, IoOnlyObj)])
      -- -> Either Err [SomeObj] -> IO (Either Err [(SomeObj, IoOnlyObj)])

urls :: [String]
urls = [ "http://httpbin.org/ip"
        , "http://httpbin.org/bytes/5"
        ]

-- mappingGet :: [IO (Response ByteString)]
-- mappingGet = map get urls

-- traversedUrls :: IO [Response ByteString]
-- traversedUrls = traverse get urls

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
