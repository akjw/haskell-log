module ZiplistApp where

import Control.Applicative
-- import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype ZipList' a =
  ZipList' [a]
  deriving (Eq, Show)

-- instance Monoid a => Monoid (ZipList a) where
--   mempty = pure mempty
--   mappend = liftA2 mappend

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take 3000 l
          ys' = let (ZipList' l) = ys
                in take 3000 l  

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

repeat' :: a -> [a]
repeat' x = xs where xs = x:xs

zip' :: [a -> b] -> [a] -> [b]
zip' [] _ = [] 
zip' _ []  = [] 
zip' (f:fs) (a:as) = f a : zip' fs as

instance Applicative ZipList' where
  pure x = ZipList'(repeat' x) -- an infinite list of a single fixed value
                               -- -- ZipList (fix (a:)) = ZipList [a,a,a,a,...

  -- This will not work:
  -- pure x = ZipList [x] 
  -- fails for infinite lists, which is a requirement for ZipList'

  ZipList' _   <*> ZipList' [] = ZipList' [] 
  ZipList' []  <*> ZipList' _   = ZipList' [] 
  ZipList' fs <*> ZipList' xs = ZipList' (zip' fs xs) 
  -- ghc definition:
  -- liftA2 f (ZipList xs) (ZipList ys) = ZipList (zipWith f xs ys)

  -- without zip':
  -- (ZipList' y) <*> (ZipList' z) = ZipList' $ go y z
  --   where
  --     go [] _ = []
  --     go _ [] = []
  --     go (f:fs) (x:xs) = f x : go fs xs

  -- with let:
  -- ZipList' (f : fs) <*> ZipList' (x : xs) =
  --   let ZipList' fsxs = ZipList' fs <*> ZipList' xs
  --   in ZipList' (f x : fsxs)

  -- This will not work:
  -- ZipList' (f:fs) <*> ZipList' (x:xs) = ZipList' $ f x : (fs <*> xs)
  -- fs <*> xs will use the in-built list applicative instance, defined in ghc as:
  -- instance Applicative [] where
  --   pure x    = [x]
  --   fs <*> xs = [f x | f <- fs, x <- xs]
  -- with this applicative instance, zipTest will return [-1, 2, 3, 4, 5] instead of [-1, 2] as expected

  -- also of interest: https://stackoverflow.com/questions/49805219/prevent-inadvertently-using-different-type-class-instance

zipTest = fs <*> xs
  where fs = ZipList' [negate, id]
        xs = ZipList' [1..5]

instance (Arbitrary a) => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

main :: IO ()
main =
  let ls = ZipList' [("3", "2", 1 :: Int)]
  in quickBatch (applicative ls)