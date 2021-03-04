module LsApp where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x y) = Cons (f x) (fmap f y) 

instance Semigroup (List a) where 
  Nil <> x = x
  x <> Nil = x
  (Cons x y) <> z = Cons x (y <> z)

instance Monoid (List a) where
  mempty = Nil

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ =  Nil
  _ <*> Nil = Nil
  (Cons x y) <*> z =  (x <$> z) <> (y <*> z)
                  -- append (x <$> z) (y <*> z) 
              -- z ~ (Cons a b)

-- On a granular level:
-- Cons (+1) (Cons (*2) Nil) <*> Cons 1 (Cons 2 Nil)
-- ((+1) <$> Cons 1 (Cons 2 Nil)) <> ((Cons (*2) Nil) <*> Cons 1 (Cons 2 Nil))
-- Cons (+1) 1 (fmap (+1) Cons 2 Nil) <> (fmap (*2) Cons 1 (Cons 2 Nil) <> Nil <*> Cons 1 (Cons 2 Nil))
-- Cons 2 (Cons 3 Nil)                <> (Cons (*2) 1 (fmap (*2) Cons 2 Nil) <> Nil)
-- Cons 2 (Cons 3 Nil)                <> (Cons 2 (Cons 4 Nil)) <> Nil
-- Cons 2 (Cons 3 Nil <> (Cons 2 (Cons 4 Nil))) <> Nil
-- Cons 2 (Cons 3 (Nil <> (Cons 2 (Cons 4 Nil)))) <> Nil
-- Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)) <> Nil
-- Cons 2 ((Cons 3 (Cons 2 (Cons 4 Nil)) <> Nil)
-- Cons 2 ((Cons 3 (Cons 2 (Cons 4 (Nil <> Nil)))))
-- Cons 2 ((Cons 3 (Cons 2 (Cons 4 Nil)))))

instance Arbitrary a => Arbitrary (List a) where
  arbitrary =
    frequency [
      (1, return Nil),
      (1, Cons <$> arbitrary <*> arbitrary)
    ]

instance Eq a => EqProp (List a) where
  (=-=) = eq

-- helper fns
append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys =
  Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

-- write this one in terms
-- of concat' and fmap
flatMap :: (a -> List b)
  -> List a
  -> List b
flatMap f as = concat' (f <$> as)

main :: IO ()
main = do
  let ls = Cons ("xs", "ys", 1 :: Int) Nil
  quickBatch (applicative ls)