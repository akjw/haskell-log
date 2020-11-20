{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

-- simple example
-- example = 1

-- 1)
--   a) 54 :: Num a => a
--   b) (0, "doge") :: Num a => (a, [Char])
--   c) (0, "doge") :: (Integer, [Char])
--   d) False :: Bool
--   e) 5 :: Int
--   f) False :: Bool

-- 2) w :: Num a => a

-- 3) z :: Num a => a -> a

-- 4) f :: Fractional a => a

-- 5) f :: [Char]