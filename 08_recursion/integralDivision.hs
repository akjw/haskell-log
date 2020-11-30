-- dividedBy :: Integer -> Integer -> Integer
-- dividedBy = div

-- type Numerator = Integer
-- type Denominator = Integer
-- type Quotient = Integer

-- dividedBy :: Numerator 
--           -> Denominator
--           -> Quotient
-- dividedBy = div

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n   d count
         | n < d = (count, n) -- base case
         | otherwise =
              go (n - d) d (count + 1)

-- dividedBy 10 2 =
--   go 10 2 0
--   go 8 2 1
--   go (8 - 2) 2 (1 + 1)
--   go 6 2 2
--   go (6 - 2) 2 (2 + 1)
--   go 4 2 3
--   go (4 - 2) 2 (3 + 1)
--   go 2 2 4
--   go (2 - 2) 2 (4 + 1)
--   go 0 2 5
  -- n < d
  -- return result
  -- (5, 0)
