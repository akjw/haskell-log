-- 1)
i :: a -> a
i x = x

-- 2)
c :: a -> b -> a
c x _ = x

-- 3) no difference
c'' :: b -> a -> b
c'' x _ = x

--4)
c' :: a -> b -> b
c' _ y = y

--5)
r :: [a] -> [a]
r x = take 1 x

--6)
co :: (b->c) -> (a -> b) -> a -> c
co yToZ xToY x = 
  (yToZ (xToY x))

--7)
a :: (a -> c) -> a -> a
a _ x = x

--8)
a' :: (a -> b) -> a -> b
a' xToY x = (xToY x)