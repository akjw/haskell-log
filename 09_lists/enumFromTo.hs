eftBool :: Bool -> Bool -> [Bool]
eftBool False True = [False, True]
eftBool True False = []
eftBool b    _     = [b]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd LT EQ = [LT, EQ]
eftOrd LT GT = [LT, EQ, GT]
eftOrd EQ GT = [EQ, GT]
eftOrd EQ LT = []
eftOrd GT LT = []
eftOrd GT EQ = []
eftOrd ord _ = [ord]

-- Common mechanism for eft:
eftThis :: (Enum a, Ord a) => a -> a -> [a]
eftThis x y 
  | x > y = []
  | x == y = [x]
  | otherwise = x : eftThis (succ x) y

eftInt :: Int -> Int -> [Int]
eftInt x y = eftThis x y 

eftChar :: Char -> Char -> [Char]
eftChar x y = eftThis x y
