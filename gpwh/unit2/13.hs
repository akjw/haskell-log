class Describable a where
  describe :: a -> String

-- Word only takes positive Int values --> maxBound is double that of  Int


-- Because there’s no true successor to a Bounded type, succ throws an error. The inc function
-- just rotates you back to the beginning

cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n = if n == maxBound
              then minBound
              else succ n