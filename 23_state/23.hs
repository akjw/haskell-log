-- State: means of expressing variable state w/o mutation
-- ST type: use if in-place mutation is needed

-- Qualities of State type:
-- 1) Doesn't require IO
-- 2) Limited only to data w/n State container
-- 3) Maintains referential transparency
-- 4) Explicit in types of fns

newtype State s a =
  State { runState :: s -> (a, s) }

-- State :: (s -> (a, s)) -> State s a
-- runState :: State s a -> s -> (a, s)

-- note the similarities:
-- random :: (Random a) => StdGen -> (a, StdGen)
-- State { runState :: s -> (a, s) }

