{-HLINT Ignore-}
{-# LANGUAGE InstanceSigs #-}

newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

-- State Functor
instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = 
    Moi (\s -> let (a, ns) = g s -- g :: \s -> (a, s)
                in (f a, ns))

-- State Applicative
instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi (\s -> (a, s))

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  -- f :: \s -> ((a -> b), s)
  -- g :: \s -> (a, s)
  (Moi f) <*> (Moi g) = 
    Moi (\s -> let (a, ns) = g s
                   (a2b, ns') = f ns
                in (a2b a, ns'))

-- State Monad
instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  -- f :: \s -> (a, s)
  -- g :: \a ->  Moi s b
  (Moi f) >>= g = 
    Moi (\s -> let (a, ns) = f s
                in runMoi (g a) ns) 
                -- runMoi :: Moi s b -> s -> (b, s)

-- Chapter Exercises 
-- 1)
get :: Moi s s
get = Moi $ \s -> (s, s)

-- 2)
put :: s -> Moi s ()
put arg = Moi $ \s -> ((), arg)

-- 3)
exec :: Moi s a -> s -> s
exec (Moi sa) s = snd $ sa s

-- 4)
eval :: Moi s a -> s -> a
eval (Moi sa) s = fst $ sa s

-- 5)
modify :: (s -> s) -> Moi s ()
modify f = Moi $ \s -> ((), f s)