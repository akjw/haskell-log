module Arith4 where

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

--5) point free version
roundTrip' :: (Show a, Read b) => a -> b
roundTrip' = read . show

main = do
  -- tell read to dispatch Int instance
  print ((roundTrip' 4) :: Int)
  print (id 4)