fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x

-- Scans exercises
--1) fibs20 = take 20 $ fibs
--2) fibsLT100 = takeWhile (<100) fibs
--3) fact x = (scanl (*) 1 [1..]) !! x