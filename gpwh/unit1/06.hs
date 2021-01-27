repeat x = cycle (x:[])

subseq x y ls = drop x $ take y ls

inFirstHalf x ls = elem x $ take ((length ls) `div` 2) ls