{-
foldr const 0 [1..3]
(1 `c` (2 `c` (3 `c` 0))) [purely illustrative; substitution does not actually occur here since foldr does not recurse unconditionally]

--> right associativity: head of the list gets evaluated, set
aside, and then the function moves to the right, evaluates the next
head, and so on
--> (1 `c` (..)) == const 1 (..), where (..) indicates rest of the the fold; (..) does not need to be evaluated 
--> ans: 1

foldr (flip const) 0 [1..3]
(1 `fc` (2 `fc` (3 `fc` 0)))
(1 `fc` (2 `fc` 0))
(1 `fc` 0) == flip const 1 0 == const 0 1
0

--> (1 `fc` (..)) == flip const 1 (..), or const (..) 1; since const evaluates first argument, (..) must be evaluated 
--> since foldr is right associative. substitute recursively, then evaluate innermost parentheses: (3 `fc` 0) == flip const 3 0 == const 0 3
--> ans: 0

foldl const 0 [1..3]
(((0 `c` 1) `c` 2) `c` 3)
((0 `c` 2) `c` 3)
(0 `c` 3) == const 0 3
0

--> (..) `c` 3 == const (..) 3; since foldl is left associative, substitute recursively, then evaluate starting from innermost parentheses: (0 `c` 1) == const 0 1 
--> ans: 0

foldl (flip const) 0 [1..3]
(((0 `fc` 1) `fc` 2) `fc` 3)
((1 `fc` 2) `fc` 3)
(2 `fc` 3) == flip const 2 3 == const 3 2 
3

--> process of evaluation:
--> (..) `fc` 3 == flip const (..) 3 == const 3 (..), where (..) indicates rest of the the fold
--> ans: 3

** For finite lists:
foldr f z xs =
foldl (flip f) z (reverse xs)

-}

