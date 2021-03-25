{- HLINT Ignore -}

---------------------------------------------------------------------------
-- REVIEW
---------------------------------------------------------------------------

-- functors generalize map from lists to other data structures
-- f is a paramterized type, like List or Tree or Maybe
class Functor f where 
  fmap :: (a -> b) 
          -> f a -- data structure containing as 
          -> f b 

-- Maybe is functorial, so:
-- > fmap (+1) (Just 1)
-- Just 2

-- applicatives generalize functors to functions with more than 1 arg
class Functor f => Applicative f where
  pure :: a -> f a -- embeds value a into data structure
  -- generalized form of fn app:
  (<*>) :: f (a -> b) -> f a -> f b

-- > pure (+) <*> Just 1 <*> Just 2
-- Just 3

-- applicatives capture the pattern of applying pure fns like (+) to effectful arguments, and applicatives manage effects

---------------------------------------------------------------------------
-- Monad e.g. Simple Evaluator
---------------------------------------------------------------------------

data Expr = Val Int | Div Expr Expr

-- basic fn; doesn't deal with zero division --> will crash
eval1 :: Expr -> Int
eval1 (Val n) = n
eval1 (Div x y) = eval1 x `div` eval1 y

-- Approach 1: use Maybe:
safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)

-- works, but tedious:
eval2 :: Expr -> Maybe Int
eval2 (Val n) = Just n
eval2 (Div x y) = case eval2 x of 
                       Nothing -> Nothing -- propagate failure
                       Just n  -> case eval2 y of
                                       Nothing -> Nothing -- propagate failure
                                       Just m  -> safediv n m

-- Approach 2: rewrite 1 w/ Applicative style?
eval3 :: Expr -> Maybe Int
eval3 (Val n) = pure n -- using Maybe Applicative; will embed into Just
eval3 (Div x y) = pure safediv <*> eval3 x <*> eval3 y
                -- type error:
                -- eval3 x :: Maybe Int ; eval3 y :: Maybe Int
                -- pure safediv needs to be Int -> Int -> Int (i.e. pure function)
                -- actual type is Int -> Int -> Maybe Int (safediv is NOT a safe fn)

-- so what is the common pattern in eval2? 
-- case __mx__ of
--   Nothing -> Nothing
--   Just x  -> __f__ x

-- turn the above into a definition: 
-- mx >>= f = case mx of 
--                 Nothing -> Nothing                               
--                 Just x -> f x

-- type of (>>=) aka bind:
-- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b

-- Approach 3: using bind:
eval4 :: Expr -> Maybe Int
eval4 (Val n) = Just n
-- if eval a is successful, bind successful Int result n to lambda fn     
eval4 (Div a b) = eval a >>= (\n ->   
                    eval b >>= (\m ->                   
                      safediv n m))  
-- advantage here is that failure management is not necessary, since bind takes care of it
-- (>>=) hides plumbing of failure management; so does safediv --> makes program appear imperative

-- * Note:
-- breakdown of (eval b >>= (\m -> safediv n m)) :
-- mx >>= f = case mx of 
--                 Nothing -> Nothing                               
--                 Just x -> f x


-- general pattern for use of binding operator:
-- m1 >>= \x1 -> 
-- m2 >>= \x2 ->
--     .
--     .
--     .
-- mn >>= \xn ->
-- f x1 x2 ... xn -- this only succeeds if every intermediate computation succeeds

-- general pattern for do, which gets expanded out to bind's pattern above: 
-- do x1 <- m1
--    x2 <- m2
--       .
--       .
--       .
--    xn <- mn
--    f x1 x2 ... xn
         
-- w/ do notation, which is syntactic sugar for Approach 3:
eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div x y) = do n <- eval x -- (n comes from result of applying eval x)
                    m <- eval y
                    safediv n m

-- final program resembles imperative program, since it appears very procedural 

