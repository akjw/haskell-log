-- identical to (a, b, c, d)
data Silly a b c d =
  MkSilly a b c d deriving Show

-- :kind
-- Silly :: * -> * -> * -> * -> *
-- Silly Int :: * -> * -> * -> *
-- Silly Int String :: * -> * -> *
-- Silly Int String Bool :: * -> *
-- Silly Int String Bool String :: *

-- Identical to (a, b, c, d)
-- (,,,) :: * -> * -> * -> * -> *
-- Prelude> :kind (Int, String, Bool, String)
-- (Int, String, Bool, String) :: *

-- Lists are polymorphic
-- define the list type without using an infix constructor:
-- Same type, redefined
-- with different syntax
data List a = Nil | Cons a (List a)