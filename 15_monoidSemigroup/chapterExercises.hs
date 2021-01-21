import Data.Monoid
import Test.QuickCheck
import Control.Monad

data Optional a =
      Nada
      | Only a
      deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) x Nada = x
  (<>) Nada x = x 
  (<>) (Only a) (Only b) = Only $ a <> b 

instance Monoid a => Monoid (Optional a) where
  mempty = Nada 
  -- mappend = (<>)

-- Madness
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation
  -> Adverb
  -> Noun
  -> Adjective
  -> String
madlibbin' e adv noun adj =
  e <> "! he said " <>
  adv <> " as he jumped into his car " <>
  noun <> " and drove off with his " <>
  adj <> " wife."

madlibbinBetter' :: Exclamation
  -> Adverb
  -> Noun
  -> Adjective
  -> String
madlibbinBetter' e adv noun adj = 
  mconcat [e, "! he said ", adv, " as he jumped into his car ",
          noun, " and drove off with his ", adj, " wife."]

-- Validating associativity with QuickCheck
asc :: Eq a
  => (a -> a -> a)
  -> a -> a -> a
  -> Bool
asc (<>) a b c =
  a <> (b <> c) == (a <> b) <> c

-- stack ghci --package QuickCheck
monoidAssoc :: (Eq m, Monoid m)
  => m -> m -> m -> Bool
monoidAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

-- Testing left and right identity
monoidLeftIdentity :: (Eq m, Monoid m)
  => m
  -> Bool
monoidLeftIdentity a = (mempty <> a) == a
monoidRightIdentity :: (Eq m, Monoid m)
  => m
  -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- Testing QuickCheck’s patience
-- this not-actually-a-Monoid for Bool turns out to pass associativity
-- but fail on the right and left identity checks. 

data Bull =
  Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [ (1, return Fools)
    , (1, return Twoo) ]

instance Semigroup Bull where
  (<>) _ _ = Fools

instance Monoid Bull where
  mempty = Fools

type BullMappend =
  Bull -> Bull -> Bull -> Bool

-- main :: IO ()
-- main = do
--   let ma = monoidAssoc
--       mli = monoidLeftIdentity
--       mri = monoidRightIdentity
--   quickCheck (ma :: BullMappend)
--   quickCheck (mli :: Bull -> Bool)
--   quickCheck (mri :: Bull -> Bool)


-- Exercise: Maybe another Monoid

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Semigroup (First' a) where
  (<>) x (First' Nada) = x
  (<>) (First' Nada) x = x 
  (<>) _ (First' (Only a)) = First' (Only a)

instance Monoid (First' a) where
  mempty = First' Nada

firstMappend :: First' a
  -> First' a
  -> First' a
firstMappend = mappend

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = frequency [(1, return $ First' Nada)
                , (3, fmap (First' . Only) arbitrary)]

type FirstMappend =
  First' String
  -> First' String
  -> First' String
  -> Bool

type FstId =
  First' String -> Bool

-- main :: IO ()
-- main = do
--   quickCheck (monoidAssoc :: FirstMappend)
--   quickCheck (monoidLeftIdentity :: FstId)
--   quickCheck (monoidRightIdentity :: FstId)


-------------------------------------------------------------------------------------
-- SEMIGROUP EXERCISES
-------------------------------------------------------------------------------------

--1)

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m)
  => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool

main1 :: IO ()
main1 = do
  let sa = semigroupAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (sa :: TrivAssoc)
  quickCheck (mli :: Trivial -> Bool)
  quickCheck (mlr :: Trivial -> Bool)

--2)

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (<>) (Identity a) (Identity b) = Identity (a <> b)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

type IdAssocChar = Identity [Char] -> Identity [Char] -> Identity [Char] -> Bool
type IdAssocInt  = Identity [Int] -> Identity [Int] -> Identity [Int] -> Bool

--3)

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two a b) (Two x y) = Two (a <> x) (b <> y)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoIntStr = Two (Sum Int) String 
type TwoAssoc = TwoIntStr -> TwoIntStr -> TwoIntStr -> Bool

--4)
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (<>) (Three a b c) (Three x y z) = Three (a <> x) (b <> y) (c <> z)

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
  mempty = Three mempty mempty mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

type ThreeIIS = Three (Sum Int) (Sum Int) String
type ThreeAssoc = ThreeIIS -> ThreeIIS -> ThreeIIS -> Bool


--5)

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (<>) (Four a b c d) (Four w x y z) = Four (a <> w) (b <> x) (c <> y) (d <> z)

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (Four a b c d) where
  mempty = Four mempty mempty mempty mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

type FourIISS = Four (Sum Int) (Sum Int) String String
type FourAssoc = FourIISS -> FourIISS -> FourIISS -> Bool

--6)
newtype BoolConj =
  BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (<>) _ (BoolConj False) = BoolConj False
  (<>) (BoolConj False) _ = BoolConj False
  (<>) (BoolConj True) (BoolConj True) = BoolConj True

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

instance Arbitrary BoolConj where
  arbitrary = fmap (BoolConj) arbitrary

type BoolCAssoc = BoolConj-> BoolConj-> BoolConj-> Bool


--7)
newtype BoolDisj =
  BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (<>) _ (BoolDisj True) = BoolDisj True
  (<>) (BoolDisj True) _ = BoolDisj True
  (<>) (BoolDisj False) (BoolDisj False) = BoolDisj False

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

instance Arbitrary BoolDisj where
  arbitrary = fmap (BoolDisj) arbitrary

type BoolDAssoc = BoolDisj-> BoolDisj-> BoolDisj-> Bool

--8)
data Or a b =
  Fst a
  | Snd b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
  (<>) (Snd b) _ = Snd b
  (<>) _ (Snd b) = Snd b
  (<>) _ (Fst a) = Fst a
  -- can summarize last 2 patterns as:
  -- (<>) _ a = a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = frequency [(1, fmap Fst arbitrary)
                        , (1, fmap Snd arbitrary)]

type OrII = Or (Sum Int) (Sum Int)
type OrAssoc = OrII -> OrII -> OrII -> Bool

--9)
newtype Combine a b =
  Combine { unCombine :: (a -> b) } 

-- instance Show (Combine a b) where
--   show (Combine _) = "Combine"

-- since b is output only b needs a semigroup instance
instance (Semigroup b) => Semigroup (Combine a b) where 
  (<>) (Combine f) (Combine g) = Combine (f <> g)
  -- (<>) (Combine f) (Combine g) = Combine (\x -> (f x) <> (g x))

-- take b's implementation of mempty
instance Monoid b => Monoid (Combine a b) where
  mempty = Combine mempty 

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = fmap Combine arbitrary

prop_combAssoc :: (Eq b, Semigroup b)
               => Blind (Combine a b) -- negates QuickCheck's need for show instance
               -> Blind (Combine a b)
               -> Blind (Combine a b)
               -> a
               -> Bool
prop_combAssoc (Blind x) (Blind y) (Blind z) a =
    unCombine ((x <> y) <> z) a ==
    unCombine (x <> (y <> z)) a

prop_combAssocInt :: Blind (Combine Int (Sum Int))
                  -> Blind (Combine Int (Sum Int))
                  -> Blind (Combine Int (Sum Int))
                  -> Int
                  -> Bool
prop_combAssocInt = prop_combAssoc

f = Combine $ \n -> Sum (n + 1)

g = Combine $ \n -> Sum (n - 1)

fg0test :: Sum Int
fg0test = unCombine (f <> g) 0 

fg1test :: Sum Int
fg1test = unCombine (mappend f mempty) 0

ff1test :: Sum Int
ff1test = unCombine (f <> f) 1 

gf1test :: Sum Int
gf1test = unCombine (g <> f) 1

--10)
newtype Comp a =
  Comp { unComp :: (a -> a) }

instance Show (Comp a) where
  show (Comp _) = "Comp"

instance Semigroup (Comp a) where 
  (<>) (Comp f) (Comp g) = Comp (f . g)

instance Monoid (Comp a) where
  mempty = Comp id

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = fmap Comp arbitrary

prop_compAssoc :: (Eq a, Semigroup a)
               => Blind (Comp a) -- negates QuickCheck's need for show instance
               -> Blind (Comp a)
               -> Blind (Comp a)
               -> a
               -> Bool
prop_compAssoc (Blind x) (Blind y) (Blind z) a =
    unComp ((x <> y) <> z) a ==
    unComp (x <> (y <> z)) a

prop_compAssocInt :: Blind (Comp (Sum Int))
                  -> Blind (Comp (Sum Int))
                  -> Blind (Comp (Sum Int))
                  -> Sum Int
                  -> Bool
prop_compAssocInt = prop_compAssoc

--11)
data Validation a b =
  Failure' a | Success' b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (<>) (Success' x) _           = Success' x
  (<>) (Failure' x) (Failure' y) = Failure' (x <> y)
  (<>)  _          x           = x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = frequency [(1, fmap Failure' arbitrary)
                        , (1, fmap Success' arbitrary)]

type ValidStrInt = Validation String Int
type ValidAssoc = ValidStrInt -> ValidStrInt -> ValidStrInt -> Bool

main' :: IO ()
main' = do
  let 
      failure :: String -> Validation String Int
      failure = Failure'
      success :: Int -> Validation String Int
      success = Success'
  print $ success 1 <> failure "blah"  -- Success 1
  print $ failure "woot" <> failure "blah" -- Failure "wootblah"
  print $ success 1 <> success 2 -- Success 1
  print $ failure "woot" <> success 2 -- Success 2


-------------------------------------------------------------------------------------
-- MONAD EXERCISES (1-7 interleaved w/ above)
-------------------------------------------------------------------------------------

-- 8) 
newtype Mem s a =
  Mem {
    runMem :: s -> (a,s)
  }
-- Note: when constructing, written as such: Mem (\s -> ("hi", s + 1))

-- Works by piping the output of first fn into the second
instance Semigroup a => Semigroup (Mem s a) where
    (<>) (Mem f) (Mem g) = Mem comb 
      where comb s   =           -- s for the awaited argument
             let (fa, fs) = f s  -- apply s to first fn
                 (ga, gs) = g fs -- apply output of first fn to second fn
             in  (fa <> ga, gs)  -- mappend fst in the 2 output tuples 

-- ref. @johsi-k's solution:
-- instance Semigroup a => Semigroup (Mem s a) where
--   Mem x <> Mem y = Mem (\s -> let (a, s') = f s
--                                   (a', s'') = g s'
--                               in (a <> a', s''))

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem (\s -> (mempty, s))

f' = Mem $ \s -> ("hi", s + 1)

main2 = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print $ rmleft                      -- ("hi", 1)
  print $ rmright                     -- ("hi", 1)
  print $ (rmzero :: (String, Int))   -- ("", 0)
  print $ rmleft == runMem f' 0       -- True
  print $ rmright == runMem f' 0      -- True


main :: IO ()
main = do
  putStrLn "Tests for Trivial:"
  quickCheck (semigroupAssoc :: TrivAssoc)
  putStrLn "Tests for Id:"
  quickCheck (semigroupAssoc :: IdAssocChar)
  quickCheck (semigroupAssoc :: IdAssocInt)
  putStrLn "Tests for Two:"
  quickCheck (semigroupAssoc :: TwoAssoc)
  putStrLn "Tests for Three:"
  quickCheck (semigroupAssoc :: ThreeAssoc)
  putStrLn "Tests for Four:"
  quickCheck (semigroupAssoc :: FourAssoc)
  putStrLn "Tests for BoolConj:"
  quickCheck (semigroupAssoc :: BoolCAssoc)
  putStrLn "Tests for BoolDisj:"
  quickCheck (semigroupAssoc :: BoolDAssoc)
  putStrLn "Tests for Or:"
  quickCheck (semigroupAssoc :: OrAssoc)
  putStrLn "Tests for Combine:"
  quickCheck prop_combAssocInt
  putStrLn "Tests for Comp:"
  quickCheck prop_compAssocInt
  putStrLn "Tests for Validation:"
  quickCheck (semigroupAssoc :: ValidAssoc)

