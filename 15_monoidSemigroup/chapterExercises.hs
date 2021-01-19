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


-- Semigroup exercises 
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

--8)
newtype Combine a b =
  Combine { unCombine :: (a -> b) }



main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdAssocChar)
  quickCheck (semigroupAssoc :: IdAssocInt)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (semigroupAssoc :: BoolCAssoc)
  quickCheck (semigroupAssoc :: BoolDAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)