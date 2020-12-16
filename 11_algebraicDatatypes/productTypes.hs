data QuantumBool = QuantumTrue
                 | QuantumFalse
                 | QuantumBoth
                 deriving (Eq, Show)

data TwoQs =
  MkTwoQs QuantumBool QuantumBool
  deriving (Eq, Show)

-- alternatively, using type alias
-- type TwoQs = (QuantumBool, QuantumBool)

-- cardinality of 9 (3 * 3)

-- Record Syntax
-- Old way
-- data Person =
--   MkPerson String Int
--   deriving (Eq, Show)

-- jm = MkPerson "julie" 108
-- ca = MkPerson "chris" 16
-- namae :: Person -> String
-- namae (MkPerson s _) = s

-- with record syntax
data Person =
  Person { name :: String
         , age :: Int }
         deriving (Eq, Show)

jm = Person "julie" 111
ca = Person "chris" 77

