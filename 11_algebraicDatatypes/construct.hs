data GuessWhat = ChickenButt deriving (Eq, Show)

data Id a = MkId a deriving (Eq, Show)

data Product a b = Product a b deriving (Eq, Show)

data Sum a b = First a
             | Second b
             deriving (Eq, Show)

data RecordProduct a b =
  RecordProduct { pfirst  :: a
                , psecond :: b }
                deriving (Eq, Show)

{- PRODUCT TYPES -}
newtype NumCow = NumCow Int deriving (Eq, Show)
newtype NumPig = NumPig Int deriving (Eq, Show)

-- the following two are the same:
data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)
-- see line 5:
type Farmhouse' = Product NumCow NumPig

-- nesting is possible since Product takes two
-- arguments, one of which can also be another Product of values
newtype NumSheep = NumSheep Int deriving (Eq, Show)
data BigFarmhouse = BigFarmhouse NumCow NumPig NumSheep deriving (Eq, Show)
-- use Product with 3 values:
type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)

{- SUM TYPES -}
type Name = String
type Age = Int
type LovesMud = Bool
type PoundsOfWool = Int

data CowInfo =
  CowInfo Name Age
  deriving (Eq, Show)

data PigInfo =
  PigInfo Name Age LovesMud
  deriving (Eq, Show)

data SheepInfo =
  SheepInfo Name Age PoundsOfWool
  deriving (Eq, Show)

data Animal =
    Cow CowInfo
  | Pig PigInfo
  | Sheep SheepInfo
  deriving (Eq, Show)

-- Alternatively:
type Animal' =
  Sum CowInfo (Sum PigInfo SheepInfo)

-- equivalent to () unit type:
trivialValue :: GuessWhat
trivialValue = ChickenButt

idInt :: Id Integer
idInt = MkId 10

type Awesome = Bool
type Name' = String
person :: Product Name' Awesome
person = Product "Simon" True

data Twitter = Twitter deriving (Eq, Show)
data AskFm = AskFm deriving (Eq, Show)

-- Sum --> Twitter OR AskFm
socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter

data SocialNetwork = Twitter'
                   | AskFm'
                   deriving (Eq, Show)

-- with type synonyms:
-- avoid using type synonyms with unstructured data like text or binary. 
-- Type synonyms are best used when you want something lighter weight than newtypes but also want your type signatures to be more explicit.
type Twitter'' = String
type AskFm'' = String

twitter :: Sum Twitter'' AskFm''
twitter = First "Twitter"

askfm :: Sum Twitter'' AskFm''
askfm = Second "AskFm"

{- RECORD -}
myRecord :: RecordProduct Integer Float
myRecord = RecordProduct 42 0.00001

myRecord' :: RecordProduct Integer Float
myRecord' =
  RecordProduct { pfirst = 42
                , psecond = 0.00001 }

data OperatingSystem =
    GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang =
    Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgLang }
  deriving (Eq, Show)

nineToFive :: Programmer
nineToFive = Programmer { os = Mac
                        , lang = Haskell }

feelingWizardly :: Programmer
feelingWizardly = Programmer { lang = Agda
                             , os = GnuPlusLinux }

-- Exercise: Programmers
allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgLang]
allLanguages =
  [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = 
   [Programmer {os = o, lang = l} | o <- allOperatingSystems, l <- allLanguages]

-- Accidental bottoms from records
-- the following will throw a warning [DO NOT DO THIS]:
-- partialRecord :: IntFloat
-- partialRecord = RecordProduct { pfirst = 420} 

-- Instead, use partial application of the data constructor:
data ThereYet =
  There Float Int Bool
  deriving (Eq, Show)

nope = There

notYet :: Int -> Bool -> ThereYet
notYet = nope 25.5 

notQuite :: Bool -> ThereYet
notQuite = notYet 10

yusssss :: ThereYet
yusssss = notQuite False

-- type progression:
-- There :: Float -> Int -> Bool -> ThereYet
-- notYet :: Int -> Bool -> ThereYet
-- notQuite :: Bool -> ThereYet
-- yusssss :: ThereYet
