--1) does not typecheck; must derive show

data Person =  Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

--2) does not typecheck; must derive show & eq

data Mood = Blah 
          | Woot deriving (Eq, Show)
settleDown x = if x == Woot
                 then Blah
                 else x

--3) 
-- a) Blah | Woot
-- b) fails to compile; no instance for Num Mood
-- c) fails to compile; no instance for Ord Mood

--4) s1 will not type check; incorrect no. of arguments
type Subject = String
type Verb = String 
type Object = String 

data Sentence = 
  Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drip" "drool"
s2 = Sentence "Julie" "loves" "dogs"

-- Given a datatype declaration, what can we do? 

data Rocks = 
  Rocks String deriving (Eq, Show)
data Yeah = 
  Yeah Bool deriving (Eq, Show)
data Papu = 
  Papu Rocks Yeah
  deriving (Eq, Show)

--1) will not type check; Papu takes Rocks and Yeah as arguments, not a String and Bool
phew = Papu (Rocks "chases") (Yeah True)

--2) typechecks; Rocks and Yeah supplied as arguments
truth = Papu (Rocks "chomskydoz") (Yeah True)

--3) typechecks; Papu has instance of Eq
equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

--4)  will not typecheck; Papu does not have instance of Ord
-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p'

