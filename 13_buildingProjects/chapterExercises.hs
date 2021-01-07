-- Intermission: Check your understanding
--1) forever, when
--2) Data.Bits & Database.Blacktip.Types
--3) Types from the blacktip database..?
--4) Control.Concurrent.MVar, Filesystem.Path.CurrentOS, Control.Concurrent
--5) Filesystem
--6) Control.Monad

--This throws a type error: 
-- can’t match the expected type IO Bool with the actual type Bool
-- twoo' :: IO Bool
-- twoo' = do c <- getChar
--           c' <- getChar
--           c == c'

-- twoo :: IO Bool
-- twoo = do c <- getChar
--           c' <- getChar
--           return (c == c')

-- main :: IO ()
-- main = do c <- getChar
--           c' <- getChar
--           if c == c'
--             then putStrLn "True"
--             else return ()

-- Modifying Code 
--2 & 3)
import Control.Monad ( forever )
import Data.Char ( isAlpha, isSpace, toLower ) 
import System.Exit (exitSuccess) 

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  let linetext = formatString line1
   in case (linetext == reverse linetext) of
       True -> putStrLn "It's a palindrome!"
       False -> do 
         putStrLn "Nope!"
         exitSuccess

formatString :: String -> String
formatString = map toLower . filter (\x -> isAlpha x == True && isSpace x == False)

--4)
type Name = String
type Age = Integer
data Person = Person Name Age deriving Show
data PersonInvalid =
    NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =
      Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
      Left $ PersonInvalidUnknown $
        "Name was: " ++ show name ++
        " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Enter name: "
  name <- getLine
  putStrLn "Enter age: "
  age  <- getLine
  case (mkPerson name $ read age) of 
    (Right a) -> do 
      putStrLn $ "Yay! Successfully got a person: " ++ show a
    (Left b) -> do
      putStrLn $ "An error occured: " ++ show b