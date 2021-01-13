module Lib where


import Control.Monad (forever) 
import Data.Maybe (isJust) 
import Data.List (intersperse) 
import System.Exit (exitSuccess)  
import System.Random (randomRIO) 

type WordList = [String]

data Puzzle =
  Puzzle String [Maybe Char] [Char]
  deriving (Eq)
  -- String: word we're trying to guess
  -- [Maybe Char]: Letters correctly guessed
  -- [Char]: Characters guessed by player so far
 
instance Show Puzzle where
   show (Puzzle _ discovered guessed) =
    (intersperse ' ' $
    fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle str = Puzzle str (map (const Nothing) str) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle str _ _) c = elem c str

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c = elem c guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word
                filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c : s)
  where zipper guessed wordChar guessChar =
         if wordChar == guessed
         then Just wordChar
         else guessChar
        newFilledInSoFar =
          zipWith (zipper c)
            word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess
       , alreadyGuessed puzzle guess) of
   (_, True) -> do
     putStrLn "You already guessed that\
               \ character, pick\
               \ something else!"
     return puzzle
   (True, _) -> do
     putStrLn "This character was in the\
              \ word, filling in the word\
              \ accordingly"
     return (fillInCharacter puzzle guess)
   (False, _) -> do
     putStrLn "This character wasn't in\
             \ the word, try again."
     return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess correctlyGuessed guessed) =
  if (countIncorrectGuesses (Puzzle wordToGuess correctlyGuessed guessed)) > 9 then
    do putStrLn "You lose!"
       putStrLn $
         "The word was: " ++ wordToGuess
       exitSuccess
  else return ()

countIncorrectGuesses :: Puzzle -> Int
countIncorrectGuesses (Puzzle word _ guessed) = numIncorrect
  where numIncorrect = length (filter (\x -> elem x word == False ) guessed)

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameWin puzzle
  gameOver puzzle
  putStrLn $
    "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must \
              \ be a single character"

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return (filter gameLength aw)
  where gameLength w =
           let l = length (w :: String)
           in l >= minWordLength
              && l < maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO (0, ((length wl)) - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord
