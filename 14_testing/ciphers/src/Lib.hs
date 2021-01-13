module Lib (caesar, unCaesar, vigenere, unVigenere) where
import Data.Char

encode :: Int -> Char -> Char
encode = shift (+)

decode :: Int -> Char -> Char
decode = shift (-)

shift:: (Int -> Int -> Int) -> Int -> Char -> Char
shift fn shiftNum x = chr $ (newPosition `mod` 26) + handleAlphaCase x
  where newPosition = fn (alphabetPos x) shiftNum

handleAlphaCase :: Char -> Int
handleAlphaCase c
  | isUpper c = ord 'A'
  | isLower c = ord 'a'

alphabetPos :: Char -> Int
alphabetPos c = ord c - handleAlphaCase c

caesar :: String -> Int -> String
caesar text shiftNum = map (encode shiftNum) text

unCaesar :: String -> Int -> String
unCaesar []     _      = []
unCaesar (x:xs) spaces = decode spaces x : unCaesar xs spaces

vigenere :: String -> String-> String
vigenere = vigenere' encode

unVigenere :: String -> String-> String
unVigenere = vigenere' decode

vigenere' :: (Int -> Char -> Char) -> String -> String -> String
vigenere' fn xs key = go xs (cycle key)
  where  go [] _              = ""
         go (' ': xs) key    = ' ': go xs key
         go (x : xs) (y : ys) = fn (alphabetPos y) x : go xs ys

