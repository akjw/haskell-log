{-# LANGUAGE OverloadedStrings #-}
module Text.Fractions where
import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta


badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

main :: IO ()
main = do
  let parseFraction' =
        parseString parseFraction mempty
  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork
  print $ parseFraction' alsoBad
  print $ parseFraction' badFraction

-- result of 3rd test: Success *** Exception: Ratio has zero denominator
-- error does not result from the process of parsing; has Success data constructor. 
-- error  results from trying to construct a ratio with a denominator that is 0
-- the error terminates the program; 4th test never runs

-- handle zero denominator case so that it becomes a parse error:
virtuousFraction :: Parser Rational
virtuousFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero" -- calls out failure explicitly
    _ -> return (numerator % denominator)

-- will return Failure value w/ cause of failure + program continues
testVirtuous :: IO ()
testVirtuous = do
  let virtuousFraction' =
        parseString virtuousFraction mempty
  print $ virtuousFraction' badFraction
  print $ virtuousFraction' alsoBad
  print $ virtuousFraction' shouldWork
  print $ virtuousFraction' shouldAlsoWork

-- unit of success
successfulInteger :: Parser Integer
successfulInteger = integer >>= \i -> eof >> return i
-- successfulInteger = do
--   i <- integer 
--   eof
--   return i

testSuccess :: IO ()
testSuccess = do
  let successfulInteger' =
        parseString successfulInteger mempty
  print $ successfulInteger' "123"
  print $ successfulInteger' "123abc"

-- minimal complete def requires: try, <?>, notFollowedBy

-- Text.Parser.Combinators
-- class Alternative m => Parsing m where
--   try :: m a -> m a (takes parser that may consume input; reruns on failure; fails if no input is consumed)

-- allows matching on keywords 
-- notFollowedBy :: Show a => m a -> m ()
-- > noAlpha = notFollowedBy alphaNum
-- > keywordLet =
-- try $ string "let" <* noAlpha

-- from Text.Parser.Char

-- Parses any single character other
-- than the one provided. Returns
-- the character parsed.
-- notChar :: Char -> m Char

-- Parser succeeds for any character.
-- Returns the character parsed.
-- anyChar :: m Char

-- Parses a sequence of characters, returns
-- the string parsed.
-- string :: String -> m String

-- Parses a sequence of characters
-- represented by a Text value,
-- returns the parsed Text fragment.
-- text :: Text -> m Text


-- trytry
type IntegerOrFraction =
  Either Rational Integer

parseIOF :: Parser IntegerOrFraction
parseIOF =
    try (Left <$> virtuousFraction) 
  <|> try (Right <$> integer) 

testIOF :: IO ()
testIOF = do
  let parseIOF' =
        parseString parseIOF mempty
  print $ parseIOF' "123"
  print $ parseIOF' "007" -- will parse as 7
  print $ parseIOF' shouldWork
  print $ parseIOF' shouldAlsoWork