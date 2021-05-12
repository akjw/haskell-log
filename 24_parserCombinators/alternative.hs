{-# LANGUAGE QuasiQuotes #-}
module AltParsing where

import Control.Applicative
import Text.RawString.QQ
import Text.Trifecta

type NumberOrString =
  Either Integer String


eitherOr :: String
eitherOr = [r|
123
abc
456
def|]
-- the above allows us to write multiline strings w/o needing to manually escape newlines. Quasiquoter generates as a string:
-- "\n\
-- \123\n\
-- \abc\n\
-- \456\n\
-- \def\n"


a = "blah"
b = "123"
c = "123blah789"

parseNos :: Parser NumberOrString
parseNos =
    (Left <$> integer) -- must lift over Parser structure
  <|> (Right <$> some letter) -- <|> is or (disjunction); some is one / more 
-- someLetter = some letter :: Parser String
-- Right expects string; lifting is needed; allowed since Parser us a functor

-- make parsrr cope with terminal newlines:
parseNos' :: Parser NumberOrString
parseNos' = do
  skipMany (oneOf "\n")
  v <- (Left <$> integer)
        <|> (Right <$> some letter)
  skipMany (oneOf "\n")
  return v

main = do
  let p f i =
        parseString f mempty i
  print $ p parseNos eitherOr
  print $ p (some letter) a
  print $ p integer b
  print $ p parseNos a
  print $ p parseNos b
  print $ p (many parseNos) c -- many is zero or more
  print $ p (some parseNos) c

-- some, many & <|> utilize the Alternative type class:
-- class Applicative f => Alternative f where
--   -- | The identity of '<|>'
--   empty :: f a
--   -- | An associative binary operation
--   (<|>) :: f a -> f a -> f a
--   -- | One or more.
--   some :: f a -> f [a]
--   some v = some_v
--     where
--       many_v = some_v <|> pure []
--       some_v = (fmap (:) v) <*> many_v

--   -- | Zero or more.
--   many :: f a -> f [a]
--   many v = many_v
--     where
--       many_v = some_v <|> pure []
--       some_v = (fmap (:) v) <*> many_v

