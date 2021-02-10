{-# LANGUAGE OverloadedStrings #-}
-- allows string literals to be assigned T.Text type

-- ghc text.hs -XOverloadedStrings

-- other pragmas:
-- ViewPatterns—Allows for more-sophisticated pattern matching.
-- TemplateHaskell—Provides tools for Haskell metaprogramming.
-- DuplicateRecordFields—Solves the annoying problem from lesson 16, where using
-- the same field name for different types using record syntax causes a conflict.
-- NoImplicitPrelude—As mentioned, some Haskell programmers prefer to use a
-- custom Prelude. This language extension allows you to not use the default Prelude.

import qualified Data.Text as T
import Data.Semigroup
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO

-- T.pack :: String -> T.Text
-- T.unpack :: T.Text -> String

firstWord :: String
firstWord = "pessimism"

secondWord :: T.Text
secondWord = T.pack firstWord

thirdWord :: String
thirdWord = T.unpack secondWord

fourthWord :: T.Text
fourthWord = T.pack thirdWord

aWord :: T.Text
aWord = "Cheese"

sampleInput :: T.Text
sampleInput = "this\nis\ninput"

-- GHCi>T.lines sampleInput
-- ["this","is","input"]

tLines :: T.Text -> [T.Text]
tLines = T.splitOn "\n" 

breakText :: T.Text
breakText = "simple"

exampleText :: T.Text
exampleText = "This is simple to do"

-- GHCi> T.splitOn breakText exampleText
-- ["This is "," to do"]

-- GHCi> T.intercalate breakText (T.splitOn breakText exampleText)
-- "This is simple to do"

tUnlines :: [T.Text] -> T.Text
tUnlines = T.intercalate "\n"

-- GHCi> T.unlines (T.lines sampleInput)
-- "this\nis\ninput\n"

-- GHCi> T.unwords (T.words someText)
-- "Some text for you"

combinedTextMonoid :: T.Text
combinedTextMonoid = mconcat ["some"," ","text"]

combinedTextSemigroup :: T.Text
combinedTextSemigroup = "some" <> " " <> "text"

dharma :: T.Text
dharma = "धर्म"

bgText :: T.Text
bgText = "श्रेयान्स्वधर्मो विगुणः परधर्मात्स्वनुष्ठितात्। स्वधर्मे निधनं श्रेयः परधर्मो भयावहः"

highlight :: T.Text -> T.Text -> T.Text
highlight query fullText = T.intercalate highlighted pieces
  where pieces = T.splitOn query fullText
        highlighted = mconcat ["{",query,"}"]

helloPerson :: T.Text -> T.Text
helloPerson name = mconcat ["Hello"," ", name, "!"]

toInts :: TL.Text -> [Int]
toInts = map (read . TL.unpack) . TL.lines 
-- T.Text -> [T.Text] -> [String] -> [Int]


main :: IO ()
main = do
  TIO.putStrLn (highlight dharma bgText)
  putStrLn "Hello! What's your name?"
  name <- TIO.getLine
  let statement = helloPerson name
  TIO.putStrLn statement
  userInput <- TLIO.getContents
  let numbers = toInts userInput
  TLIO.putStrLn ((TL.pack . show . sum) numbers)
  -- [Int] -> Int -> String ->  T.Text




