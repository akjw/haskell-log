{- HLINT Ignore -}
module LearnParsers where
import Text.Trifecta
import Text.Parser.Combinators

stop :: Parser a
stop = unexpected "stop" -- unexpected is a way to throw an error

-- rudimentary char
-- demo only, this won't work as is.
-- char :: Char -> Parser Char
-- char c =
--   Parser $ \s ->
--     case s of
--         (x:xs) -> if c == x
--                   then [(c, xs)]
--                   else []
--         _ -> []

-- from Text.ParserCombinators.HuttonMeijer
-- polyparse-1.11
-- type Token = Char
-- newtype Parser a =
--   P ([Token] -> [(a, [Token])])

-- Same thing, differently formatted
type Parser' a = String -> [(a, String)]

one = char '1'

one' = one >> stop

-- read two characters, '1' and '2'
oneTwo = char '1' >> char '2'

-- read two characters,
-- '1' and '2', then die
oneTwo' = oneTwo >> stop

-- 1)
oneFail = one >> eof
oneTwoFail = oneTwo >> eof

-- () because (>>) throws away whatever is returned
failTest :: Parser () -> IO ()
failTest p =
  print $ parseString p mempty "123"

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123" -- p is a char parser

pNL s = putStrLn ('\n' : s)

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "one eof fail:"
  failTest oneFail
  -- oneFailTest oneFail
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
  pNL "oneTwo eof fail:"
  failTest oneTwoFail
  -- oneTwoFailTest oneTwoFail

-- 2)
-- from Text.Parser.Char:
-- class Parsing m => CharParsing m where
-- string :: String -> m String 
-- parses a sequence of characters given by s. Returns the parsed string
p123 :: String -> IO ()
p123 s = 
  print $ parseString (string s) mempty s

-- 3)
parsingString :: CharParsing m => String -> m String
parsingString = traverse char
-- char :: CharParsing m => Char -> m Char






