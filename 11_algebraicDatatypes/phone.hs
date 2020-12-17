import Data.Char
import Data.List

data DaPhone =
  DaPhone [(Digit, Letters)]
  deriving (Eq, Show)

-- validButtons = "1234567890*#"
type Digit = Char
type Letters = [Char]
-- Valid presses: 1 and up
type Presses = Int
phone = DaPhone [ ('1', "1")
                , ('2',"abc2")
                , ('3',"def3")
                , ('4',"ghi4")
                , ('5',"jkl5")
                , ('6', "mno6")
                , ('7', "pqrs7")
                , ('8', "tuv8")
                , ('9', "wxyz9")
                , ('*', "^")
                , ('0', " 0")
                , ('#', ".,")]

convo :: [String]
convo = ["Wanna play 20 questions",
         "Ya",
         "U 1st haha",
         "Lol ok. Have u ever tasted alcohol lol",
         "Lol ya",
         "Wow ur cool haha. Ur turn",
         "Ok. Do u think I am pretty Lol",
         "Lol ya",
         "Haha thanks just making sure rofl ur turn"]


-- 2)
getKeyCount :: Char -> DaPhone -> [(Digit, Presses)]
getKeyCount c (DaPhone p) = 
                (map (\(digit, Just presses) -> (digit, presses + 1)) -- since elemIndex starts from 0, add 1 to get no. of presses
                . filter ((/=Nothing) . snd) -- filter to find correct btn (only one)
                . map (\(digit, ltrs) -> (digit, elemIndex c ltrs))) p -- map no. of presses for each button w/o verification; if character does not occur in btn letters, (digit, Nothing) will be returned


-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
reverseTaps ::  DaPhone -> Char -> [(Digit, Presses)]
reverseTaps phone c = case isUpper c of
                            True  -> ('*', 1) : getKeyCount (toLower c) phone
                            False -> getKeyCount c phone


cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone str = (concat . map (reverseTaps phone)) str


--3)
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps seq = sum [snd tups | tups <- seq]

--4)
mostPopularLetter :: String -> Char
mostPopularLetter = 
              snd . --  take only char from tuple 
              maximum . -- get max tuple
              filter (\x -> snd x  /= ' ') . -- don't count whitespace 
              map (\x -> (length x, head x)) . -- create tuples with char and frequency [('a', 1), ('b', 2)]
              group .   -- split into list of lists, with equal constituents e.g. ["a","bb","c","ddd","ee","a"]
              sort      -- order chars 

mostPopularCost :: String -> Int
mostPopularCost = fingerTaps . reverseTaps phone . mostPopularLetter 

--5)
coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

mostPop :: [String] -> String
mostPop = snd . maximum . map (\x -> (length x, head x)) . group .  sort    

coolestWord :: [String] -> String
coolestWord = mostPop . concatMap words
