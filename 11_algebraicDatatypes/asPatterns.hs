import Data.Char

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf lx@(x:xs) (y:ys)
  | x == y = isSubseqOf xs ys
  | otherwise = isSubseqOf lx ys

-- Split a sentence into words, then tuple each one with its capitalized form:
capitalizeWords :: String -> [(String, String)]
capitalizeWords sentence = [(word, toUpper x:xs) | word@(x:xs) <- words sentence]

-- Language exercises
--1)
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (' ':xs) = ' ' : capitalizeWord xs
capitalizeWord (x:xs) = toUpper x:xs

--2) 
-- Method 1: do pattern match character by character; capitalize word that comes after '.'
capitalizeParagraph :: String -> String
capitalizeParagraph [] = []
capitalizeParagraph ('.':' ':xs) = ". " ++ capitalizeParagraph (capitalizeWord xs) -- capitalizeWord will ignore whitespace, so having trailing whitespace after '.' doesn't matter
capitalizeParagraph (x:xs) = x : (capitalizeParagraph xs) 

capsNoGo :: String -> String
capsNoGo = capitalizeParagraph . capitalizeWord -- capitalize first word of sentence before passing to capitalizeParagraph'

-- Method 1a: get rid of capsNoGo by using go pattern:
capitalizeParagraph' :: String -> String
capitalizeParagraph' para = go (capitalizeWord para)
  where 
      go [] = []
      go ('.':' ':xs) = ". " ++ go (capitalizeWord xs) 
      go (x:xs) = x : (go xs) 

-- Method 2: Break into list of sentences & capitalize first word in each
-- first split paragraph into list of sentences (strings)
-- credit @johsi-k
makeWords :: String -> [String]
makeWords = foldr rf []
  where
    rf c []     = [[c]]
    rf '.' ss   = "." : ss
    rf c (s:ss) = (c : s) : ss
-- then capitalize each sentence & join into string again 
capParagraph :: String -> String
capParagraph = unwords . map capitalizeWord . makeWords

-- Method 3 (credit @abevoelker): 
capPara :: String -> String
capPara x = unwords $ go (words x) True where -- boolean must start with True so that first word of sentence is capitalized
  go :: [String] -> Bool -> [String]
  go [] _ = []
  go (y:ys) capitalizeThis
    | capitalizeThis   = (capitalizeWord y) : (go ys capNextWord) -- if y ends with '.', capsNextWord returns True, which triggers capitalizeThis for next element y 
    | otherwise = y : (go ys capNextWord) 
    where
      capNextWord = (last y == '.')

-- capPara "that looks plenty. this is hungry work."
-- "That"                     [last "that" /= '.']
-- (capitalizeWord y) : (go ys capNextWord) 
-- since capNextWord returns false, go to "otherwise"
-- -- "That": "looks"
--                                                             [last "looks" /= '.']
-- "looks"  : (go ["plenty.", "this", "is", "hungry", "work."] capsNextWord)
-- since capNextWord returns false, go to "otherwise"
-- "That" : "looks" : "plenty."
--                                                   [last "plenty." == '.']
-- "plenty."  : (go ["this", "is", "hungry", "work."] capsNextWord)
-- capNextWord returns True; go to capitalizeThis:
-- "That": "looks" : "plenty." : "This":
-- "This"                      [last "this" /= '.']
-- (capitalizeWord y) : (go ys capNextWord) 
