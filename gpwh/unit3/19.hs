import qualified Data.Map as Map
import Data.List ( intercalate )
import Data.Maybe (isNothing)


data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord, Enum)

organs :: [Organ]
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]

ids :: [Int]
ids = [2,7,13,14,21,24]

organPairs :: [(Int,Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

possibleDrawers :: [Int]
possibleDrawers = [1 .. 50]

getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map getContents ids
  where getContents = \id -> Map.lookup id catalog

emptyDrawers :: [Int] -> Map.Map Int Organ -> Int
emptyDrawers ids catalog= (length . filter isNothing) contents
  where contents =  getDrawerContents ids catalog

availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalog

countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ available = length (filter 
                                      (\x -> x == Just organ) 
                                      available)

isSomething :: Maybe Organ -> Bool
isSomething Nothing = False
isSomething (Just _) = True

-- with Data.Maybe
-- justTheOrgans = filter isJust availableOrgans

justTheOrgans :: [Maybe Organ]
justTheOrgans = filter isSomething availableOrgans

showOrgan :: Maybe Organ -> String
showOrgan (Just organ) = show organ
showOrgan Nothing = ""

organList :: [String]
organList = map showOrgan justTheOrgans

cleanList :: String
cleanList = intercalate ", " organList

numOrZero :: Maybe Int -> Int
numOrZero (Just a) = a
numOrZero Nothing = 0

data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
  show (Vat organ) = show organ ++ " in a vat"
  show (Cooler organ) = show organ ++ " in a cooler"
  show (Bag organ) = show organ ++ " in a bag"

data Location = Lab | Kitchen | Bathroom deriving Show

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ

placeInLocation :: Container -> (Location,Container)
placeInLocation (Vat a) = (Lab, Vat a)
placeInLocation (Cooler a) = (Lab, Cooler a)
placeInLocation (Bag a) = (Kitchen, Bag a)

process :: Organ -> (Location, Container)
process organ = placeInLocation (organToContainer organ)

report ::(Location,Container) -> String
report (location,container) = show container ++
                              " in the " ++
                              show location

report' :: Maybe (Location,Container) -> String
report' Nothing = "Error: not valid"
report' (Just (location,container)) = show container ++
                                      " in the " ++
                                      show location

processAndReport :: (Maybe Organ) -> String
processAndReport (Just organ) = report (process organ)
processAndReport Nothing = "error, id not found"

processRequest :: Int -> Map.Map Int Organ -> String
processRequest id catalog = processAndReport organ
  where organ = Map.lookup id catalog

maybeMap :: Eq a => (Maybe a -> Maybe a) -> [Maybe a] -> [Maybe a]
maybeMap _ []     = []
maybeMap f (x:xs) = [f x] ++ (maybeMap f xs)

addMaybe :: Num a => Maybe a -> Maybe a
addMaybe (Just x) = Just (x + 1)
addMaybe Nothing = Nothing


