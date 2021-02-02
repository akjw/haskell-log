data Color = Red |
          Yellow |
          Blue |
          Green |
          Purple |
          Orange |
          White  |
          Brown deriving (Show,Eq)

-- instance Semigroup Color where
--   (<>) Red Blue = Purple
--   (<>) Blue Red = Purple
--   (<>) Yellow Blue = Green
--   (<>) Blue Yellow = Green
--   (<>) Yellow Red = Orange
--   (<>) Red Yellow = Orange
--   (<>) a b = if a == b
--   then a
--   else Brown

instance Semigroup Color where
  (<>) Red Blue = Purple
  (<>) Blue Red = Purple
  (<>) Yellow Blue = Green
  (<>) Blue Yellow = Green
  (<>) Yellow Red = Orange
  (<>) Red Yellow = Orange
  (<>) a b | a == b = a
           | a == White = b
           | b == White = a
           | all (`elem` [Red,Blue,Purple]) [a,b] = Purple
           | all (`elem` [Blue,Yellow,Green]) [a,b] = Green
           | all (`elem` [Red,Yellow,Orange]) [a,b] = Orange
           | otherwise = Brown

instance Monoid Color where
  mempty = White
  mappend = (<>)


type Events = [String]
type Probs = [Double]
type Event = String
type Prob = Double

data Events' = Events' [String] deriving (Show,Eq)
data Probs' = Probs' [Double] deriving (Show,Eq)

instance Semigroup Events' where
  Events' x <> Events' y = Events' (x <> y)

instance Monoid Events' where
  mempty = Events' mempty

instance Semigroup Probs' where
  Probs' x <> Probs' y = Probs'(x <> y)

instance Monoid Probs' where
  mempty = Probs' mempty


data PTable = PTable Events Probs
data PTable' = PTable' Events' Probs'

createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProbs
  where totalProbs = sum probs
        normalizedProbs = map (\x -> x/totalProbs) probs

createPTable' :: Events' -> Probs' -> PTable'
createPTable' events probs = (PTable' events (Probs' normalizedProbs))
  where totalProbs = sumUp probs
        normalizedProbs = map (\x -> x/totalProbs) (getDub probs)

sumUp :: Probs' -> Double
sumUp (Probs' x) = sum x

getDub :: Probs' -> [Double]
getDub (Probs' x) = x

showPair :: Event -> Prob -> String
showPair event prob = mconcat [event,"|", show prob,"\n"]

instance Show PTable where
  show (PTable events probs) = mconcat pairs
    where pairs = zipWith showPair events probs

instance Show PTable' where
  show (PTable' (Events' e) (Probs' p)) = mconcat pairs
    where pairs = zipWith showPair e p

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
  where nToAdd = length l2
        repeatedL1 = map (take nToAdd . repeat) l1
        newL1 = mconcat repeatedL1
        cycledL2 = cycle l2

combineEvents :: Events -> Events -> Events
combineEvents e1 e2 = cartCombine combiner e1 e2
  where combiner = (\x y -> mconcat [x,"-",y])

combineEvents' :: Events' -> Events' -> Events'
combineEvents' = mappend

combineProbs :: Probs -> Probs -> Probs
combineProbs p1 p2 = cartCombine (*) p1 p2

combineProbs' :: Probs' -> Probs' -> Probs'
combineProbs' = mappend

instance Semigroup PTable where
  (<>) ptable1 (PTable [] []) = ptable1
  (<>) (PTable [] []) ptable2 = ptable2
  (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
    where newEvents = combineEvents e1 e2
          newProbs = combineProbs p1 p2

instance Monoid PTable where
  mempty = PTable [] []
  mappend = (<>)

instance Semigroup PTable' where
  (<>) ptable1 (PTable' (Events' []) (Probs' [])) = ptable1
  (<>) (PTable' (Events' []) (Probs' [])) ptable2 = ptable2
  (<>) (PTable' e1 p1) (PTable' e2 p2) = createPTable' newEvents newProbs
    where newEvents = combineEvents' e1 e2
          newProbs = combineProbs' p1 p2

instance Monoid PTable' where
  mempty = PTable' (Events' []) (Probs' [])
  mappend = (<>)

coin :: PTable
coin = createPTable ["heads","tails"] [0.5,0.5]

coin' :: PTable'
coin' = createPTable' (Events' ["heads","tails"]) (Probs' [0.5,0.5])

spinner :: PTable
spinner = createPTable ["red","blue","green"] [0.1,0.2,0.7]

spinner' :: PTable'
spinner' = createPTable' (Events' ["red","blue","green"]) (Probs' [0.1,0.2,0.7])