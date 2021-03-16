{- HLINT IGNORE -}
import Control.Monad (join)
import Control.Applicative ((*>))

bind :: Monad m => (a -> m b) -> m a -> m b
bind f xs = join $ fmap f xs

sequencing :: IO ()
sequencing = do
  putStrLn "blah"
  putStrLn "another thing"

sequencing' :: IO ()
sequencing' =
  putStrLn "blah" >> -- sequencing
  putStrLn "another thing"

sequencing'' :: IO ()
sequencing'' =
  putStrLn "blah" *>
  putStrLn "another thing"

binding :: IO ()
binding = do
  name <- getLine
  putStrLn name

binding' :: IO ()
binding' =
  getLine >>= putStrLn -- no need to name variable; output passed to putStrLn directly

bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn ("y helo thar: " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
  putStrLn "name pls:" >>
  getLine >>=
  \name ->
  putStrLn ("y helo thar: " ++ name)

twoBinds :: IO ()
twoBinds = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn "age pls:"
  age <- getLine
  putStrLn ("y helo thar: "
    ++ name ++ " who is: "
    ++ age ++ " years old.")

twoBinds' :: IO ()
twoBinds' =
  putStrLn "name pls:" >>
  getLine >>=
    \name ->
      putStrLn "age pls:" >>
        getLine >>=
        \age ->
          putStrLn ("y helo thar: "
          ++ name ++ " who is: "
          ++ age ++ " years old.")

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs -- binds values within the list; sort of like list comprehension
  if even x -- this entire expression is (a -> m b)
    then [x*x, x*x] -- works on each value in the list; possibly expands list size
    else [x*x]

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else []

twiceWhenEven'' :: [Integer] -> [Integer]
twiceWhenEven'' xs = 
  xs >>= -- pulls out single Integer from list and passes it to fn
    \x -> if even x then [x*x, x*x] else [x*x]

data Cow = Cow {
  name :: String
  , age :: Int
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

-- if Cow's name is Bess,
-- it must be under 500
weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499
     then Nothing
     else Just c

mkSphericalCow :: String
  -> Int
  -> Int
  -> Maybe Cow
mkSphericalCow name' age' weight' =
  case noEmpty name' of
  Nothing -> Nothing
  Just nammy ->
    case noNegative age' of
    Nothing -> Nothing
    Just agey ->
      case noNegative weight' of
      Nothing -> Nothing
      Just weighty ->
        weightCheck
        (Cow nammy agey weighty)

-- w do syntax:
mkSphericalCow' :: String
  -> Int
  -> Int
  -> Maybe Cow
mkSphericalCow' name' age' weight' = do
  nammy <- noEmpty name'
  agey <- noNegative age'
  weighty <- noNegative weight'
  weightCheck (Cow nammy agey weighty)

-- w >>= :
mkSphericalCow'' :: String
  -> Int
  -> Int
  -> Maybe Cow
mkSphericalCow'' name' age' weight' =
  noEmpty name' >>= -- Monadic instance of Maybe: a Nothing return value will cause >>= to drop rest of computation
  \nammy ->
  noNegative age' >>=
  \agey ->
  noNegative weight' >>=
  \weighty ->
  weightCheck (Cow nammy agey weighty)

-- using applicatives in this instance (w/ mkSphericalCow) will only embed more structure
-- i.e. nested maybe
-- With the Maybe Applicative, each Maybe computation fails or succeeds independently of one another. 
-- i.e. lifting functions that are also Just or Nothing over Maybe values.
-- With the Maybe Monad, computations contributing to the final
-- result can choose to return Nothing based on previous computations.