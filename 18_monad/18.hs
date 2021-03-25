{- HLINT IGNORE -}
import Control.Monad (join, (>=>))
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
  -- with noEmpty "Bess", "Bess" is passed as a string instead of Just "Bess", since >>= binds over monadic value a, not m a
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

-- Either

-- years ago
type Founded = Int
-- number of programmers
type Coders = Int

data SoftwareShop =
  Shop {
    founded :: Founded
  , programmers :: Coders
  } deriving (Eq, Show)

data FoundedError =
    NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded Coders
  deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
  | n < 0 = Left $ NegativeYears n
  | n > 500 = Left $ TooManyYears n
  | otherwise = Right n

validateCoders :: Int -> Either FoundedError Coders
validateCoders n
  | n < 0 = Left $ NegativeCoders n
  | n > 5000 = Left $ TooManyCoders n
  | otherwise = Right n

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded <- validateFounded years 
  -- since values can be dependent in Monad, Either always short-circuits on first point of failure
  programmers <- validateCoders coders
  if programmers > div founded 10
    then Left $ TooManyCodersForYears founded programmers
    else Right $ Shop founded programmers

-- Keeping in mind
-- (<*>) :: Applicative f
--   => f (a -> b) -> f a -> f b
-- ap :: Monad m
--   => m (a -> b) -> m a -> m b

-- deriving Applicative <*> from the stronger instance:
ap :: (Monad m) => m (a -> b) -> m a -> m b
ap m m' = do
  x <- m
  x' <- m'
  return (x x')

-- Short Exercise: Either Monad
data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x) = First x
  fmap f (Second y) = Second (f y)

instance Applicative (Sum a) where
  pure = Second
  (First x)  <*>  _         = First x
  _          <*> (First x)  = First x
  (Second f) <*> (Second x) = Second (f x)

instance Monad (Sum a) where
  return = pure
  (First x)  >>= _    = First x
  (Second y) >>= f    = f y

-- Monad laws

-- right identity
-- m >>= return = m

-- left identity
-- return x >>= f = f x

-- associativity
-- (m >>= f) >>= g = m >>= (\x -> f x >>= g)

sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

readM :: Read a => String -> IO a
readM = return . read

-- (>=>)    :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
-- flip (.) ::             (a -> b) -> (b -> c) -> a -> c
-- (.)    :: (b -> c) -> (a -> b) -> a -> c
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = getAge "Hello! How old are you? "

-- 1.
j :: Monad m => m (m a) -> m a
j x = join x
-- j x = x >>= id

-- 2. 
l1 :: Monad m => (a -> b) -> m a -> m b
l1 f x = fmap f x

-- 3. 
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f x y = f <$> x <*> y
-- l2 = liftM2

-- 4. 
a :: Monad m => m a -> m (a -> b) -> m b
a x f = f <*> x

-- 5. You’ll need recursion for this one:
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = do 
  y <- f x
  ys <- meh xs f
  return (y:ys)

-- 6. Hint: reuse meh:
flipType :: (Monad m) => [m a] -> m [a]
flipType ms = meh ms id