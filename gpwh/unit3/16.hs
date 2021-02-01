data BreakfastSide = Toast | Biscuit | Homefries | Fruit deriving Show
data BreakfastMeat = Sausage | Bacon | Ham deriving Show
data BreakfastMain = Egg | Pancake | Waffle deriving Show

data BreakfastSpecial = Kids KidsBreakfast | Basic BasicBreakfast | Lumber TheLumberjack deriving Show

data KidsBreakfast = KidsBreakfast BreakfastMain BreakfastSide deriving Show
data BasicBreakfast = BasicBreakfast BreakfastMain BreakfastMeat BreakfastSide deriving Show
data TheLumberjack = TheLumberjack TwoMains TwoMeats ThreeSides deriving Show

data TwoMains = TM BreakfastMain BreakfastMain deriving Show
data TwoMeats = TME BreakfastMeat BreakfastMeat deriving Show
data ThreeSides = TS BreakfastSide BreakfastSide BreakfastSide deriving Show



type FirstName = String
type LastName = String
type MiddleName = String
data Name = Name FirstName LastName 
          | NameWithMiddle FirstName MiddleName LastName
          | TwoInitialsWithLast Char Char LastName
          | FirstNameWithTwoInits FirstName Char Char
          deriving Show

data StoreItem = BookItem Book
               | RecordItem VinylRecord
               | ToyItem CollectibleToy 
               | PamphletItem Pamphlet

data AuthorName = AuthorName String String
data Artist = Person Name | Band String deriving Show
data Creator = AuthorCreator Author | ArtistCreator Artist deriving Show
data Author = Author Name deriving Show
data AuthName = AuthName {
                          first :: String,
                          last :: String
                         }
data Book = Book {
                    author :: Creator
                  , isbn :: String
                  , bookTitle :: String
                  , bookYear :: Int
                  , bookPrice :: Double
                  }

data VinylRecord = VinylRecord {
                                  artist :: Creator
                                , recordTitle :: String
                                , recordYear :: Int
                                , recordPrice :: Double
                                }
data CollectibleToy = CollectibleToy {
                                        name :: String
                                      , descrption :: String
                                      , toyPrice :: Double
                                      }

data Pamphlet = Pamphlet {
                            title :: String,
                            description :: String,
                            orgContact :: String
                          }


hpLovecraft :: Creator
hpLovecraft = AuthorCreator
  (Author
  (TwoInitialsWithLast 'H' 'P' "Lovecraft"))

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (PamphletItem _) = 0.0 

madeBy :: StoreItem -> String
madeBy (BookItem book) = show (author book)
madeBy (RecordItem record) = show (artist record)
madeBy _ = "unknown"

data Shape = CircleShape Circle 
           | SquareShape Square 
           | RectangleShape Rectangle 
          

data Circle = Circle { radius :: Float }
data Square = Square { side :: Float }
data Rectangle = Rectangle { len :: Float, brth :: Float }

perimeter :: Shape -> Float
perimeter (CircleShape c) = 2 * pi * (radius c)
perimeter (SquareShape s) = 4 * (side s)
perimeter (RectangleShape r) = 2 * (len r) + 2 * (brth r)

area :: Shape -> Float
area (CircleShape c) = pi * (radius c)^2
area (SquareShape s) = (side s)^2
area (RectangleShape r) = (len r) * (brth r)
