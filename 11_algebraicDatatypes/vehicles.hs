data Price = Price Integer deriving (Eq, Show)

data Size = Small | Medium | Large deriving (Eq, Show)


data Manufacturer = 
              Mini
            | Mazda
            | Tata 
              deriving (Eq, Show)


data Airline = 
        PapuAir 
      | CatapultsR'Us
      | TakeYourChancesUnited
        deriving (Eq, Show)


data Vehicle = Car Manufacturer Price
             | Plane Airline Size
             deriving (Eq, Show)


-- Exercises: Vehicles
myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir Small

--1) myCar :: Vehicle
--2) 
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar
--3) 
getManu :: Vehicle -> Manufacturer
getManu (Car a _) = a
getManu _ = error "it's gotta be a car, dude"
--4) Exception thrown
--5) add size to plane