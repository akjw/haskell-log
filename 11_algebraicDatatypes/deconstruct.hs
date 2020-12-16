newtype Name = Name String deriving Show
newtype Acres = Acres Int deriving Show

-- FarmerType is a Sum
data FarmerType = DairyFarmer
                | WheatFarmer
                | SoybeanFarmer
                deriving Show

-- Farmer is a plain product of Name, Acres and FarmerType
data Farmer =
  Farmer Name Acres FarmerType
  deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer  _                       = False

-- with record syntax:
data FarmerRec =
  FarmerRec { name       :: Name
            , acres      :: Acres
            , farmerType :: FarmerType }
            deriving Show

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer =
  case farmerType farmer of
    DairyFarmer -> True
    _           -> False

-- Accidental bottoms from records
data Automobile = Null 
                | Car { make :: String
                      , model :: String
                      , year :: Integer }
                deriving (Eq, Show)

-- using record accessor on Null, eg. 'make Null' will fail

-- Instead, split out the product into an independent type with its
-- own type constructor instead of only as an inline data constructor product
data Car' = Car' { make' :: String
                 , model'  :: String
                 , year' :: Integer }
                 deriving (Eq, Show)

-- Null here is still not great; more appropriate to use "Maybe" 
data Automobile' = Null'    
                 | Automobile' Car'
                deriving (Eq, Show)

data Quantum =
    Yes
  | No
  | Both
  deriving (Eq, Show)

-- data Either a b = Left a | Right b (-- Defined in ‘Data.Either’)

-- sum types
-- 3 + 3
quantSum1 :: Either Quantum Quantum
quantSum1 = Right Yes

quantSum2 :: Either Quantum Quantum
quantSum2 = Right No

quantSum3 :: Either Quantum Quantum
quantSum3 = Right Both

quantSum4 :: Either Quantum Quantum
quantSum4 = Left Yes

quantSum5 :: Either Quantum Quantum
quantSum5 = Left No

quantSum6 :: Either Quantum Quantum
quantSum6 = Left Both


-- product types
-- 3 * 3
quantProd1 :: (Quantum, Quantum)
quantProd1 = (Yes, Yes)

quantProd2 :: (Quantum, Quantum)
quantProd2 = (Yes, No)

quantProd3 :: (Quantum, Quantum)
quantProd3 = (Yes, Both)

quantProd4 :: (Quantum, Quantum)
quantProd4 = (No, Yes)

quantProd5 :: (Quantum, Quantum)
quantProd5 = (No, No)

quantProd6 :: (Quantum, Quantum)
quantProd6 = (No, Both)

quantProd7 :: (Quantum, Quantum)
quantProd7 = (Both, Yes)

quantProd8 :: (Quantum, Quantum)
quantProd8 = (Both, No)

quantProd9 :: (Quantum, Quantum)
quantProd9 = (Both, Both)

-- function types
-- 3 ^ 3
quantFlip1 :: Quantum -> Quantum
quantFlip1 Yes  = Yes
quantFlip1 No   = Yes
quantFlip1 Both = Yes

quantFlip2 :: Quantum -> Quantum
quantFlip2 Yes  = Yes
quantFlip2 No   = Yes
quantFlip2 Both = No

quantFlip3 :: Quantum -> Quantum
quantFlip3 Yes  = Yes
quantFlip3 No   = Yes
quantFlip3 Both = Both

quantFlip4 :: Quantum -> Quantum
quantFlip4 Yes  = Yes
quantFlip4 No   = No
quantFlip4 Both = Yes

quantFlip5 :: Quantum -> Quantum
quantFlip5 Yes  = Yes
quantFlip5 No   = Both
quantFlip5 Both = Yes

quantFlip6 :: Quantum -> Quantum
quantFlip6 Yes  = No
quantFlip6 No   = Yes
quantFlip6 Both = Yes

quantFlip7 :: Quantum -> Quantum
quantFlip7 Yes  = Both
quantFlip7 No   = Yes
quantFlip7 Both = Yes

quantFlip8 :: Quantum -> Quantum
quantFlip8 Yes  = Both
quantFlip8 No   = Yes
quantFlip8 Both = No

quantFlip9 :: Quantum -> Quantum
quantFlip9 Yes  = Both
quantFlip9 No   = No
quantFlip9 Both = No

quantFlip10 :: Quantum -> Quantum
quantFlip10 Yes  = Both
quantFlip10 No   = No
quantFlip10 Both = Both

-- Exponentiation in what order?
-- 2^3 == 8 

convert1 :: Quantum -> Bool
convert1 Yes  = True
convert1 No   = True
convert1 Both = True

convert2 :: Quantum -> Bool
convert2 Yes  = True
convert2 No   = True
convert2 Both = False 

convert3 :: Quantum -> Bool
convert3 Yes  = True
convert3 No   = False 
convert3 Both = True

convert4 :: Quantum -> Bool
convert4 Yes  = False
convert4 No   = True 
convert4 Both = True 

convert5 :: Quantum -> Bool
convert5 Yes  = True
convert5 No   = False
convert5 Both = False

convert6 :: Quantum -> Bool
convert6 Yes  = False
convert6 No   = False
convert6 Both = True

convert7 :: Quantum -> Bool
convert7 Yes  = False
convert7 No   = True
convert7 Both = False

convert8 :: Quantum -> Bool
convert8 Yes  = False
convert8 No   = False 
convert8 Both = False 