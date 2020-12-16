-- PugType is a type constant & PugData is a constant value 
-- Both take no arguments
data PugType = PugData

-- HuskyType is a type constructor with fully polymorphic type variable 'a'
-- HuskyData is a constant value
-- Type argument 'a' does not appear in data constructor, so it is phantom/has no witness
data HuskyType a = HuskyData

-- DogueDeBordeaux is a type constructor, doge is type variable
-- doges on either side of '=' must align
data DogueDeBordeaux doge = DogueDeBordeaux doge

myPug = PugData :: PugType
myHusky :: HuskyType a
myHusky = HuskyData
myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData
myOtherOtherHusky :: HuskyType [[[[Int]]]]
myOtherOtherHusky = HuskyData
-- no witness to the contrary

myDoge :: DogueDeBordeaux Int
myDoge =  DogueDeBordeaux 10

-- This will not work, because 10 cannot be reconciled with the type
-- variable being bound to String
-- badDoge :: DogueDeBordeaux String
-- badDoge = DogueDeBordeaux 10

data Doggies a =
    Husky a
    | Mastiff a
    deriving (Eq, Show)


