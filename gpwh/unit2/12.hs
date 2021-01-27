type FirstName = String
type LastName = String
type Age = Int
type Height = Int
type PatientName = (FirstName,LastName)
type MiddleName = String
data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName

firstName :: PatientName -> FirstName
firstName patient = fst patient
lastName :: PatientName -> LastName
lastName patient = snd patient

data Sex = Male | Female
data RhType = Pos | Neg
data ABOType = A | B | AB | O
data BloodType = BloodType ABOType RhType

data Patient = Patient { name :: Name
                        , sex :: Sex
                        , age :: Int
                        , height :: Int
                        , weight :: Int
                        , bloodType :: BloodType }

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"
showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"
showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False --otherwise

patientInfoV2 :: PatientName -> Age -> Height -> String
patientInfoV2 (fname,lname) age height = name ++ " " ++ ageHeight
  where name = lname ++ ", " ++ fname
        ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

jackieSmith :: Patient
jackieSmith = Patient {name = Name "Jackie" "Smith"
                      , age = 43
                      , sex = Female
                      , height = 62
                      , weight = 115
                      , bloodType = BloodType O Neg }

jackieSmithUpdated :: Patient
jackieSmithUpdated = jackieSmith { age = 44 }

compatibleDonor :: Patient -> Patient -> Bool
compatibleDonor x y = canDonateTo (bloodType x) (bloodType y)

patientSummary :: Patient -> IO ()
patientSummary p = putStr ("Patient Name: " ++ showName (name p) ++ "\n"
                   ++ "Sex: " ++ [sexInitial (sex p)] ++ "\n"
                   ++ "Age: " ++ show (age p) ++ "\n"
                   ++ "Height: " ++ show (height p) ++ "in." ++ "\n"
                   ++ "Weight: " ++ show (weight p) ++ "lbs" ++ "\n"
                   ++ "Blood Type: " ++ showBloodType (bloodType p))