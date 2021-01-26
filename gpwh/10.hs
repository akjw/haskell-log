cup flOz = \message -> message flOz
getOz aCup = aCup (\flOz -> flOz)



drink aCup ozDrank = if ozDiff >= 0
                     then cup ozDiff
                     else cup 0
  where flOz = getOz aCup
        ozDiff = flOz - ozDrank

isEmpty aCup = getOz aCup == 0


coffeeCup = cup 12
afterManySips = foldl drink coffeeCup [1,1,1,1,1]


type Name = String

data Robot = Robot { name :: Name
                   , attack :: Integer
                   , hp:: Integer
} deriving (Show)

robot (name,attack,hp) = \message -> message (name,attack,hp)

setName :: Robot -> Name -> Robot
setName r newName = r { name = newName }
setAttack :: Robot -> Integer -> Robot
setAttack r newAttack = r { attack = newAttack }
setHP :: Robot -> Integer -> Robot
setHP r newHP = r { hp = newHP }

damage :: Robot -> Integer -> Robot
damage r attackDamage = r { hp = hp r - attackDamage }

fight :: Robot -> Robot -> Robot
fight attacker defender = damage defender attackDamage
  where attackDamage = if hp attacker > 0 then attack attacker else 0


-- printRobot aRobot = aRobot (\(n,a,h) -> n ++
--                                         " attack:" ++ (show a) ++
--                                         " hp:"++ (show h))

-- damage aRobot attackDamage = aRobot (\(n,a,h) ->
--                                       robot (n,a,h-attackDamage))

-- fight aRobot defender = damage defender attack
--   where attack = if getHP aRobot > 10
--                  then getAttack aRobot
--                  else 0

fastRobot = Robot "speedy" 15 40
slowRobot = Robot "slowpoke" 20 30

getAllHPS :: [Robot] -> [Integer]
getAllHPS xs = map hp xs

-- using nested lambdas
threeRoundFight :: Robot -> Robot -> Robot
threeRoundFight a b = if hp aRound3 > hp bRound3 then aRound3 else bRound3
 where
  (aRound3, bRound3) = 
    (\(botA, botB) -> (fight botB botA, fight botA botB))
    ((\(botA, botB) -> (fight botB botA, fight botA botB))
      ((\(botA, botB) -> (fight botB botA, fight botA botB)) (a, b)))

-- with let syntax 
threeRounds :: Robot -> Robot -> Robot
threeRounds a b = 
  let (aRound3, bRound3) =  (\(x, y) -> (fight y x, fight x y)) (a2, b2)
      (a2, b2)         =  (\(x, y) -> (fight y x, fight x y)) (a, b)
  in 
    if hp aRound3 > hp bRound3 then aRound3 else bRound3

killerRobot = Robot "Kill3r" 25 200
gentlerRobot = setAttack killerRobot 5
weakerRobot = setHP killerRobot 50

robots = [fastRobot, slowRobot, killerRobot]
fightWeakerRobot = fight weakerRobot
thisIsSparta = map fightWeakerRobot robots

getSurvivorsHealth = map hp thisIsSparta
