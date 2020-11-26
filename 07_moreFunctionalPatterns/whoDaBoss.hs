data Employee = Coder
              | Manager
              | Veep
              | CEO
              deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' = 
  putStrLn $ show e ++ 
             " is the boss of " ++
             show e'

codersRuleCEOsDrool :: Employee
                    -> Employee
                    -> Ordering

codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _ = GT
codersRuleCEOsDrool _ Coder = LT
codersRuleCEOsDrool e e' =
  compare e e'

employeeRank :: (Employee 
              -> Employee 
              -> Ordering )
              -> Employee
              -> Employee
              -> IO ()

employeeRank f e e' =
  case f e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither employee\
                    \ is the boss"
    LT -> (flip reportBoss) e e'


-- using new ordering fn:
-- cs = codersRuleCEOsDrool 
-- employeeRank cs Coder CEO 
-- : Coder is the boss of CEO

-- using compare fn:
-- employeeRank compare Coder CEO
-- : CEO is the boss of Coder

