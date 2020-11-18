data Mood = Blah | Woot deriving Show
changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood    _ = Blah

-- '_' denotes catchall for all other potential inputs 
-- with potential inputs being defined by data and type constructors
-- in this case there are only 2, since Mood is binary 
-- i.e. last line is functionally equivalent to changeMood Woot = Blah