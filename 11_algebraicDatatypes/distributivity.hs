
-- we use nullary data constructors here because 
-- you can’t use a type while only permitting
-- one of its inhabitants as a possible value.
-- using Fiction and Nonfiction will allow us to factor out book types

-- a * (b + c) -> (a * b) + (a * c)

data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show

-- sum type
data BookType = FictionBook Fiction
              | NonfictionBook Nonfiction
              deriving Show

type AuthorName = String
-- not in normal form; BookType unevaluated 
-- data Author = Author (AuthorName, BookType)


-- Normal form:
data Author' =
    Fiction' AuthorName
  | Nonfiction' AuthorName
  deriving (Eq, Show)