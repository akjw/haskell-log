data BinaryTree a =
    Leaf 
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a
            => a
            -> BinaryTree a
            -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right) 
  | b == a = Node left a right
  | b < a  = Node (insert' b left) a right
  | b > a  = Node left a (insert' b right)
 

mapTree :: (a -> b)
         -> BinaryTree a
         -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf)
       1 
       (Node Leaf 4 Leaf)

mapExpected =
  Node (Node Leaf 4 Leaf)
       2 
       (Node Leaf 5 Leaf)

-- acceptance test for mapTree
mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup OK!"
  else error "test failed!"

-- Convert binary trees to lists
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a : preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ [a] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = inorder left ++ inorder right ++ [a]

testTree :: BinaryTree Integer
testTree =
  Node (Node Leaf 1 Leaf)
       2
       (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "Bad news bears."

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder

-- Write foldr for BinaryTree
-- any traversal order is fine
foldTree :: (a -> b -> b)
         -> b
         -> BinaryTree a
         -> b  
foldTree _ acc Leaf = acc
foldTree f acc (Node left a right) =
  f a (foldTree f (foldTree f acc left) right)


-- foldTree (+) 0 testTree  =
--  (+) 2 (foldTree (+) (foldTree (+) 0 (Node Leaf 1 Leaf)) (Node Leaf 3 Leaf))
--  (+) 2 (foldTree (+) ((+) 1 (foldTree (+) (foldTree (+) 0 Leaf) Leaf)) 
--                      (Node Leaf 3 Leaf))
--  (+) 2 (foldTree (+) ((+) 1 (foldTree (+) 0 Leaf)) 
--                      (Node Leaf 3 Leaf))
--  (+) 2 (foldTree (+) ((+) 1 0) 
--                      (Node Leaf 3 Leaf))
--  (+) 2 (foldTree (+) 1 (Node Leaf 3 Leaf))
--  (+) 2 ((+) 3 (foldTree (+) (foldTree (+) 1 Leaf) Leaf))
--  (+) 2 ((+) 3 (foldTree (+) 1 Leaf))
--  (+) 2 ((+) 3 1)
--  (+) 2 4
--  6




 