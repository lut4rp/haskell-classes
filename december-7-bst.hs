module BST where

--} Implement both these functions for a binary search tree.
-- bfind :: BST -> Int -> Bool
-- binsert :: BST -> Int -> BST

-- newtype BST = BST [Node] deriving (Show)

-- data Node = Node { left :: BST, value :: Int, right :: BST }
--           | Empty
--             deriving (Show)

data BST = Empty
         | Node BST Int BST 
         deriving (Show)

bfind    :: BST -> Int -> Bool
binsert  :: BST -> Int -> BST
bdelete  :: BST -> Int -> BST

bfind Empty _num = False
bfind (Node left val right) num =
  num == val ||
  if num > val
    then bfind right num
    else bfind left num

binsert Empty num = Node Empty num Empty
binsert (Node left val right) num = if num > val 
    then Node left val (binsert right num)
    else Node (binsert left num) val right

bdelete Empty _num = Empty
