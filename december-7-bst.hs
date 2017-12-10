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

binsert :: BST -> Int -> BST

binsert Empty num = Node Empty num Empty
binsert (Node left val right) num =
  if num > val
    then undefined
    else undefined
