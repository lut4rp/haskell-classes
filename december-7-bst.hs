module BST where

--} Implement both these functions for a binary search tree.
-- bfind :: BST -> Int -> Bool
-- binsert :: BST -> Int -> BST

-- newtype BST = BST [Node] deriving (Show)

-- data Node = Node { left :: BST, value :: Int, right :: BST }
--           | Empty
--             deriving (Show)

data BST = Empty
         | Node { left :: BST, val :: Int, right :: BST }
         deriving (Show)

bfind    :: BST -> Int -> Bool
binsert  :: BST -> Int -> BST

bfind Empty _num = False

bfind mybst num =
  num == val mybst ||
  if num > val mybst
    then bfind (right mybst) num
    else bfind (left  mybst) num

binsert Empty num = Node Empty num Empty

binsert mybst num =
  if num > val mybst then
    if right mybst == Empty then
      Node (left mybst) (val mybst) (Node Empty num Empty) else
      binsert (right mybst) num
  else if left mybst == Empty then
    Node (Node Empty num Empty) (val mybst) (right mybst) else
    binsert (left mybst) num


-- Node {
--   left = Node {
--     left = Node {
--       left = Empty, val = 1, right = Empty
--     },
--     val = 5,
--     right = Node {
--       left = Empty,
--       val = 6,
--       right = Empty
--     }
--   },
--   val = 10,
--   right = Node {
--     left = Node {
--       left = Empty,
--       val = 17,
--       right = Empty
--     },
--     val = 19,
--     right = Empty
--   }
-- }
