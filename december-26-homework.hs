module December26 where

---- Homework ->>
--
-- Use your BST homework and...
-- Write a "delete" function which takes a number to be deleted from the BST
-- Write a "depth" function which gives the maximum number of levels in a BST

data BST = Empty
         | Node BST Int BST
         deriving (Show)

bfind :: BST -> Int -> Bool
bfind Empty _num = False
bfind (Node left val right) num =
    num == val ||
    if num > val
      then bfind right num
      else bfind left num


binsert :: BST -> Int -> BST
binsert Empty num = Node Empty num Empty
binsert (Node left val right) num = if num > val
    then Node left val (binsert right num)
    else Node (binsert left num) val right


bdepth :: BST -> Int
bdepth Empty = 0
bdepth (Node left _ right) = if bdepth left > bdepth right
  then 1 + bdepth left
  else 1 + bdepth right


--
-- Find the Hamming distance between two strings
-- Find it here: https://en.wikipedia.org/wiki/Hamming_distance
--

hamming :: String -> String -> Int
hamming s1@(x:xs) s2@(y:ys)
  | length s1 /= length s2 = error "cannot hamming for different string lengths"
  | x /= y                   = 1 + hamming xs ys
  | otherwise                = 0 + hamming xs ys

hamming _ _ = 0
