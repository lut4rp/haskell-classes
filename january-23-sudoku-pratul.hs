module January23Sudoku where

tester :: String
tester = "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"

data Cell = Fixed (Int, Int) Int | OneOf (Int, Int) [Int]  deriving (Show)

type Row = [Cell]

type Grid = [Cell]

type Board = [Row]


-- printBoard :: Board -> String
-- printBoard b = 
