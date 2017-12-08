module December7th where

-- Define a datatype Point
-- Define a datatype LineSegment
-- Define a function Slope to calculate slope of a LineSegment

data Point = Point Double Double deriving (Show)

data LineSegment = LineSegment Point Point deriving (Show)

slope :: LineSegment -> Double
slope (LineSegment (Point x1 y1) (Point x2 y2)) = (y2 - y1) / (x2 - x1)

----
data List = Null | NewList Int List deriving (Show)


lenth :: List -> Int
lsum  :: List -> Int

lenth list = case list of
  Null -> 0
  NewList _ rest -> 1 + lenth rest

lsum list = case list of
  Null -> 0
  NewList first rest -> first + lsum rest

--------------------

data Frup q = Nothing | NewFrup q (Frup q) deriving (Show)
-- here, Frup is a type constructor
-- Frup alone has no meaning, it only has meaning with an actual type.
-- so a "Frup Bool" or a "Frup String" is the data type that can be constructed.

--------------------
-- Define a binary search tree of Ints with "find" and "insert" functions.

newtype Node = Node Int
