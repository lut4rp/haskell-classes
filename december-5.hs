module Dec5 where

data Shape = Circle Int | Square Int | Rect Int Int

data Point = Point { x :: Int, y :: Int }
data Point' = Point' Int Int

unPoint :: Point' -> Int
unPoint (Point' fir _sec) = fir

area  :: Shape -> Int
area' :: Shape -> Int
arEa  :: Shape -> Double

area (Circle radius) = 3 * fromIntegral (radius * radius)
area (Square side) = side * side
area (Rect width height) = width * height
-- area _ = error "i do not understand"

area' shaep = case shaep of
  Circle raid -> 3 * raid * raid
  Square sied -> sied * sied
  Rect width height -> width * height

arEa (Circle radius) = 3.14 * fromIntegral (radius * radius)
arEa (Square side) = fromIntegral (side * side)
arEa (Rect width height) = fromIntegral (width * height)
-- arEa _ = error "i do not understand"

data Person = Person { pName :: String, pAge :: Int, pLocation :: Point }

data Person' = Person' { name :: String, age :: Int }
-- let p = Person' 10
-- "age p" will give you 10.

type Coord = (Int, Int)
type Z = (Int, Int)
-- type String' = [Char] -- this is how a String is defined!

-- newtype useful and more efficient for single constructor types
newtype Age = Age Int
newtype Mass = Mass Int


----------------------------------------------------------------------------
-- Homework:
-- Define a data type that can hold a list of integers

data IntList = NewIntList Int IntList | Empty deriving (Show)
-- x = NewIntList 3 Nothing
-- x = NewIntList 3 (NewIntList 4 (NewIntList 9 Nothing))
