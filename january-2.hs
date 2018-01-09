module January2 where

class Show' a where
  show' :: a -> String


--------------

data Color = Red | Green | Blue

instance Show' Color where
  show' Red = "R"
  show' Green = "G"
  show' Blue = "B"


--------------

data Maybe' a = Just' a | Nothing'

instance (Show' a) => Show' (Maybe' a) where
  show' Nothing' = "Nothing'"
  show' (Just' a) = "Just [" ++ show' a ++ "]"

instance Show' Bool where
  show' True = "T"
  show' False = "F"


---------------

data List a = Empty | Cons a (List a) -- deriving (Show)

instance (Show a) => Show (List a) where
  show l = "<" ++ show'' l ++ ">"
    where
      show'' Empty = ""
      show'' (Cons x Empty) = show x
      show'' (Cons x xs) = show x ++ ", " ++ show'' xs


----------------

class Eq' a where
  equal :: a -> a -> Bool
  equal x y = not (notEqual x y)

  notEqual :: a -> a -> Bool
  notEqual x y = not (equal x y)

  {-# MINIMAL equal | notEqual #-}

instance Eq' Color where
  equal Red Red = True
  equal Green Green = True
  equal Blue Blue = True
  equal _ _ = False

instance (Eq' a) => Eq' (List a) where
  equal Empty Empty = True
  equal (Cons x xs) (Cons y ys) = x `equal` y && xs `equal` ys
  equal _ _ = False


----------------

data Dir = North
         | East
         | South
         | West
         -- deriving (Show, Enum)

instance Show Dir where
  show North = "North"
  show East = "East"
  show South = "South"
  show West = "West"

instance Enum Dir where
  fromEnum North = 0
  fromEnum East = 1
  fromEnum South = 2
  fromEnum West = 3

  toEnum 0 = North
  toEnum 1 = East
  toEnum 2 = South
  toEnum 3 = West
  toEnum n = toEnum (n `mod` 4)
