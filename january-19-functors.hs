module January19th where

data Maybe' a = Just' a | Nothing'

instance Functor Maybe' where
  fmap f (Just' x)  = Just' (f x)
  fmap _ Nothing'   = Nothing'

--------

newtype Iden a = Iden a deriving (Show)

instance Functor Iden where
  fmap f (Iden x) = Iden (f x)

--------

data Eether a b = Failure a | Success b deriving (Show)

instance Functor (Eether a) where
  fmap f (Success x) = Success (f x)
  fmap _ (Failure x) = Failure x


--------

data List a = End | Cons a (List a) deriving (Show)

instance Functor List where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
  fmap _ End = End

--------

data Pair a b = Pair a b deriving (Show)

instance Functor (Pair a) where
  fmap f (Pair rigid y) = Pair rigid (f y)


--------

data Set a = EndSet | Node (Set a) a (Set a) deriving (Show)

instance Functor Set where
  fmap f (Node aset a bset) = Node (fmap f aset) (f a) (fmap f bset)
  fmap _ EndSet = EndSet
