module January19th where

data Maybe' a = Just' a | Nothing'

instance Functor Maybe' where
  fmap f (Just' a)  = Just' (f a)
  fmap _ Nothing'   = Nothing'

--------

newtype Iden a = Iden a deriving (Show)

instance Functor Iden where
  fmap f (Iden a) = Iden (f a)

--------

data List a = End | Cons a (List a) deriving (Show)


--------

data Pair a b = Pair a b


--------

data Set a = EndSet | Node (Set a) a (Set a)


--------

data Eether a b = Failure a | Success b deriving (Show)

instance Functor (Eether a) where
  fmap f (Success a) = Success (f a)
  fmap _ (Failure b) = Failure b
