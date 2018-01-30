module January30 where

-- Write applicatives for List' and Either'

data List a = EndList | Cons a (List a) deriving (Show)

listfuns :: List (Integer -> Integer)
listfuns = Cons (+3) (Cons (*3) EndList)

testlist :: List Integer
testlist = Cons 1 (Cons 2 (Cons 3 EndList))

instance Functor List where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
  fmap _ EndList = EndList

instance Applicative List where
  pure x = Cons x EndList
  Cons f fs <*> Cons x xs = Cons (f x) (fmap f xs)
  Cons _ _ <*> EndList = EndList
  _        <*> _       = EndList


-------------------------

data Eether a b = Failure a | Success b deriving (Show)

instance Functor (Eether a) where
  fmap f (Success x) = Success (f x)
  fmap _ (Failure x) = Failure x

instance Applicative (Eether a) where
  pure = Success
  Success f <*> Success x = Success (f x)
  Failure f <*> Success _ = Failure f
  _         <*> Failure x = Failure x
