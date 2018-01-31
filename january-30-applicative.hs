module January30 where

import Data.Monoid ((<>))

data List a = EndList | Cons a (List a) deriving (Show)

listfuns :: List (Integer -> Integer)
listfuns = Cons (+3) (Cons (*3) EndList)

testlist :: List Integer
testlist = Cons 1 (Cons 2 (Cons 3 (Cons 4 EndList)))

instance Monoid (List a) where
  mempty = EndList
  mappend = pp
    where
      pp EndList xs = xs
      pp (Cons x xs) ys = Cons x (pp xs ys)

instance Functor List where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
  fmap _ EndList = EndList

instance Applicative List where
  pure x = Cons x EndList
  Cons f fs <*> list   = fmap f list <> (fs <*> list)
  _         <*> _      = EndList


--------------------------------------------------------------------------------

data Eether a b = Failure a | Success b deriving (Show)

instance Functor (Eether a) where
  fmap f (Success x) = Success (f x)
  fmap _ (Failure x) = Failure x

instance Applicative (Eether a) where
  pure = Success
  Success f <*> Success x = Success (f x)
  Failure f <*> Success _ = Failure f
  _         <*> Failure x = Failure x
