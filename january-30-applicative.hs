module January30 where

import Data.Monoid ((<>))
import Control.Applicative

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

instance Alternative List where
  empty = EndList

--------------------------------------------------------------------------------

data Eether a b = Failure a | Success b deriving (Show)

instance Functor (Eether a) where
  fmap f (Success x)   = Success (f x)
  fmap _ (Failure err) = Failure err

instance Applicative (Eether a) where
  pure = Success
  Success f <*> Success x = Success (f x)
  Success _ <*> Failure x = Failure x
  Failure e <*> _         = Failure e


--------------------------------------------------------------------------------

data Mebbe a = Only a | Nada

instance Functor Mebbe where
  fmap f (Only x)  = Only (f x)
  fmap _ Nada   = Nada

instance Applicative Mebbe where
  pure = Only
  Only f <*> Only x = Only (f x)
  _      <*> _      = Nada

instance Alternative Mebbe where
  empty = Nada
  Only x <|> _      = Only x
  _      <|> Only y = Only y
  _      <|> _      = Nada
