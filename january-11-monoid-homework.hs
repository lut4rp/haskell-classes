module January11Homework() where

-- Implement Monoid for these data types...
-- Max, Min, All, Any, First, Last.
--
-- newtype Max' a = Max' a
-- newtype Min' a = Min' a
-- newtype Any'= Any' Bool
-- newtype All'= All' Bool
-- newtype First' a = First' (Maybe a)
-- newtype Last' a = Last' (Maybe a)
-- data List a = EmptyList | Cons a (List a)
-- data Set a = EmptySet | Node (Set a) a (Set a)
--
-- Reuse your `insert` function from BST homework
--
--
-- Here's the behaviour of First' and Last':
--
-- Prelude Data.Monoid> First' (Just 1) <> First' (Just 2) <> First' (Just 3)
-- First' {getFirst = Just 1}
-- Prelude Data.Monoid> First' Nothing <> First' (Just 2) <> First' (Just 3)
-- First' {getFirst = Just 2}
-- Prelude Data.Monoid> First' Nothing <> First' (Just 2) <> First' Nothing
-- First' {getFirst = Just 2}
-- Prelude Data.Monoid> Last' (Just 1) <> Last' (Just 2) <> Last' (Just 3)
-- Last' {getLast = Just 3}
-- Prelude Data.Monoid> Last' (Just 1) <> Last' (Just 2) <> Last' Nothing
-- Last' {getLast = Just 2}

newtype Max' a = Max' a
newtype Min' a = Min' a

instance (Ord a, Bounded a) => Monoid (Max' a) where
    mempty = Max' minBound
    mappend (Max' a) (Max' b) = if a > b then Max' a else Max' b

instance (Ord a, Bounded a) => Monoid (Min' a) where
    mempty = Min' maxBound
    mappend (Min' a) (Min' b) = if a < b then Min' a else Min' b

newtype Any' = Any' Bool
instance Monoid Any' where
  mempty = Any' False
  mappend (Any' a) (Any' b) = Any' (a || b)

newtype All' = All' Bool
instance Monoid All' where
  mempty = All' True
  mappend (All' a) (All' b) = All' (a && b)

newtype First' a = First' (Maybe a)
instance Monoid (First' a) where
  mempty = First' Nothing
  mappend (First' (Just a)) _ = First' (Just a)
  mappend _ x = x

newtype Last' a = Last' (Maybe a)
instance Monoid (Last' a) where
  mempty = Last' Nothing
  mappend _ (Last' (Just a)) = Last' (Just a)
  mappend x _ = x


data List a = EmptyList | Cons a (List a) deriving (Show)
pp :: List a -> List a -> List a
pp EmptyList xs = xs
pp (Cons p ppp) qqq = Cons p (pp ppp qqq)

instance Monoid (List a) where
  mempty = EmptyList
  mappend = pp

-----------

data Set a = EmptySet | Node (Set a) a (Set a) deriving Show

binsert :: (Ord a) => Set a -> a -> Set a
binsert EmptySet el = Node EmptySet el EmptySet
binsert (Node left val right) el
  | el == val = Node left val right
  | el > val = Node left val (binsert right el)
  | otherwise = Node (binsert left el) val right

instance (Ord a) => Monoid (Set a) where
  mempty = EmptySet
  mappend EmptySet xset = xset
  mappend (Node alset aval arset) another = binsert another aval
