module December28 where

-- lets make our own Maybe
data Mebbe a = Nothing | Meb a

-- can also do this cool thing
data Result a = Failure String | Success a

-- an empty tuple has only 1 value... itself.
-- repl: :t ()
sq [] = []
sq (x : xs) = x * x : sq xs

-- lets implement our own version of map
mapp f [] = []
mapp f (x : xs) = f x : mapp f xs

-- lets implement our own version of filter
filt _ [] = []
filt f (x : xs) = if f x then x : filt f xs else filt f xs

-- lets implement our own version of fold (elsewhere called reduce)
foldd :: (s -> a -> s) -> s -> [a] -> s
foldd f s [] = s
foldd f s (x : xs) = foldd f (f s x) xs

-- here's how you write anon functions.
-- filter (\x -> x `mod` 2 == 1) [1..100]
