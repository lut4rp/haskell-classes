module February1Parser where

import Data.Char
import Debug.Trace

newtype Parser i o = Parser { runParser :: i -> Maybe (o, i) }

instance Functor (Parser i) where
  -- fmap :: (o -> o') -> Parser i o -> Parser i o'
  fmap f p = Parser $ \input ->
    let Maybe (o, rest) = runParser p input
    in (fmap f mo, rest)

instance Applicative (Parser i) where
  pure x = Parser $ \input -> (Just x, input)

  -- (<*>) :: Parser i (o -> o') -> Parser i o -> Parser i o'
  pf <*> po = Parser $ \input -> case runParser pf input of
    (Nothing, _)   -> (Nothing, input)
    (Just f, rest) -> case runParser po rest of
      (Nothing, _)    -> (Nothing, rest)
      (Just o, rest') -> (Just (f o), rest')

type Digit = Int

digit :: Parser String Digit
digit = Parser $ \input -> traceShow input $ traceShowId $ case input of
  (c:cs) | isDigit c -> (Just $ digitToInt c, cs)
  cs                 -> (Nothing, cs)

wholeNumber :: Parser String Int
wholeNumber = pure (\d num -> d * 10^countDigits num + num)
              <*> digit
              <*> wholeNumber
  where
    countDigits = length . show

-- wholeNumber = Parser $ \input -> case input of
--   "" -> (Nothing, "")
--   _ -> case runParser digit input of
--               (Just d, rest) -> case runParser wholeNumber rest of
--                                   (Just num, rest') -> (Just (d * 10^countDigits num + num), rest')
--                                   (Nothing, rest') -> (Just d, rest')
--               (Nothing, rest) -> (Nothing, rest)
--

char :: Char -> Parser String Char
char x = Parser $ \input -> case input of
  (c:cs) | c == x -> (Just c, cs)
  _               -> (Nothing, input)

word :: String -> Parser String String
word "" = Parser $ \input -> (Just "", input)
word (x:xs) = Parser $ \input -> case runParser (char x) input of
  (Just c, rest) -> case runParser (word xs) rest of
                      (Just w, rest') -> (Just (c:w), rest')
                      (Nothing, _)    -> (Nothing, input)
  (Nothing, rest) -> (Nothing, rest)
