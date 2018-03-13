module Main where

-- You have a bunch of numbers in a file, one per line. The file is too big
-- to be sorted in memory. You have to sort it on disk using merge sort.
--
-- Run this using...
-- ghc --make march-13-mergesort-disk.hs -o mergesort

import Data.List

main :: IO ()
main = putStrLn "Hello World"

readNumbers :: FilePath -> IO [Int]
readNumbers path = do
  numbers <- readFile path
  return . map read . lines $ numbers

chunky :: Int -> [Int] -> [[Int]]
chunky _ [] = []
chunky c xs = take c xs : chunky c (drop c xs)

writeChunks :: [[Int]] -> FilePath -> IO ()
writeChunks chunkedList path = do
  let sortedstring = unlines . map show . head $ chunkedList
  writeFile path sortedstring

splitFile :: FilePath -> FilePath -> IO ()
splitFile inputPath outputPath = do
  numbers <- readFile inputPath
  let chunked = sort . map read . take 10000 . lines $ numbers :: [Int]
      sortedstring = unlines . map show $ chunked
  writeFile outputPath sortedstring
