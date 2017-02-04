module Advent3 where

import Data.List

checkSides :: [Int] -> Bool
checkSides xs
  | length xs /= 3 = False
  | otherwise = (xs !! 0) + (xs !! 1) > (xs !! 2)

isValidTriangle :: [Int] -> Bool
isValidTriangle sides = and $ map checkSides $ permutations sides

parseInput :: String -> [[Int]]
parseInput = (map (map read)) . (map words) . lines

process :: [[Int]] -> Int
process input = length $ filter id $ map isValidTriangle input

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks len xs = (take len xs) : chunks len (drop len xs)

part1 = do
  contents <- readFile "day3input.txt"
  print $ process $ parseInput contents

part2 = do
  contents <- readFile "day3input.txt"
  print $ process $ chunks 3 $ concat $ transpose $ parseInput contents
