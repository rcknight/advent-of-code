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

main = do
    contents <- readFile "input.txt"
    print contents
