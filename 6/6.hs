module Advent6 where

import Data.List

transform :: (String -> String -> Ordering) -> String -> String
transform orderBy fileContents =
    map head
    $ map (maximumBy orderBy) 
    $ map group
    $ map sort
    $ transpose
    $ lines
    $ fileContents


part1 = do
    contents <- readFile "day6input.txt"
    print $ transform (\g1 g2 -> compare (length g1) (length g2)) contents

part2 = do
    contents <- readFile "day6input.txt"
    print $ transform (\g1 g2 -> compare (length g2) (length g1)) contents

