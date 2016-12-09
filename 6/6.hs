module Advent6 where

import Data.List

part1 = do
    contents <- readFile "day6input.txt"
    let transformed = 
            map head
            $ map (maximumBy (\g1 g2 -> compare (length g1) (length g2)))
            $ map group
            $ map sort
            $ transpose
            $ lines
            $ contents

    print $ transformed
