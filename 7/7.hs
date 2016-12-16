module Advent7 where

import Data.List
import Data.List.Split

type IPv7Address = String

test1 = "abba[mnop]qrst"
test2 = "abcd[bddb]xyyx"

isPalindrome :: String -> Bool
isPalindrome x = reverse x == x

isNotRepeating :: String -> Bool
isNotRepeating x = not (x == (take (length x) (repeat (head x))))

isNonRepeatingPalindrome :: String -> Bool
isNonRepeatingPalindrome x = isPalindrome x && isNotRepeating x

supportsTLS :: IPv7Address -> Bool
supportsTLS ip = hasPalindromeOutside && (not hasPalindromeInside)
    where
        (outside, inside) = deconstructIp ip
        hasPalindromeOutside = or $ map isNonRepeatingPalindrome $ concat $ map (windows 4) outside
        hasPalindromeInside = or $ map isNonRepeatingPalindrome $ concat $ map (windows 4) inside

windows m = foldr (zipWith (:)) (repeat []) . take m . tails

deconstructIp :: IPv7Address -> ([String], [String])
deconstructIp xs = (outsideBrackets, insideBrackets)
    where
        bracketSplit = zip (split (condense . dropDelims $ oneOf("[]")) xs) [0..]
        outsideBrackets = map fst $ filter (even . snd) bracketSplit 
        insideBrackets = map fst $ filter (odd . snd) bracketSplit

part1 = do
    contents <- readFile "day7input.txt"
    print $ length $ filter id $ map supportsTLS $ lines contents
