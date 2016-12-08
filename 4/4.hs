module Advent4 where

import Data.List
import Data.List.Split
import Data.Char

data Room = Room {
    name :: [Char],
    sectorId :: Int,
    checksum :: [Char]
} deriving Show


readRoom :: String -> Room
readRoom s =
    let 
        bracketSplit = split (condense . dropDelims $ oneOf("[]")) s
        nameAndSector = split (condense . dropDelims $ oneOf("-")) (bracketSplit !! 0)
    in 
        Room { 
            name = concat $ take ((length nameAndSector) - 1) nameAndSector,
            sectorId = read $ last nameAndSector,
            checksum = bracketSplit !! 1 
        }

orderLetters :: [Char] -> [Char] -> Ordering
orderLetters c1 c2 = 
    let 
        lengthComparison = compare (length c2) (length c1)
    in 
        case lengthComparison of 
            EQ -> compare c1 c2
            _ -> lengthComparison


doChecksum :: String -> String
doChecksum s =
        take 5 $ map head $ sortBy orderLetters $ group $ sort $ filter isAlpha s

isValid :: Room -> Bool
isValid r = doChecksum (name r) == checksum r

part1 = do
    contents <- readFile "day4input.txt"
    let rooms = map readRoom $ lines contents
    let validRooms = filter isValid rooms
    print $ sum $ map sectorId validRooms
