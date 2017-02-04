module Advent4 where

import Data.Char
import Data.List
import Data.List.Split

data Room = Room
  { name :: [Char]
  , sectorId :: Int
  , checksum :: [Char]
  } deriving (Show)

readRoom :: String -> Room
readRoom s =
  let bracketSplit = split (condense . dropDelims $ oneOf ("[]")) s
      nameAndSector = split (condense . dropDelims $ oneOf ("-")) (bracketSplit !! 0)
  in Room
     { name = concat $ intersperse "-" $ take ((length nameAndSector) - 1) nameAndSector
     , sectorId = read $ last nameAndSector
     , checksum = bracketSplit !! 1
     }

orderLetters :: [Char] -> [Char] -> Ordering
orderLetters c1 c2 =
  let lengthComparison = compare (length c2) (length c1)
  in case lengthComparison of
       EQ -> compare c1 c2
       _ -> lengthComparison

doChecksum :: String -> String
doChecksum s = take 5 $ map head $ sortBy orderLetters $ group $ sort $ filter isAlpha s

isValid :: Room -> Bool
isValid r = doChecksum (name r) == checksum r

decryptName :: Room -> String
decryptName r =
  let words = split (condense . dropDelims $ oneOf ("-")) (name r)
      sector = sectorId r
      decryptWord = map (shiftChar sector)
  in unwords $ map decryptWord words

shiftChar :: Int -> Char -> Char
shiftChar shift c = chr ((((ord c) - 97 + shift) `mod` 26) + 97)

part1 = do
  contents <- readFile "day4input.txt"
  let rooms = map readRoom $ lines contents
  let validRooms = filter isValid rooms
  print $ sum $ map sectorId validRooms

part2 = do
  contents <- readFile "day4input.txt"
  let rooms = map readRoom $ lines contents
  let validRooms = filter isValid rooms
  let decryptedRooms = map (\r -> (sectorId r, decryptName r)) validRooms
  let northRooms = filter (\r -> isSubsequenceOf "northpole" (snd r)) decryptedRooms
  print $ head northRooms
