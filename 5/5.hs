module Advent5 where

import Crypto.Hash
import Data.ByteString (ByteString)
import Data.Char (isNumber)
import Data.List (isPrefixOf)
import Data.String (fromString)

input = "cxdnnyjw"

hexMD5 :: String -> String
hexMD5 = show . md5 . fromString
  where
    md5 :: ByteString -> Digest MD5
    md5 = hash

generateHashes :: String -> [String]
generateHashes seed = [hexMD5 (seed ++ show i) | i <- [0 ..]]

updatePassword :: String -> (Int, Char) -> String
updatePassword existing (pos, new)
  | pos >= (length existing) = existing
  | (existing !! pos) /= '_' = existing
  | otherwise = (take pos existing) ++ [new] ++ (drop (pos + 1) existing)

part1 :: String -> String
part1 seed = map (!! 5) $ take 8 $ filter (isPrefixOf "00000") $ generateHashes seed

part2 :: String -> [String]
part2 seed =
  scanl updatePassword "________" $
  map (\x -> (read [x !! 5], x !! 6)) $
  filter (\x -> isNumber (x !! 5)) $ filter (isPrefixOf "00000") $ generateHashes seed
