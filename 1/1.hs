module Advent1 where

import Data.List.Split
import Data.List

data Direction = N | S | E | W
      deriving (Read, Show, Enum, Eq, Ord)

data Turn = R | L
      deriving (Read, Show, Enum, Eq, Ord)

type Length = Int
type Input = (Turn, Length)
type Position = (Int, Int)
type Distance = Int
type Walker = (Position, Direction)

parseInputs :: String -> [Input]
parseInputs s = 
        map parseInput processed
    where
        processed = split (condense . dropDelims $ oneOf ", ") s
        parseInput (d:l) = (read [d], read l)

move :: Position -> Direction -> Distance -> Position
move (x, y) N dist = (x, y + dist)
move (x, y) S dist = (x, y - dist)
move (x, y) E dist = (x + dist, y)
move (x, y) W dist = (x - dist, y)

turn :: Direction -> Turn -> Direction
turn N L = W
turn N R = E
turn S L = E
turn S R = W
turn E R = S
turn E L = N
turn W L = S
turn W R = N

walk :: [Input] -> Walker -> Walker
walk [] w = w
walk ((t,dist):xs) (currentPos, currentDir) = 
        walk xs (newPosition, newDirection)
    where 
        newDirection = turn currentDir t
        newPosition = move currentPos newDirection dist

question = parseInputs "R1, R1, R3, R1, R1, L2, R5, L2, R5, R1, R4, L2, R3, L3, R4, L5, R4, R4, R1, L5, L4, R5, R3, L1, R4, R3, L2, L1, R3, L4, R3, L2, R5, R190, R3, R5, L5, L1, R54, L3, L4, L1, R4, R1, R3, L1, L1, R2, L2, R2, R5, L3, R4, R76, L3, R4, R191, R5, R5, L5, L4, L5, L3, R1, R3, R2, L2, L2, L4, L5, L4, R5, R4, R4, R2, R3, R4, L3, L2, R5, R3, L2, L1, R2, L3, R2, L1, L1, R1, L3, R5, L5, L1, L2, R5, R3, L3, R3, R5, R2, R5, R5, L5, L5, R2, L3, L5, L2, L1, R2, R2, L2, R2, L3, L2, R3, L5, R4, L4, L5, R3, L4, R1, R3, R2, R4, L2, L3, R2, L5, R5, R4, L2, R4, L1, L3, L1, L3, R1, R2, R1, L5, R5, R3, L3, L3, L2, R4, R2, L5, L1, L1, L5, L4, L1, L1, R1"

distance :: [Input] -> Distance
distance inputs =
        abs (fst finalPosition) + abs (snd finalPosition)
    where
        finalPosition = fst $ walk question ((0,0), N)
