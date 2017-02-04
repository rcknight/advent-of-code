module Advent1 where

import Data.List
import Data.List.Split

data Direction
  = N
  | S
  | E
  | W
  deriving (Read, Show)

data Turn
  = R
  | L
  deriving (Read, Show)

type Distance = Int

type Position = (Int, Int)

type Vector = (Direction, Distance)

type Input = (Turn, Distance)

type Walker = (Position, Direction)

type History = [Position]

parseInputs :: String -> [Input]
parseInputs s = map parseInput processed
  where
    processed = split (condense . dropDelims $ oneOf ", ") s
    parseInput (d:l) = (read [d], read l)

move :: Position -> Vector -> Position
move (x, y) (N, dist) = (x, y + dist)
move (x, y) (S, dist) = (x, y - dist)
move (x, y) (E, dist) = (x + dist, y)
move (x, y) (W, dist) = (x - dist, y)

turn :: Direction -> Turn -> Direction
turn N L = W
turn N R = E
turn S L = E
turn S R = W
turn E R = S
turn E L = N
turn W L = S
turn W R = N

step :: Input -> Walker -> Walker
step (t, dist) (currentPos, currentDir) = (newPosition, newDirection)
  where
    newDirection = turn currentDir t
    newPosition = move currentPos (newDirection, dist)

question =
  parseInputs
    "R1, R1, R3, R1, R1, L2, R5, L2, R5, R1, R4, L2, R3, L3, R4, L5, R4, R4, R1, L5, L4, R5, R3, L1, R4, R3, L2, L1, R3, L4, R3, L2, R5, R190, R3, R5, L5, L1, R54, L3, L4, L1, R4, R1, R3, L1, L1, R2, L2, R2, R5, L3, R4, R76, L3, R4, R191, R5, R5, L5, L4, L5, L3, R1, R3, R2, L2, L2, L4, L5, L4, R5, R4, R4, R2, R3, R4, L3, L2, R5, R3, L2, L1, R2, L3, R2, L1, L1, R1, L3, R5, L5, L1, L2, R5, R3, L3, R3, R5, R2, R5, R5, L5, L5, R2, L3, L5, L2, L1, R2, R2, L2, R2, L3, L2, R3, L5, R4, L4, L5, R3, L4, R1, R3, R2, R4, L2, L3, R2, L5, R5, R4, L2, R4, L1, L3, L1, L3, R1, R2, R1, L5, R5, R3, L3, L3, L2, R4, R2, L5, L1, L1, L5, L4, L1, L1, R1"

part1 :: [Input] -> Distance
part1 inputs = abs (fst finalPosition) + abs (snd finalPosition)
  where
    finalPosition = fst $ foldr step ((0, 0), N) inputs

firstRevisit :: History -> Position
firstRevisit = go []
  where
    go :: History -> History -> Position
    go _ [] = (0, 0)
    go hist (x:xs) =
      if x `elem` hist
        then x
        else go (x : hist) xs

scanMove :: Position -> Vector -> History
scanMove pos (dir, dist) = scanl move pos (take dist (repeat (dir, 1)))

scanStep :: Input -> (Walker, History) -> (Walker, History)
scanStep (t, dist) ((currentPos, currentDir), hist) = ((newPosition, newDirection), hist ++ steps)
  where
    newDirection = turn currentDir t
    steps = tail (scanMove currentPos (newDirection, dist))
    newPosition = last steps

getHistory :: [Input] -> (Walker, History)
getHistory = foldl (flip scanStep) (((0, 0), N), [])

part2 :: [Input] -> Distance
part2 inputs = abs (fst hqLocation) + abs (snd hqLocation)
  where
    hqLocation = firstRevisit (snd (getHistory inputs))
