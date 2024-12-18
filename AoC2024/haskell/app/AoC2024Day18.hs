{-# LANGUAGE ImportQualifiedPost #-}

module AoC2024Day18 where

import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace (trace)
import Text.Parsec
import Text.Parsec.String (Parser)

testCase =
  unlines
    [ "5,4",
      "4,2",
      "4,5",
      "3,0",
      "2,1",
      "6,3",
      "2,4",
      "1,5",
      "0,6",
      "3,3",
      "2,6",
      "5,1",
      "1,2",
      "5,5",
      "2,5",
      "6,5",
      "1,4",
      "0,4",
      "6,4",
      "1,1",
      "6,1",
      "1,0",
      "0,5",
      "1,6",
      "2,0"
    ]

type Point = (Int, Int)

data Item = Wall | Open | Start | End deriving (Show, Eq)

data Direction = LEFT | RIGHT | UP | DOWN deriving (Show, Eq, Ord)

type Maze = Map.Map Point Item

type Path = Set.Set Point

directionToPoint :: Direction -> Point
directionToPoint UP = (0, -1)
directionToPoint DOWN = (0, 1)
directionToPoint LEFT = (-1, 0)
directionToPoint RIGHT = (1, 0)

getNextPosition :: Point -> Direction -> Point
getNextPosition (x, y) dir = (x + dx, y + dy)
  where
    (dx, dy) = directionToPoint dir

getItemAtPoint :: Maze -> Point -> Item
getItemAtPoint maze point = case (Map.lookup point maze) of
  Just res -> res
  Nothing -> Wall

getPossibleNextDirectionsAsPoints :: Direction -> [Direction]
getPossibleNextDirectionsAsPoints UP = [UP, RIGHT, LEFT]
getPossibleNextDirectionsAsPoints DOWN = [DOWN, RIGHT, LEFT]
getPossibleNextDirectionsAsPoints RIGHT = [UP, RIGHT, DOWN]
getPossibleNextDirectionsAsPoints LEFT = [UP, DOWN, LEFT]

recurseThroughPaths :: Maze -> Point -> Direction -> Path -> Int -> [Maybe Int]
recurseThroughPaths maze curPoint curDir path curPathVal
  --   | getItemAtPoint maze curPoint == End = trace ("Found path: " ++ show curPathVal) ([Just curPathVal])
  | getItemAtPoint maze curPoint == End = [Just curPathVal]
  | null validNextPointsAndDirections = [Nothing]
  | otherwise = valuesFromNextPaths
  where
    valuesFromNextPaths =
      let newResults =
            foldl
              ( \acc (point, dir) ->
                  acc ++ (recurseThroughPaths maze point dir (Set.insert point path) (curPathVal + 1))
              )
              []
              validNextPointsAndDirections
       in newResults

    nextPossibleDirections = getPossibleNextDirectionsAsPoints curDir
    nextPossiblePoints = map (\dir -> (getNextPosition curPoint dir, dir)) nextPossibleDirections
    validNextPointsAndDirections = filter (\(point, dir) -> ((getItemAtPoint maze point) == Open || (getItemAtPoint maze point) == End) && Set.notMember point path) nextPossiblePoints

createMaze :: [Point] -> Int -> Maze
createMaze points size = foldl (\accMaze x -> Map.insert x Wall accMaze) initialMap points
  where
    startingPoint = (0, 0)
    endingPoint = (size, size)
    initialMap :: Maze
    initialMap = Map.union (Map.fromList [(startingPoint, Start), (endingPoint, End)]) (Map.fromList [((x, y), Open) | x <- [0 .. size], y <- [0 .. size]])

coordParser :: Parser Point
coordParser = do
  x <- many1 digit
  char ','
  y <- many1 digit
  optional endOfLine
  pure (read x, read y)

parseCorruptedMemory :: Parser [Point]
parseCorruptedMemory = do
  points <- many1 coordParser
  pure points

part1 :: IO ()
part1 = do
  fileContent <- readFile "input/inputday18.txt"
  --   let fileContent = testCase
  let corruptedMemory =
        ( case (parse parseCorruptedMemory "" fileContent) of
            Left _ -> error "Failed while parsing corrupted memory"
            Right res -> res
        )
  let maze = createMaze (take 1024 corruptedMemory) 70
  let result = minimum $ catMaybes $ recurseThroughPaths maze (0, 0) RIGHT Set.empty 0
  print result
