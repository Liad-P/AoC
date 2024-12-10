module AoC2024Day10 where

import Data.List (elemIndices, nub)

data Point = Point (Int, Int) deriving (Show, Eq)

testCase =
  [ "89010123",
    "78121874",
    "87430965",
    "96549874",
    "45678903",
    "32019012",
    "01329801",
    "10456732"
  ]

convertInputStringsToListOfInts :: [String] -> [[Int]]
convertInputStringsToListOfInts inputStrings = map (\x -> (map (\y -> read [y]) x)) inputStrings

isPointOutOfBound :: [[Int]] -> Point -> Bool
isPointOutOfBound inputNums (Point (x, y))
  | y < 0 || y >= length inputNums = True
  | x < 0 || x >= length (head inputNums) = True
  | otherwise = False

addPoints :: Point -> Point -> Point
addPoints (Point (x1, y1)) (Point (x2, y2)) = Point (x1 + x2, y1 + y2)

getNumberFromPoint :: [[Int]] -> Point -> Int
getNumberFromPoint inputNums (Point (x, y)) = inputNums !! y !! x

findCoordsOfStartingPoints :: [[Int]] -> [Point]
findCoordsOfStartingPoints inputNums = foldl (\acc i -> acc ++ map (\x -> Point (x, i)) (elemIndices 0 (inputNums !! i))) [] [0 .. ((length $ head inputNums) - 1)]

iterateThroughPath :: [[Int]] -> Point -> [Point]
iterateThroughPath topMap point
  | isPointOutOfBound topMap point = []
  | length pointsOfEndings > 0 = pointsOfEndings
  | otherwise = foldr (\x acc -> acc ++ (iterateThroughPath topMap x)) [] validSurroundingPoints
  where
    surroundingPoints = [addPoints point (Point (0, 1)), addPoints point (Point (0, -1)), addPoints point (Point (1, 0)), addPoints point (Point (-1, 0))]
    validSurroundingPoints = filter (\x -> not (isPointOutOfBound topMap x) && ((getNumberFromPoint topMap x) - (getNumberFromPoint topMap point)) == 1) surroundingPoints
    pointsOfEndings = filter (\x -> (getNumberFromPoint topMap x) == 9) validSurroundingPoints

part1 :: IO ()
part1 = do
  fileContent <- readFile "input/inputday10.txt"
  let fileLines = lines fileContent
  --   let fileLines = testCase
  let topMap = convertInputStringsToListOfInts fileLines
  let startingPositions = findCoordsOfStartingPoints topMap
  let endingPoints = foldr (\x acc -> acc ++ (nub $ iterateThroughPath topMap x)) [] startingPositions

  print $ length endingPoints

testCasePart2 =
  [ "89010123",
    "78121874",
    "87430965",
    "96549874",
    "45678903",
    "32019012",
    "01329801",
    "10456732"
  ]

part2 :: IO ()
part2 = do
  fileContent <- readFile "input/inputday10.txt"
  let fileLines = lines fileContent
  --   let fileLines = testCasePart2
  let topMap = convertInputStringsToListOfInts fileLines
  let startingPositions = findCoordsOfStartingPoints topMap
  let endingPoints = foldr (\x acc -> acc ++ (iterateThroughPath topMap x)) [] startingPositions

  print $ length endingPoints