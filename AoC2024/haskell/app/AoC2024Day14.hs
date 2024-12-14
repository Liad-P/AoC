module AoC2024Day14 where

import Data.List (nub)
import Text.Parsec
import Text.Parsec.String (Parser)

testCase =
  unlines
    [ "p=0,4 v=3,-3",
      "p=6,3 v=-1,-3",
      "p=10,3 v=-1,2",
      "p=2,0 v=2,-1",
      "p=0,0 v=1,3",
      "p=3,0 v=-2,-2",
      "p=7,6 v=-1,-3",
      "p=3,0 v=-1,-2",
      "p=9,3 v=2,3",
      "p=7,3 v=-1,2",
      "p=2,4 v=2,-3",
      "p=9,5 v=-3,-3"
    ]

data Point = Point (Int, Int) deriving (Show, Eq)

data Robot = Robot Point Point deriving (Show, Eq)

parseLine :: Parser Robot
parseLine = do
  string "p="
  pX <- many1 digit
  string ","
  pY <- many1 digit
  string " v="
  vXSign <- optionMaybe (char '-')
  vX <- many1 digit
  string ","
  vYSign <- optionMaybe (char '-')
  vY <- many1 digit
  optional endOfLine
  let signedVX =
        ( case vXSign of
            Just _ -> -1 * read vX
            Nothing -> read vX
        )
  let signedVY =
        ( case vYSign of
            Just _ -> -1 * read vY
            Nothing -> read vY
        )
  pure (Robot (Point (read pX, read pY)) (Point (signedVX, signedVY)))

tilesWide = 101

-- tilesWide = 11

tilesTall = 103

-- tilesTall = 7

moveRobotNSeconds :: Int -> Robot -> (Int, Int)
moveRobotNSeconds seconds (Robot (Point (pX, pY)) (Point (vX, vY))) = ((pX + (seconds * vX)) `mod` tilesWide, (pY + (seconds * vY)) `mod` tilesTall)

getSafetyFactorForEachQuadrand :: [(Int, Int)] -> [Int]
getSafetyFactorForEachQuadrand inputPositions = [firstQ, secondQ, thirdQ, fourthQ]
  where
    positionsInTermsOfQuadrants =
      map
        ( \(x, y) ->
            if x < (tilesWide `div` 2) && y < (tilesTall `div` 2)
              then 1
              else
                if x > (tilesWide `div` 2) && y < (tilesTall `div` 2)
                  then 2
                  else
                    if x < (tilesWide `div` 2) && y > (tilesTall `div` 2)
                      then 3
                      else
                        if x > (tilesWide `div` 2) && y > (tilesTall `div` 2)
                          then 4
                          else -1
        )
        inputPositions
    firstQ = length $ filter (== 1) positionsInTermsOfQuadrants
    secondQ = length $ filter (== 2) positionsInTermsOfQuadrants
    thirdQ = length $ filter (== 3) positionsInTermsOfQuadrants
    fourthQ = length $ filter (== 4) positionsInTermsOfQuadrants

part1 :: IO ()
part1 = do
  fileContent <- readFile "input/inputday14.txt"
  --   let fileContent = testCase
  let robots =
        ( case (parse (many parseLine) "" fileContent) of
            Left _ -> []
            Right res -> res
        )
  let robotPositionsAfter100Seconds = map (\x -> moveRobotNSeconds 100 x) robots
  let safetyScore = product $ getSafetyFactorForEachQuadrand robotPositionsAfter100Seconds
  print safetyScore

-- Part 2

doRobotsOverLap :: [(Int, Int)] -> Bool
doRobotsOverLap inputPositions = length inputPositions > (length $ nub inputPositions)

iterateThroughRobotsUntilNoOverlap :: [Robot] -> Int
iterateThroughRobotsUntilNoOverlap robots = last $ takeWhile (\i -> (doRobotsOverLap $ (map (\x -> moveRobotNSeconds i x) robots))) [1 ..]

part2 :: IO ()
part2 = do
  fileContent <- readFile "input/inputday14.txt"
  --   let fileContent = testCase
  let robots =
        ( case (parse (many parseLine) "" fileContent) of
            Left _ -> []
            Right res -> res
        )
  let result = iterateThroughRobotsUntilNoOverlap robots
  print (result + 1)