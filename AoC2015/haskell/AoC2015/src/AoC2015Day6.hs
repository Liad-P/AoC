-- https://adventofcode.com/2015/day/6

module AoC2015Day6 where

import Text.Parsec
import Text.Parsec.String (Parser)

doActionToArray :: (Int -> Int) -> (Int, Int) -> (Int, Int) -> [[Int]] -> [[Int]]
doActionToArray f (startX, startY) (endX, endY) input = foldr (\y acc -> performFunctionOnRow f (startX, endX) y acc) input [startY .. endY]
  where
    performFunctionOnRow :: (Int -> Int) -> (Int, Int) -> Int -> [[Int]] -> [[Int]]
    performFunctionOnRow f (startX, endX) rowNum grid = take rowNum grid ++ [modifyRow f (startX, endX) (grid !! rowNum)] ++ drop (rowNum + 1) grid
    modifyRow :: (Int -> Int) -> (Int, Int) -> [Int] -> [Int]
    modifyRow f (startX, endX) row = take startX row ++ map f (drop startX (take (endX + 1) row)) ++ drop (endX + 1) row

data Action
  = TurnOn (Int, Int) (Int, Int)
  | TurnOff (Int, Int) (Int, Int)
  | Toggle (Int, Int) (Int, Int)
  deriving (Show, Eq)

parseLineWithActions :: Parser Action
parseLineWithActions =
  try (parseLine (string "turn on ") TurnOn)
    <|> try (parseLine (string "turn off ") TurnOff)
    <|> try (parseLine (string "toggle ") Toggle)

parseCoord :: Parser (Int, Int)
parseCoord = do
  x <- read <$> many1 digit
  char ','
  y <- read <$> many1 digit
  pure (x, y)

parseLine :: (Parser String) -> ((Int, Int) -> (Int, Int) -> Action) -> Parser Action
parseLine stringParser targetAction = do
  stringParser
  (x1, y1) <- parseCoord
  string " through "
  (x2, y2) <- parseCoord
  pure (targetAction (x1, y1) (x2, y2))

performAction :: Action -> [[Int]] -> [[Int]]
performAction (TurnOn (x1, y1) (x2, y2)) = doActionToArray (\x -> 1) (x1, y1) (x2, y2)
performAction (TurnOff (x1, y1) (x2, y2)) = doActionToArray (\x -> 0) (x1, y1) (x2, y2)
performAction (Toggle (x1, y1) (x2, y2)) = doActionToArray (\x -> if x == 1 then 0 else 1) (x1, y1) (x2, y2)

performActionOnEachLine :: [String] -> [[Int]] -> [[Int]]
performActionOnEachLine input lights =
  foldl
    ( \acc line ->
        let result = parse parseLineWithActions "" line
         in case result of
              Left err -> acc
              Right ns -> performAction ns acc
    )
    lights
    input

make2DArrayOfZeros :: Int -> Int -> [[Int]]
make2DArrayOfZeros rows cols = replicate rows (replicate cols 0)

sumNumberOfOnLights :: [[Int]] -> Int
sumNumberOfOnLights lights = foldr (\row count -> count + sum row) 0 lights

day6part1 :: IO ()
day6part1 = do
  content <- readFile "./src/inputDay6.txt"
  let inputStrings = lines content
  let lights = make2DArrayOfZeros 1000 1000
  let adjustedLights = performActionOnEachLine inputStrings lights
  print $ sumNumberOfOnLights adjustedLights

-- PART 2

performActionPart2 :: Action -> [[Int]] -> [[Int]]
performActionPart2 (TurnOn (x1, y1) (x2, y2)) = doActionToArray (\x -> x + 1) (x1, y1) (x2, y2)
performActionPart2 (TurnOff (x1, y1) (x2, y2)) = doActionToArray (\x -> if x >= 1 then x - 1 else x) (x1, y1) (x2, y2)
performActionPart2 (Toggle (x1, y1) (x2, y2)) = doActionToArray (\x -> x + 2) (x1, y1) (x2, y2)

performActionOnEachLinePart2 :: [String] -> [[Int]] -> [[Int]]
performActionOnEachLinePart2 input lights =
  foldl
    ( \acc line ->
        let result = parse parseLineWithActions "" line
         in case result of
              Left err -> acc
              Right ns -> performActionPart2 ns acc
    )
    lights
    input

day6part2 :: IO ()
day6part2 = do
  content <- readFile "./src/inputDay6.txt"
  let inputStrings = lines content
  let lights = make2DArrayOfZeros 1000 1000
  let adjustedLights = performActionOnEachLinePart2 inputStrings lights
  print $ sumNumberOfOnLights adjustedLights