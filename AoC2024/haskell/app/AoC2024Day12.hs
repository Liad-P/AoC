{-# LANGUAGE ImportQualifiedPost #-}

module AoC2024Day12 where

import Data.List (group, intersect, nub, (\\))
import Data.Map (Map, empty, insert)
import Data.Map qualified as Map

testCase =
  [ "RRRRIICCFF",
    "RRRRIICCCF",
    "VVRRRCCFFF", -- 2, 4
    "VVRCCCJFFF", -- 2, 2
    "VVVVCJJCFE", -- 4, 5
    "VVIVCCJJEE", -- 3, 5
    "VVIIICJJEE", -- 2, 4
    "MIIIIIJJEE", -- 13, 20
    "MIIISIJEEE",
    "MMMISSJEEE"
  ]

data PlantRow = PlantRow Char Int (Int, Int) deriving (Show, Eq)

data PlantField = PlantField [PlantRow] Int deriving (Show, Eq)

getGridOfPlantRows :: [String] -> [PlantRow]
getGridOfPlantRows inputStrings = foldl (\acc i -> (encodedPlants acc (inputStrings !! i) i)) [] [0 .. (length inputStrings - 1)]
  where
    encodedPlants :: [PlantRow] -> String -> Int -> [PlantRow]
    encodedPlants targetList inputString i =
      snd $
        foldl
          ( \acc x ->
              (fst acc + (length x), (snd acc) ++ [(PlantRow (head x) i (fst acc, fst acc + (length x) - 1))])
          )
          (0, targetList)
          (group inputString)

getPlantRowsFromPlantFieldInSpecificRow :: PlantField -> Int -> [PlantRow]
getPlantRowsFromPlantFieldInSpecificRow (PlantField plantRows _) rowNum = filter (\(PlantRow _ i _) -> i == rowNum) plantRows

getPlantRowsInSpecificRow :: [PlantRow] -> Int -> [PlantRow]
getPlantRowsInSpecificRow inputPlantRows rowNum = filter (\(PlantRow _ i _) -> i == rowNum) inputPlantRows

findRowsIntersectingWithPoint :: (Int, Int) -> [PlantRow] -> PlantRow
findRowsIntersectingWithPoint (x, y) plantRows = head $ filter (\(PlantRow _ _ (x1, x2)) -> x >= x1 && x <= x2) rowsWithSameY
  where
    rowsWithSameY = getPlantRowsInSpecificRow plantRows y

findRowsIntersectingWithPointAndChar :: (Int, Int) -> Char -> [PlantRow] -> PlantRow
findRowsIntersectingWithPointAndChar (x, y) targetChar plantRows = head $ filter (\(PlantRow c _ (x1, x2)) -> x >= x1 && x <= x2 && targetChar == c) rowsWithSameY
  where
    rowsWithSameY = getPlantRowsInSpecificRow plantRows y

recursivelyGetConnectedPlantRows :: PlantRow -> [PlantRow] -> [PlantRow] -> [PlantRow]
recursivelyGetConnectedPlantRows initialPlantRow collectedPlantRows allPlantRows
  | null relevantBelowRow && null relevantAboveRow = collectedPlantRows
  | otherwise = concatMap (\x -> recursivelyGetConnectedPlantRows x newAllPlantRows allPlantRows) (relevantAboveRow ++ relevantBelowRow)
  where
    (PlantRow targetChar rowNum (s1, s2)) = initialPlantRow
    getLengthOfIntersection (x1, x2) = length ([x1 .. x2] `intersect` [s1 .. s2])
    relevantAboveRow = filter (\(PlantRow c i (x1, x2)) -> getLengthOfIntersection (x1, x2) > 0 && c == targetChar) (getPlantRowsInSpecificRow allPlantRows (rowNum - 1) \\ collectedPlantRows)
    -- perimeterAdditionFromAbove = sum $ map (\(PlantRow c i (x1, x2)) -> 2 * (x2 - x1 + 1) + 2 - getLengthOfIntersection (x1, x2)) relevantAboveRow
    relevantBelowRow = filter (\(PlantRow c i (x1, x2)) -> getLengthOfIntersection (x1, x2) > 0 && c == targetChar) (getPlantRowsInSpecificRow allPlantRows (rowNum + 1) \\ collectedPlantRows)
    -- perimeterAdditionFromBelow = sum $ map (\(PlantRow c i (x1, x2)) -> 2 * (x2 - x1 + 1) + 2 - getLengthOfIntersection (x1, x2)) relevantBelowRow
    newAllPlantRows = collectedPlantRows ++ relevantAboveRow ++ relevantBelowRow

recursivelyGetConnectedPlantRowsWithPeri :: PlantRow -> [PlantRow] -> [PlantRow] -> Int -> ([PlantRow], Int)
recursivelyGetConnectedPlantRowsWithPeri initialPlantRow collectedPlantRows allPlantRows perimeter
  | null relevantBelowRow && null relevantAboveRow = (collectedPlantRows, perimeter)
  | otherwise = (plantRowsToReturn, perimeterToReturn)
  where
    (PlantRow targetChar rowNum (s1, s2)) = initialPlantRow
    getLengthOfIntersection (x1, x2) = length ([x1 .. x2] `intersect` [s1 .. s2])
    relevantAboveRow = filter (\(PlantRow c i (x1, x2)) -> getLengthOfIntersection (x1, x2) > 0 && c == targetChar) (getPlantRowsInSpecificRow allPlantRows (rowNum - 1) \\ collectedPlantRows)
    perimeterAdditionFromAbove = sum $ map (\(PlantRow c i (x1, x2)) -> 2 * (x2 - x1 + 1) + 2 - 2 * getLengthOfIntersection (x1, x2)) relevantAboveRow
    relevantBelowRow = filter (\(PlantRow c i (x1, x2)) -> getLengthOfIntersection (x1, x2) > 0 && c == targetChar) (getPlantRowsInSpecificRow allPlantRows (rowNum + 1) \\ collectedPlantRows)
    perimeterAdditionFromBelow = sum $ map (\(PlantRow c i (x1, x2)) -> 2 * (x2 - x1 + 1) + 2 - 2 * getLengthOfIntersection (x1, x2)) relevantBelowRow
    newAllPlantRows = collectedPlantRows ++ relevantAboveRow ++ relevantBelowRow
    plantRowsAndPeriFromFuturePlantRows = map (\x -> recursivelyGetConnectedPlantRowsWithPeri x newAllPlantRows allPlantRows 0) (relevantAboveRow ++ relevantBelowRow)
    perimeterToReturn = perimeter + perimeterAdditionFromAbove + perimeterAdditionFromBelow + (sum $ map (\(_, p) -> p) plantRowsAndPeriFromFuturePlantRows)
    plantRowsToReturn = concatMap (\(pr, _) -> pr) plantRowsAndPeriFromFuturePlantRows

getPerimeterOfPlantRow :: PlantRow -> Int
getPerimeterOfPlantRow (PlantRow _ _ (x1, x2)) = 2 * (x2 - x1 + 1) + 2

getAreaFromPlantRows :: [PlantRow] -> Int
getAreaFromPlantRows plantRows = sum $ map (\(PlantRow _ _ (x1, x2)) -> (x2 - x1) + 1) plantRows

getFieldGroupsFromPlantRows :: [PlantRow] -> [PlantField] -> [PlantField]
getFieldGroupsFromPlantRows plantRowsLeftOvers plantFields
  | null plantRowsLeftOvers = plantFields
  | otherwise = getFieldGroupsFromPlantRows newPlantRowsLeftOver (plantFields ++ [newPlantField])
  where
    (plantRowsForNewField, perimeter) = recursivelyGetConnectedPlantRowsWithPeri (head plantRowsLeftOvers) [head plantRowsLeftOvers] (tail plantRowsLeftOvers) (getPerimeterOfPlantRow (head plantRowsLeftOvers))
    newPlantRowsLeftOver = plantRowsLeftOvers \\ plantRowsForNewField
    newPlantField = PlantField (nub plantRowsForNewField) perimeter

getPlantRowsFromPlantField :: PlantField -> [PlantRow]
getPlantRowsFromPlantField (PlantField plantRows _) = plantRows

getValueOfFields :: [PlantField] -> [Int]
getValueOfFields plantFields = map (\(PlantField pR peri) -> peri * getAreaFromPlantRows pR) plantFields

testing :: IO ()
testing = do
  fileContent <- readFile "input/inputday12.txt"
  -- let inputStrings = lines fileContent
  let inputStrings = testCase
  let plantRows = getGridOfPlantRows inputStrings
  -- let foundPlantRow = findRowsIntersectingWithPoint (0, 2) plantRows
  -- let (fieldOfRPlants, peri) = recursivelyGetConnectedPlantRowsWithPeri foundPlantRow [foundPlantRow] plantRows (getPerimeterOfPlantRow foundPlantRow)
  -- let area = getAreaFromPlantRows fieldOfRPlants
  let plantFields = getFieldGroupsFromPlantRows plantRows []
  let result = getValueOfFields plantFields
  -- let perimeter = getPerimeterFromPlantRows (getPlantRowsFromPlantField (head plantFields)) 0
  print $ sum result

-- map (startingPoint, char) : Spans  -> Spans = [(i, (x1,x2))]
-- Recurse through Spans of map
-- Create function to getSpan at point
-- How to recurse from span search everywhere arround current span that has the same char
