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

getGridOfPlants :: [String] -> [[((Int, Int), Char)]]
getGridOfPlants inputStrings = map (\x -> (encodedPlants x)) inputStrings
  where
    encodedPlants inputString = snd $ foldl (\acc x -> (fst acc + (length x), snd acc ++ [((fst acc, fst acc + (length x) - 1), head x)])) (0, []) (group inputString)

-- getGridOfPlantsV2 :: [String] -> [[((Int, Int), Char, Bool)]]
-- getGridOfPlantsV2 inputStrings = map (\x -> (encodedPlants x)) inputStrings
--   where
--     encodedPlants inputString = snd $ foldl (\acc x -> (fst acc + (length x), snd acc ++ [((fst acc, fst acc + (length x) - 1), head x, False)])) (0, []) (group inputString)

data Span = Span [(Int, Int)] deriving (Show, Eq)

data Point = Point (Int, Int) deriving (Show, Eq, Ord)

data Group = Group Char Point Span Int Int deriving (Show, Eq)

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

-- newPerimeter = perimeter + perimeterAdditionFromBelow + perimeterAdditionFromAbove

-- getPerimeterFromPlantRows :: [PlantRow] -> Int -> Int
-- getPerimeterFromPlantRows plantRows trackingPerimeter
--   | null plantRows = trackingPerimeter
--   | (perimeterAdditionFromAbove + perimeterAdditionFromBelow) == 0 = trackingPerimeter
--   | otherwise = sum ([trackingPerimeter, perimeterAdditionFromAbove, perimeterAdditionFromBelow]) + sum (map (\x -> getPerimeterFromPlantRows ([x] ++ (tail plantRows)) 0) (relevantAboveRow ++ relevantBelowRow))
--   where
--     (PlantRow targetChar rowNum (s1, s2)) = head plantRows
--     getLengthOfIntersection (x1, x2) = length ([x1 .. x2] `intersect` [s1 .. s2])
--     relevantAboveRow = filter (\(PlantRow c i (x1, x2)) -> getLengthOfIntersection (x1, x2) > 0) (getPlantRowsInSpecificRow plantRows (rowNum - 1))
--     relevantBelowRow = filter (\(PlantRow c i (x1, x2)) -> getLengthOfIntersection (x1, x2) > 0) (getPlantRowsInSpecificRow plantRows (rowNum + 1))
--     perimeterAdditionFromAbove = sum $ map (\(PlantRow c i (x1, x2)) -> 2 * (x2 - x1 + 1) + 2 - getLengthOfIntersection (x1, x2)) relevantAboveRow
--     perimeterAdditionFromBelow = sum $ map (\(PlantRow c i (x1, x2)) -> 2 * (x2 - x1 + 1) + 2 - getLengthOfIntersection (x1, x2)) relevantBelowRow

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
    -- PlantRow Char y (x1, x2) = head plantRowsLeftOvers
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

-- getCharFromPlantRow :: PlantRow -> Char
-- getCharFromPlantRow (PlantRow c _ _) = c

-- getSpanFromPlantRow (PlantRow _ _ s) = s

-- getRowNumberFromPlantRow (PlantRow _ y _) = y

-- getFirstRowAsPlantFields :: [PlantRow] -> [PlantField]
-- getFirstRowAsPlantFields gridOfPlantRows = map (\x -> PlantField [x]) (filter (\pr -> getRowNumberFromPlantRow pr == 0) gridOfPlantRows)

-- getPlantRowKeysThatAreTouchingSpan :: [PlantRow] -> PlantRow -> ([PlantRow], [PlantRow])
-- getPlantRowKeysThatAreTouchingSpan listOfPlantRows targetPlantRow = (plantRowsThatOverLap, plantRowsThatDontOverlap)
--   where
--     PlantRow ct ci (cx1, cx2) = targetPlantRow
--     (plantRowsThatOverLap, plantRowsThatDontOverlap) = span (\(PlantRow c i (x1, x2)) -> ((x1 <= cx1 && cx1 <= x2) || (x1 <= cx2 && cx1 <= x2)) && ct == c && ci == i) listOfPlantRows

-- createNewGroupFromPlantRows gridOfPlantRows = map (\x -> PlantField [x]) gridOfPlantRows

-- processesNextPlantRow nextPlantRows currentPlantFields = foldl (\(PlantField currentPlantFields, notUsedSpans) x -> (PlantField (currentPlantFields ++ getPlantRowKeysThatAreTouchingSpan))) nextPlantRows

-- -- recurseGridOfPlants previousGroup
-- getGroupsOfFirstRow :: [((Int, Int), Char)] -> [Group]
-- getGroupsOfFirstRow = foldr (\((x1, x2), c) acc -> acc ++ [Group c (Point (x1, 0)) (Span [(x1, x2)]) (x2 - x1 + 1) (2 * (x2 - x1 + 1) + 2)]) []

-- getCurrentSpanOfGroup :: Group -> [(Int, Int)]
-- getCurrentSpanOfGroup (Group _ _ (Span (targetSpan)) _ _) = targetSpan

-- getStartingPointOfGroup :: Group -> Point
-- getStartingPointOfGroup (Group _ point _ _ _) = point
-- getCharOfGroup :: Group -> Char
-- getCharOfGroup (Group c _ _ _ _) = c

-- getGroupWithMostUpLeftStart:: Group -> Group -> Group
-- getGroupWithMostUpLeftStart group1 group2
--   | x1 + y1 > x2 + y2 = group2
--   | otherwise = group1
--   where
--     Point (x1,y1) = getStartingPointOfGroup group1
--     Point (x2,y2) = getStartingPointOfGroup group2

-- createGroupWithNewSpans (Group c p _ a per) newSpans = (Group c p (Span(newSpans)) a per)

-- addPeriAndAreaToGroup (Group c p s a per) plusarea plusperi = (Group c p s (a + plusarea) (per plusperi))

-- doSpansOverlap listOfSpans (x1, x2) = any (\(cx1, cx2) -> (x1 <= cx1 && cx1 <= x2) || (x1 <= cx2 && cx1 <= x2)) listOfSpans

-- getGroupsFromPlantGrid :: [[((Int, Int), Char)]] -> Map Char Group -> Group
-- getGroupsFromPlantGrid inputGrid groups=
--     where
--         targetRow = head inputGrid

--         targetRowSpans = map (\(s,_) -> s) targetRow

--         listOfGroupChars = Map.keys groups

--         combinedGroupsMap = foldr (\x acc -> insert x (getIfGroupsOverlapInRow (groups Map.! x) targetRowSpans) acc)

--         getIfGroupsOverlapInRow groups targetSpans
--           | null targetSpans = groups
--           | otherwise = filteredGroupsThatDontTouchSpan ++ [getUpperMostLeftGroups]
--           where
--             -- go through each span, check which groups touch that span, if 2 or more groups do make them one group
--             currentSpan = head targetSpans
--             (filteredGroupsThatTouchSpan, filteredGroupsThatDontTouchSpan) = span (\x -> doSpansOverlap (getCurrentSpanOfGroup x) currentSpan) groups

--             getUpperMostLeftGroups = foldr (\x acc -> getGroupWithMostUpLeftStart acc x) (head filteredGroupsThatTouchSpan) (tail filteredGroupsThatTouchSpan)

--             checkIfSpansOverlapIfYesDoCalc getUpperMostLeftGroups targetSpans

--         -- prevRow = head inputGrid

--         checkIfSpansOverlapIfYesDoCalc group spansFromCurrentRow =
--           if length filteredSpansFromRow > 0 then (leftOverSpans, filteredSpansFromRow, newGroup)
--           else (leftOverSpans, filteredSpansFromRow, group)

--           where
--             groupsSpans = getCurrentSpanOfGroup group
--             (filteredSpansFromRow, leftOverSpans) = span (\(x,c) -> doSpansOverlap groupsSpans x && (getCharOfGroup group) == c) spansFromCurrentRow

--             areaOfNewSpans = sum $ map (\(x1,x2) -> x2 -1x + 1) filteredSpansFromRow
--             periOfNewSpans = sum $ map (\(c1,c2) -> map (\(x1,x2) -> (2 * (x2 - x1 + 1) + 2 - (minimum (x2,c2) - maximum (x1,c1))))) filteredSpansFromRow
--             newGroup = addPeriAndAreaToGroup (createGroupWithNewSpans group filteredSpansFromRow) areaOfNewSpans periOfNewSpans

-- map (startingPoint, char) : Spans  -> Spans = [(i, (x1,x2))]
-- Recurse through Spans of map
-- Create function to getSpan at point
-- How to recurse from span search everywhere arround current span that has the same char
