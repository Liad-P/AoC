-- https://adventofcode.com/2024/day/4
module AoC2024Day4 where

import Data.Maybe (catMaybes)

-- recursive
-- accumulator - (count, 2D array of chars)
-- If you find an X search for a M, then based on that direction search for a A then S in the same direction of the X and M

-- Need to check if y is large than or equal to rows in order to know if you have scanned everything
-- incCoord :: (Int, Int) -> [String] -> (Int, Int)
-- incCoord (x, y) inputArrayOfStrings
--   | x + 1 >= numberOfCols = (0, y + 1)
--   | otherwise = (x + 1, y)
--   where
--     numberOfCols = length $ head inputArrayOfStrings

-- getCharacterUsingCoords :: [String] -> (Int, Int) -> Char
-- getCharacterUsingCoords inputString (x, y) = (inputString !! y) !! x

-- add2Coords :: (Int, Int) -> (Int, Int) -> [String] -> Maybe (Int, Int)
-- add2Coords (x1, y1) (x2, y2) inputArrayOfStrings =
--   if (fst newCoord >= (length $ head inputArrayOfStrings))
--     || (snd newCoord >= length inputArrayOfStrings)
--     || (fst newCoord < 0)
--     || (snd newCoord < 0)
--     then Nothing
--     else Just newCoord
--   where
--     newCoord = (x1 + x2, y1 + y2)

-- add2CoordsTest = add2Coords (9, 1) (1, 1) testCase

-- checkForCharAroundAnotherChar :: [String] -> Char -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
-- checkForCharAroundAnotherChar inputArrayOfStrings targetChar coordToSearchAround listOfDirections =
--   foldr
--     ( \dir listOfValid ->
--         if (getCharFunc dir == targetChar)
--           then listOfValid ++ [dir]
--           else listOfValid
--     )
--     []
--     listOfDirections
--   where
--     getCharFunc directionCoord = case (add2Coords coordToSearchAround directionCoord inputArrayOfStrings) of
--       Just newCoord -> getCharacterUsingCoords inputArrayOfStrings newCoord
--       Nothing -> '0'

-- test_checkForCharAroundAnotherChar = checkForCharAroundAnotherChar testCase 'M' (4, 0) [(-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0)] -- Expect [(-1,1),(1,1)]

-- listOfDirections = [(-1,-1), (0,-1), (1,-1), (1,0), (1,1), (0,1), (-1,1), (-1,0)]

-- getCountOfXMAS :: [String] -> [(Int, Int)] -> (Int, Int) -> String -> Char -> Int -> Int
-- getCountOfXMAS inputString directions startingCoord stringAccumulator targetChar count
--   | stringAccumulator == "XMAS" = count + 1
--   | directions == [] = count
--   | targetChar == 'M' = foldr (\dir counts -> counts + (getCountOfXMAS inputString [(getNextDirectionFromVec dir)] startingCoord (stringAccumulator ++ ['M']) 'A' 0)) count (checkForCharAroundAnotherChar inputString targetChar startingCoord listOfDirectionsForTargetM)
--   | length directions == 1 && getCharacterAtNextCoord directions == targetChar = getCountOfXMAS inputString [(getNextDirectionFromVec $ head directions)] startingCoord (stringAccumulator ++ [targetChar]) (getNextTargetChar targetChar) count
--   | otherwise = count
--   where
--     listOfDirectionsForTargetM = [(-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0)]
--     getNextDirectionFromVec :: (Int, Int) -> (Int, Int)
--     getNextDirectionFromVec directionVec = (fst directionVec * maxAbsValInCoordPlus1, snd directionVec * maxAbsValInCoordPlus1)
--       where
--         maxAbsValInCoordPlus1 = (maximum (abs <$> directionVec)) + 1

--     getCharacterAtNextCoord directionVecs = case (add2Coords (head directionVecs) startingCoord inputString) of
--       Nothing -> '0'
--       Just newCoord -> getCharacterUsingCoords inputString newCoord

--     getNextTargetChar target
--       | target == 'X' = 'M'
--       | target == 'M' = 'A'
--       | target == 'A' = 'S'
--       | otherwise = '0'

-- test_getCountOfXMAS = getCountOfXMAS testCase [(1, 1)] (4, 0) "X" 'M' 0

-- getNextDirectionFromVec directionVec = (fst directionVec * maxAbsValInCoordPlus1, snd directionVec * maxAbsValInCoordPlus1)
--   where
--     maxAbsValInCoordPlus1 = (maximum (abs <$> directionVec)) + 1

-- test_getNextDirectionFromVec = getNextDirectionFromVec (-1, -1)

-- test_foldRandom = foldr (\dir counts -> counts + (getCountOfXMAS inputString [(getNextDirectionFromVec dir)] startingCoord (stringAccumulator ++ ['M']) 'A' 0)) count (checkForCharAroundAnotherChar inputString targetChar startingCoord listOfDirectionsForTargetM)
--   where
--     inputString = testCase
--     getNextDirectionFromVec directionVec = (fst directionVec * maxAbsValInCoordPlus1, snd directionVec * maxAbsValInCoordPlus1)
--       where
--         maxAbsValInCoordPlus1 = (maximum (abs <$> directionVec)) + 1
--     startingCoord = (4, 0)
--     stringAccumulator = "X"
--     count = 0
--     targetChar = 'M'
--     listOfDirectionsForTargetM = [(-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0)]

-- findXMASInWordSearch :: [String] -> String -> Char -> Int -> (Int, Int) -> Int
-- findXMASInWordSearch inputString accumulateString nextChar countOfXMAS currentCoord
--   | snd (incCoord currentCoord inputString) >= length inputString = countOfXMAS
--   | nextChar == 'X' && getCharacterUsingCoords inputString currentCoord == nextChar = findXMASInWordSearch inputString "X" 'M' countOfXMAS currentCoord
--   | nextChar == 'M' = getCountOfXMAS inputString listOfDirectionsForTargetM currentCoord "X" nextChar countOfXMAS
--   | otherwise = findXMASInWordSearch inputString "" 'X' countOfXMAS (incCoord currentCoord inputString)
--   where
--     listOfDirectionsForTargetM = [(-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0)]

-- testCase =
--   [ "MMMSXXMASM",
--     "MSAMXMSMSA",
--     "AMXSXMAAMM",
--     "MSAMASMSMX",
--     "XMASAMXAMM",
--     "XXAMMXXAMA",
--     "SMSMSASXSS",
--     "SAXAMASAAA",
--     "MAMMMXMMMM",
--     "MXMXAXMASX"
--   ]

-- testingPart1 = findXMASInWordSearch testCase "" 'X' 0 (0, 0)

getCharacterUsingCoordsV2 :: [String] -> (Int, Int) -> Maybe Char
getCharacterUsingCoordsV2 inputString (x, y) =
  if (x >= (length $ head inputString))
    || (y >= length inputString)
    || (x < 0)
    || (y < 0)
    then Nothing
    else Just ((inputString !! y) !! x)

add2CoordsV2 :: (Int, Int) -> (Int, Int) -> (Int, Int)
add2CoordsV2 (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

getPathsFromPoint :: (Int, Int) -> [[(Int, Int)]]
getPathsFromPoint inputCoord = foldr (\dirVec paths -> paths ++ [getListOfCoordInVecDirection dirVec]) [] listOfDirectionVecs
  where
    getListOfCoordInVecDirection directionVec = go [inputCoord]
      where
        go acc
          | length acc == 4 = acc
          | otherwise = go (acc ++ [add2CoordsV2 (head $ reverse acc) directionVec])

    listOfDirectionVecs = [(-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0)]

getCountOfXMAS :: [String] -> Int
getCountOfXMAS inputString = iterateAndApplyFunctionToEachElement inputString
  where
    getStringFromPath :: [(Int, Int)] -> String
    getStringFromPath path = catMaybes (foldl (\strAcc curCoord -> strAcc ++ [getCharacterUsingCoordsV2 inputString curCoord]) [] path)
    getListOfStringsFromListOfPaths :: [[(Int, Int)]] -> [String]
    getListOfStringsFromListOfPaths = map getStringFromPath
    countMatchingXMAS :: [String] -> Int
    countMatchingXMAS inputStrings = length $ filter (== "XMAS") inputStrings
    getNumberOfXMASFromPoint :: (Int, Int) -> Int
    getNumberOfXMASFromPoint point = countMatchingXMAS $ getListOfStringsFromListOfPaths $ getPathsFromPoint point
    iterateAndApplyFunctionToEachElement :: [String] -> Int
    iterateAndApplyFunctionToEachElement inputString = snd $ foldl (\(i, sumOfXmas) elem -> (i + 1, sumOfXmas + goThroughRow i)) (0, 0) inputString
      where
        goThroughRow :: Int -> Int
        goThroughRow rowNum =
          snd $
            foldl
              ( \((x, y), count) elem ->
                  if elem == 'X'
                    then ((x + 1, y), count + getNumberOfXMASFromPoint (x, y))
                    else ((x + 1, y), count)
              )
              ((0, rowNum), 0)
              (inputString !! rowNum)

part1 :: IO ()
part1 = do
  fileContent <- readFile "input/inputday4.txt"
  let fileLines = lines fileContent
  print $ getCountOfXMAS fileLines