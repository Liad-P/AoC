-- https://adventofcode.com/2024/day/2

module AoC2024Day2 where

import Text.Parsec
import Text.Parsec.String (Parser)

getReportFromLine :: Parser [Int]
getReportFromLine = do
  arrayOfInts <- sepBy (many1 digit) space
  optional endOfLine
  pure $ read <$> arrayOfInts

-- Returns true if it is safe
calculateIfSafe :: [Int] -> Bool
calculateIfSafe [_] = False
calculateIfSafe inputArray = isNElementsSafe inputArray isIncreasing
  where
    isIncreasing = (inputArray !! 1) > head inputArray

isNElementsSafe :: [Int] -> Bool -> Bool
isNElementsSafe [] _ = True
isNElementsSafe [_] _ = True
isNElementsSafe (x1 : x2 : xs) True
  | x2 > x1 && abs (x1 - x2) >= 1 && abs (x1 - x2) <= 3 = isNElementsSafe (x2 : xs) True
  | otherwise = False
isNElementsSafe (x1 : x2 : xs) False
  | x2 < x1 && abs (x1 - x2) >= 1 && abs (x1 - x2) <= 3 = isNElementsSafe (x2 : xs) False
  | otherwise = False

getArrayOfRecords :: Parser [Int] -> String -> IO [[Int]]
getArrayOfRecords parserInput filePath = do
  fileContent <- readFile filePath
  let fileLines = lines fileContent
  let result =
        foldr
          ( \line records ->
              case (parse parserInput "" line) of
                Left _ -> records
                Right record -> records ++ [record]
          )
          []
          fileLines
  pure result

performCalcOnArray :: ([Int] -> Bool) -> [[Int]] -> Int
performCalcOnArray filderCond inputArray = length $ filter filderCond inputArray

part1 :: IO ()
part1 = do
  records <- getArrayOfRecords getReportFromLine "input/inputday2.txt"
  print $ performCalcOnArray calculateIfSafe records

-- Part 2 V1

calculateIfSafePart2 :: [Int] -> Bool
calculateIfSafePart2 [] = True
calculateIfSafePart2 [_] = True
calculateIfSafePart2 inputArray =
  isNElementsSafePart2 inputArray inc False
    || isNElementsSafePart2 inputArray dec False
  where
    inc = True
    dec = False

isNElementsSafePart2 :: [Int] -> Bool -> Bool -> Bool
isNElementsSafePart2 [] _ _ = True
isNElementsSafePart2 [_] _ _ = True
isNElementsSafePart2 [_, _] _ _ = True
isNElementsSafePart2 (x1 : x2 : x3 : xs) True hasUsedProblemDampener
  | x2 > x1 && x3 > x2 && abs (x1 - x2) <= 3 && abs (x2 - x3) <= 3 = isNElementsSafePart2 (x2 : x3 : xs) True hasUsedProblemDampener
  | x3 > x2 && abs (x2 - x3) <= 3 && isNElementsSafePart2 (x2 : x3 : xs) True True && (not hasUsedProblemDampener) = True
  | x3 > x1 && abs (x1 - x3) <= 3 && isNElementsSafePart2 (x1 : x3 : xs) True True && (not hasUsedProblemDampener) = True
  | x2 > x1 && abs (x1 - x2) <= 3 && isNElementsSafePart2 (x1 : x2 : xs) True True && (not hasUsedProblemDampener) = True
  | otherwise = False
isNElementsSafePart2 (x1 : x2 : x3 : xs) False hasUsedProblemDampener
  | x2 < x1 && x3 < x2 && abs (x1 - x2) <= 3 && abs (x2 - x3) <= 3 = isNElementsSafePart2 (x2 : x3 : xs) False hasUsedProblemDampener
  | x3 < x2 && abs (x2 - x3) <= 3 && isNElementsSafePart2 (x2 : x3 : xs) False True && (not hasUsedProblemDampener) = True
  | x3 < x1 && abs (x1 - x3) <= 3 && isNElementsSafePart2 (x1 : x3 : xs) False True && (not hasUsedProblemDampener) = True
  | x2 < x1 && abs (x1 - x2) <= 3 && isNElementsSafePart2 (x1 : x2 : xs) False True && (not hasUsedProblemDampener) = True
  | otherwise = False

part2 :: IO ()
part2 = do
  records <- getArrayOfRecords getReportFromLine "input/inputday2.txt"
  print $ performCalcOnArray calculateIfSafePart2 records

-- Part 2 V2:

removeValueAtIndex :: [Int] -> Int -> [Int]
removeValueAtIndex input i = take i input ++ drop (i + 1) input

-- The same as isNElementsSafe
-- Bool input parameter represents isIncreasing (if true treate it as the array is increase, if false treate it as decreasing)
isArraySafe :: [Int] -> Bool -> Bool
isArraySafe [] _ = True
isArraySafe [_] _ = True
isArraySafe (x1 : x2 : xs) True
  | x2 > x1 && abs (x1 - x2) >= 1 && abs (x1 - x2) <= 3 = isArraySafe (x2 : xs) True
  | otherwise = False
isArraySafe (x1 : x2 : xs) False
  | x2 < x1 && abs (x1 - x2) >= 1 && abs (x1 - x2) <= 3 = isArraySafe (x2 : xs) False
  | otherwise = False

calculateIfSafePart2V2 :: Int -> [Int] -> Bool
calculateIfSafePart2V2 indexToRemove input
  | length input == indexToRemove = False
  | isArraySafe (removeValueAtIndex input indexToRemove) True = True
  | isArraySafe (removeValueAtIndex input indexToRemove) False = True
  | calculateIfSafePart2V2 (indexToRemove + 1) input = True
  | otherwise = False

part2V2 :: IO ()
part2V2 = do
  records <- getArrayOfRecords getReportFromLine "input/inputday2.txt"
  print $ performCalcOnArray (calculateIfSafePart2V2 0) records
