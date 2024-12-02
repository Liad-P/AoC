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

-- xx :: ([Int] -> Bool) -> [[Int]] -> [[Int]]
-- xx filderCond inputArray = filter filderCond inputArray

part1 :: IO ()
part1 = do
  records <- getArrayOfRecords getReportFromLine "/home/liad/code/Side_Projects/AoC/AoC2024/haskell/app/inputday2.txt"
  print $ performCalcOnArray calculateIfSafe records

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
  records <- getArrayOfRecords getReportFromLine "/home/liad/code/Side_Projects/AoC/AoC2024/haskell/app/inputday2.txt"
  --   print $ reverse $ (\x -> (x, calculateIfSafePart2 x)) <$> records
  print $ performCalcOnArray calculateIfSafePart2 records