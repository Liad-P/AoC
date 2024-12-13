module AoC2024Day7 where

import Data.List (nub)
import Text.Parsec
import Text.Parsec.String (Parser)

testCase =
  [ "190: 10 19",
    "3267: 81 40 27",
    "83: 17 5",
    "156: 15 6",
    "7290: 6 8 6 15",
    "161011: 16 10 13",
    "192: 17 8 14",
    "21037: 9 7 18 13",
    "292: 11 6 16 20"
  ]

parseLine :: Parser (Int, [Int])
parseLine = do
  calcResult <- many1 digit
  string ": "
  numbers <- sepBy (many1 digit) space
  pure (read calcResult, read <$> numbers)

doCalc :: String -> [Int] -> Int
doCalc inputOperators inputNums
  | length inputNums == 1 = head inputNums
  | head inputOperators == '*' = doCalc (tail inputOperators) ([((head inputNums) * (head (reverse $ take 2 inputNums)))] ++ (drop 2 inputNums))
  | head inputOperators == '+' = doCalc (tail inputOperators) ([((head inputNums) + (head (reverse $ take 2 inputNums)))] ++ (drop 2 inputNums))

makeCombinationsOfCalc :: Int -> [String] -> [String]
makeCombinationsOfCalc lengthOfOperations currentOperations
  | null currentOperations = makeCombinationsOfCalc lengthOfOperations [['*'], ['+']]
  | (length $ head currentOperations) == lengthOfOperations = currentOperations
  | otherwise = makeCombinationsOfCalc lengthOfOperations (foldl (\acc operationIteration -> acc ++ [operationIteration ++ ['*']] ++ [operationIteration ++ ['+']]) [] currentOperations)

getAllPossibleCalcAnswersFromInts :: [Int] -> [Int]
getAllPossibleCalcAnswersFromInts inputInts = foldl (\acc operationVariation -> acc ++ [doCalc operationVariation inputInts]) [] (makeCombinationsOfCalc (length inputInts - 1) [])

part1 :: IO ()
part1 = do
  fileContent <- readFile "input/inputday7.txt"
  let fileLines = lines fileContent
  -- let fileLines = testCase
  let parsedResults =
        foldl
          ( \acc line ->
              case (parse parseLine "" line) of
                Left _ -> acc
                Right res -> acc ++ [res]
          )
          []
          fileLines
  let findPossibleCalculations = foldl (\acc (resultOfCalc, inputInts) -> if resultOfCalc `elem` (getAllPossibleCalcAnswersFromInts inputInts) then acc ++ [resultOfCalc] else acc) [] parsedResults
  print $ sum $ findPossibleCalculations

doCalcPart2 :: String -> [Int] -> Int
doCalcPart2 inputOperators inputNums
  | length inputNums == 1 = head inputNums
  | head inputOperators == '*' = doCalcPart2 (tail inputOperators) ([((head inputNums) * (head (tail inputNums)))] ++ (drop 2 inputNums))
  | head inputOperators == '+' = doCalcPart2 (tail inputOperators) ([((head inputNums) + (head (tail inputNums)))] ++ (drop 2 inputNums))
  | head inputOperators == '|' = doCalcPart2 (tail inputOperators) ([read ((show $ head inputNums) ++ (show $ head (tail inputNums)))] ++ (drop 2 inputNums))

makeCombinationsOfCalcPart2 :: Int -> [String] -> [String]
makeCombinationsOfCalcPart2 lengthOfOperations currentOperations
  | null currentOperations = makeCombinationsOfCalcPart2 lengthOfOperations [['*'], ['+'], ['|']]
  | (length $ head currentOperations) == lengthOfOperations = currentOperations
  | otherwise = makeCombinationsOfCalcPart2 lengthOfOperations (foldl (\acc operationIteration -> acc ++ [operationIteration ++ ['*']] ++ [operationIteration ++ ['+']] ++ [operationIteration ++ ['|']]) [] currentOperations)

getAllPossibleCalcAnswersFromIntsPart2 :: [Int] -> [Int]
getAllPossibleCalcAnswersFromIntsPart2 inputInts = foldl (\acc operationVariation -> acc ++ [doCalcPart2 operationVariation inputInts]) [] (makeCombinationsOfCalcPart2 ((length inputInts) - 1) [])


part2 :: IO ()
part2 = do
  fileContent <- readFile "input/inputday7.txt"
  let fileLines = lines fileContent
  -- let fileLines = testCase
  let parsedResults =
        foldl
          ( \acc line ->
              case (parse parseLine "" line) of
                Left _ -> acc
                Right res -> acc ++ [res]
          )
          []
          fileLines
  let findPossibleCalculations = foldl (\acc (resultOfCalc, inputInts) -> if resultOfCalc `elem` (getAllPossibleCalcAnswersFromIntsPart2 inputInts) then acc ++ [resultOfCalc] else acc) [] parsedResults
  print $ sum $ findPossibleCalculations

-- Part 2 V2

recursiveCalculation :: Int -> [Int] -> Int -> Bool
recursiveCalculation currentAccumulation restOfList targetValue
  | null restOfList && currentAccumulation == targetValue = True
  | null restOfList && currentAccumulation /= targetValue = False
  | recursiveCalculation (currentAccumulation + head restOfList) (tail restOfList) targetValue = True
  | recursiveCalculation (currentAccumulation * head restOfList) (tail restOfList) targetValue = True
  | recursiveCalculation (read (show currentAccumulation ++ (show $ head restOfList))) (tail restOfList) targetValue = True
  | otherwise = False

part2V2 :: IO ()
part2V2 = do
  fileContent <- readFile "input/inputday7.txt"
  let fileLines = lines fileContent
  -- let fileLines = testCase
  let parsedResults =
        foldl
          ( \acc line ->
              case (parse parseLine "" line) of
                Left _ -> acc
                Right res -> acc ++ [res]
          )
          []
          fileLines
  let correctCalcs = map (\(r, cn) -> r) $ filter (\(res, calcNums) -> recursiveCalculation (head calcNums) (tail calcNums) (res)) parsedResults
  print $ sum $ correctCalcs
