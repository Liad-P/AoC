{-# LANGUAGE ImportQualifiedPost #-}

-- https://adventofcode.com/2024/day/1

module AoC2024Day1 where

import Data.List (sort)
import Data.Map qualified as Map
import Text.Parsec
import Text.Parsec.String (Parser)

getLeftAndRightOfLine :: Parser (Int, Int)
getLeftAndRightOfLine = do
  leftNumber <- many1 digit
  spaces
  rightNumber <- many1 digit
  optional endOfLine
  pure (read leftNumber, read rightNumber)

accumulateFromFileWithParser :: FilePath -> Parser (Int, Int) -> IO ([Int], [Int])
accumulateFromFileWithParser filePath parser = do
  fileContent <- readFile filePath
  let fileLines = lines fileContent
  let result =
        foldr
          ( \line (leftNums, rightNums) ->
              case (parse parser "" line) of
                Left _ -> (leftNums, rightNums)
                Right (leftNum, rightNum) -> (leftNums ++ [leftNum], rightNums ++ [rightNum])
          )
          ([], [])
          fileLines
  return result

getSumOfDifferences :: ([Int], [Int]) -> Int
getSumOfDifferences (leftNums, rightNums) =
  let zippedNums = zip leftNums rightNums
   in foldr (\(leftNum, rightNum) acc -> acc + abs (leftNum - rightNum)) 0 zippedNums

part1 :: IO ()
part1 = do
  (leftNums, rightNums) <- accumulateFromFileWithParser "input/inputday1.txt" getLeftAndRightOfLine
  let leftNumsSorted = sort leftNums
  let rightNumsSorted = sort rightNums
  let summedDifference = getSumOfDifferences (leftNumsSorted, rightNumsSorted)
  print summedDifference

doCalculationOnLeftAndRightColumn :: (([Int], [Int]) -> Int) -> IO ()
doCalculationOnLeftAndRightColumn calulation = do
  (leftNums, rightNums) <- accumulateFromFileWithParser "input/inputday1.txt" getLeftAndRightOfLine
  let leftNumsSorted = sort leftNums
  let rightNumsSorted = sort rightNums
  let summedDifference = calulation (leftNumsSorted, rightNumsSorted)
  print summedDifference

getSumOfSimilarityScores :: ([Int], [Int]) -> Int
getSumOfSimilarityScores (leftNums, rightNums) =
  let rightValueCounterMap =
        foldr
          ( \x valueMap ->
              case Map.lookup x valueMap of
                Nothing -> Map.insert x 1 valueMap
                Just currentVal -> Map.insert x (currentVal + 1) valueMap
          )
          Map.empty
          rightNums
   in foldr
        ( \x acc ->
            case Map.lookup x rightValueCounterMap of
              Nothing -> acc
              Just similarityCount -> acc + (x * similarityCount)
        )
        0
        leftNums

part2 :: IO ()
part2 =
  doCalculationOnLeftAndRightColumn getSumOfSimilarityScores