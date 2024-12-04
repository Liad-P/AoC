-- https://adventofcode.com/2024/day/4
module AoC2024Day4 where

import Data.Maybe (catMaybes)

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