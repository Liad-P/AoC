module AoC2024Day9 where

import Data.List (group, zipWith)

testCase = "2333133121414131402"

parseLine :: [Char] -> [Int]
parseLine inputString = map (\x -> read [x]) inputString

convertLineToRelevantNumbers :: [Int] -> [Int]
convertLineToRelevantNumbers xs = snd $ foldl (\(i, listNums) x -> if even i then (i + 1, listNums ++ replicate x (i `div` 2)) else (i + 1, listNums)) init xs
  where
    init = (0, [])

packListIntoEmptySpaces :: [Int] -> [[Int]]
packListIntoEmptySpaces xs = snd $ foldl (\(diskMap, groupedDiskMapToFill) x -> (drop x diskMap, groupedDiskMapToFill ++ [take x diskMap])) ((reverse $ convertLineToRelevantNumbers xs), []) getEmptySpaceInts
  where
    getEmptySpaceInts = snd $ foldl (\(i, listNums) x -> if odd i then (i + 1, listNums ++ [x]) else (i + 1, listNums)) (0, []) xs

getZippedListsOfDiskMap :: [Int] -> [Int]
getZippedListsOfDiskMap xs = take (length $ convertLineToRelevantNumbers xs) $ concat $ zipWith (++) (group $ convertLineToRelevantNumbers xs) (packListIntoEmptySpaces xs)

getCheckSum :: [Int] -> Int
getCheckSum inputInts = snd $ foldl (\(i, curSum) x -> (i + 1, curSum + (x * i))) (0, 0) inputInts

part1 :: IO ()
part1 = do
  fileContent <- readFile "input/inputday9.txt"
  print $ getCheckSum $ getZippedListsOfDiskMap $ parseLine fileContent

convertLineToRelevantNumbersWithEmptySpaces :: [Int] -> [Int]
convertLineToRelevantNumbersWithEmptySpaces xs = snd $ foldl (\(i, listNums) x -> if even i then (i + 1, listNums ++ replicate x (i `div` 2)) else (i + 1, listNums ++ replicate x (-1))) init xs
  where
    init = (0, [])

packListIntoEmptySpacesPart2 :: [Int] -> [[Int]]
packListIntoEmptySpacesPart2 xs =
  snd $
    foldl
      ( \(diskMapGrouped, groupedDiskMapToFill) x ->
          if x >= (length $ head diskMapGrouped)
            then (tail diskMapGrouped, groupedDiskMapToFill ++ [head diskMapGrouped ++ replicate (x - (length $ head diskMapGrouped)) (-1)])
            else (tail diskMapGrouped ++ [head diskMapGrouped], groupedDiskMapToFill ++ [replicate x (-1)])
      )
      ((reverse (group $ convertLineToRelevantNumbers xs)), [])
      getEmptySpaceInts
  where
    getEmptySpaceInts = snd $ foldl (\(i, listNums) x -> if odd i then (i + 1, listNums ++ [x]) else (i + 1, listNums)) (0, []) xs

getOnlyOccupiedSpace :: [[Int]] -> [[Int]]
getOnlyOccupiedSpace allSpace = filter (\s -> head s /= -1) allSpace

-- xx:: [[Int]] -> [[Int]]
-- xx inputDiskMap occuppiedSpacesStillToLookAt=
--   | head $ head inputDiskMap == -1 = case (findOccuppiedSpaceThatFits occupiedSpaces (length $ head inputDiskMap) ) of
--       Nothing -> [head inputDiskMap] ++ xx (tail inputDiskMap) occuppiedSpacesStillToLookAt
--       Just (lastOccupiedSpace, leftOverOccupiedSpaces) -> [lastOccupiedSpace, replicate (length $ head inputDiskMap - lastOccupiedSpace)]
--   where
--       occupiedSpaces = getOnlyOccupiedSpace inputDiskMap
--       findOccuppiedSpaceThatFits:: [[Int]] -> Int -> Maybe ([Int], [[Int]])
--       findOccuppiedSpaceThatFits occupiedSpaces lengthOfTarget
--         | null occupiedSpaces = Nothing
--         | head $ last occupiedSpaces == -1 = findOccuppiedSpaceThatFits (init occupiedSpaces) lengthOfTarget
--         | length $ last occupiedSpaces <= lengthOfTarget = Just (last occupiedSpaces, init occupiedSpaces)
--         | otherwise = findOccuppiedSpaceThatFits (init occupiedSpaces) lengthOfTarget

--       insertAt :: Int -> Int -> [Int] -> [Int]
--       insertAt i element a = fst (splitAt i a) ++ [element] ++ snd (splitAt i a)
--       replaceElementAtIndex :: [a] -> Int -> a -> [a]
--       replaceElementAtIndex input index newElement = fst (splitAt index input) ++ [newElement] ++ tail (snd (splitAt index input))

-- find -1 spaces
-- Check from end of occupied places which ones can fit into -1 spaces
-- replace -1 spaces with occuppied places found and if more space then add another list of -1s
-- remove the occupiedSpaces used with a list of -1 of equal lenght in its place
-- start with list of occuppied places will all the ones explored removed
-- Marked the occupied spaces that were tried to move to the front and carry on from the rest

-- yy :: [[Int]] -> [[Int]] -> [[Int]]
-- yy occupiedDiskMap diskMap
--   | null $ findOccuppiedSpaceThatFitsInEmptySpace diskMap || null occupiedDiskMap = diskMap
--   where
--     revOccupiedDiskMap = reverse occupiedDiskMap
--     findOccuppiedSpaceThatFitsInEmptySpace diskMap
--       | null diskMap = []
--       | head $ head diskMap == -1 = if not $ null (filter (\x -> length x <= length $ head diskMap) occupiedDiskMap)
--         then [targetOccuppiedValueToMove, replicate (length $ head diskMap - length $ targetOccuppiedValueToMove) (-1)] ++ tail (delete targetOccuppiedValueToMove diskMap)
--         else diskMap
--       | otherwise = [head diskMap] ++ findOccuppiedSpaceThatFitsInEmptySpace (tail diskMap)
--       where
--         targetOccuppiedValueToMove = last (filter (\x -> length x <= length $ head diskMap) occupiedDiskMap)
--         indexOfOccuppiedToMove = elemIndex (targetOccuppiedValueToMove) diskMap
--     getNewOccuppiedDiskMapsLeftOver = if not $ null $ findOccuppiedSpaceThatFitsInEmptySpace diskMap
--         then take (elemIndex (findOccuppiedSpaceThatFitsInEmptySpace diskMap)) diskMap

--     replaceElementAtIndex :: [a] -> Int -> a -> [a]
--     replaceElementAtIndex input index newElement = fst (splitAt index input) ++ [newElement] ++ tail (snd (splitAt index input))

--     getNewDiskMap =
