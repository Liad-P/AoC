{-# LANGUAGE ScopedTypeVariables #-}
module AoC2025Day6 where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Functor
import Data.List (nub)
import qualified Debug.Trace as Debug

parseFile:: String -> Parser a -> IO a
parseFile filePath p = do
    content <- readFile filePath
    let res = case parse p "" content of
            Left e -> error ("Unable to Parse: " ++ show e)
            Right res -> res
    pure res


numberRowParser:: Parser [Int]
numberRowParser = do
    spaces
    nums <- many1 ((read <$> many1 digit) <* many (char ' ' <|> tab))
    optional endOfLine
    pure nums

charRowParser:: Parser [Char]
charRowParser =
    many1 $
        ((char '*' <|>
        char '+') <* spaces)

fileParser:: Parser ([[Int]], [Char])
fileParser = do
    numRows <- many1 numberRowParser
    ops <- charRowParser
    pure (numRows, ops)

aggregateCol:: Int -> a -> (a -> b -> a) -> [[b]] -> a
aggregateCol colNum initAcc f nums2D = foldl (\acc row -> f acc (row !! colNum)) initAcc nums2D


part1:: IO ()
part1 = do
    (nums, ops) <- parseFile "../input/day6.txt" fileParser
    let res =
            foldl
            (\acc i ->
                if (ops !! i) == '*' then acc + aggregateCol i 1 (*) nums
                else acc + aggregateCol i 0 (+) nums
            )
            0
            [0..(length ops - 1)]
    print res -- 5335495999141

intLog10:: Int -> Int
intLog10 input = go input 0
    where
        go n c
            | n `div` 10 > 0 = go (n `div` 10) (c+1)
            | otherwise = c

substring :: Int -> Int -> [a] -> [a]
substring start end str = drop start (take end str)

separateLineAtIndexes:: String -> [Int] -> [String]
separateLineAtIndexes line indexes =
    let indexesPlusEnd = indexes ++ [length line]
    in
    foldl
    (\acc i ->
        acc ++ [substring (indexesPlusEnd !! i) (indexesPlusEnd !! (i+1)) line]
        )
        []
        [0..(length indexesPlusEnd -2)]

separateAllLinesAtIndexes:: [String] -> [Int] -> [[String]]
separateAllLinesAtIndexes lines indexes =
    foldl
        (\acc line ->
            acc ++ [separateLineAtIndexes line indexes]
            )
            []
            lines

trim:: String -> String
trim = filter (/= ' ')

part2:: IO ()
part2 = do
    content <- readFile "../input/day6.txt"
    let ls = lines content
    let lastLine = last ls
    let (indexesOfOps, ops) = foldl
                        (\(indexAcc, opAcc) i ->
                            if lastLine !! i == '*' || lastLine !! i == '+' then (indexAcc ++ [i], opAcc ++ [lastLine !! i])
                            else (indexAcc, opAcc)
                        )
                        ([],[])
                        [0..(length lastLine -1)]
    let splitLines = separateAllLinesAtIndexes (init ls) indexesOfOps
    let numsInCols:: [[Int]] =
            foldl
                (\acc colI ->
                    let setOfColI = map (!! colI) splitLines
                        lengthOfColI = length (head setOfColI)
                    in
                        acc ++
                        [foldl
                            (\accNums i ->
                                let charsToMakeInt:: [Char] = map (!! i) setOfColI
                                in
                                    if null (trim charsToMakeInt) then accNums
                                    else accNums ++ [read (trim charsToMakeInt):: Int]
                                )
                                []
                                [0..(lengthOfColI -1)]

                        ]

                )
                []
                [0..(length (head splitLines) -1)]
    let result = sum $ map 
                        (\(op, nums) -> 
                            if op == '*' then product nums
                            else sum nums
                            ) 
                            (zip ops numsInCols)

    print result