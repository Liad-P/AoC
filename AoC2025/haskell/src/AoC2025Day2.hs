module AoC2025Day2 where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Debug.Trace as Debug

rangeParser:: Parser (Int,Int)
rangeParser = do
    start <- many1 digit
    char '-'
    end <- many1 digit
    pure (read start, read end)

fileToRangesParser:: Parser [(Int,Int)]
fileToRangesParser = sepBy1 rangeParser (char ',')

applyParserToFile:: Parser a -> String -> IO a
applyParserToFile p filePath = do
    content <- readFile filePath
    let result = parse p "" content
    pure $
        case result of
            Left err -> error "Error parsing file"
            Right res -> res

intLog10:: Int -> Int
intLog10 input = go input 0
    where
        go n c
            | n `div` 10 > 0 = go (n `div` 10) (c+1)
            | otherwise = c

numberOfDigitsInInt:: Int -> Int
numberOfDigitsInInt input = 1 + intLog10 input

hasRepeatedSequence:: Int -> Bool
hasRepeatedSequence input
    | odd len = False
    | fstHalf == sndHalf = True
    | otherwise = False
    where
        len = numberOfDigitsInInt input
        (fstHalf,sndHalf) = input `divMod` (10 ^ (len `div` 2))

part1:: IO ()
part1 = do
    ranges <- applyParserToFile fileToRangesParser "../input/day2.txt"
    let res = foldr (\(x1,x2) acc ->
                    acc + sum (filter hasRepeatedSequence [x1..x2])
                )
                0
                ranges
    print res

hasRepeatedSequenceV2:: Int -> Bool
-- hasRepeatedSequenceV2 input = any allElementsEqual (Debug.trace ("Possible filters for input = " ++ show input ++ " with length " ++ show len ++ ", " ++ show allPossibleRepeatedSequences) allPossibleRepeatedSequences)
hasRepeatedSequenceV2 input = any allElementsEqual allPossibleRepeatedSequences
    where
        len = numberOfDigitsInInt input
        possibleRepeatedLengths
            | len < 2 = []
            | otherwise = filter (\elem -> len `mod` elem == 0) [1..(len `div` 2)]

        getRepeatedSequences:: Int -> Int -> [Int]
        getRepeatedSequences splitLen numInput = go numInput []
            where
                go i arr
                    | r == 0 && d == 0 = arr
                    | otherwise = go d (arr ++ [r])
                    where
                        (d,r) = i `divMod` (10 ^ splitLen)
        allPossibleRepeatedSequences::[[Int]]
        allPossibleRepeatedSequences = foldr (\elem acc -> acc ++ [getRepeatedSequences elem input]) [] possibleRepeatedLengths
        allElementsEqual:: [Int] -> Bool
        allElementsEqual [] = False
        allElementsEqual (x: xs)
            | null xs = False
            | length xs == 1 = x == head xs
            | x == head xs = allElementsEqual xs
            | otherwise = False


part2:: IO ()
part2 = do
    ranges <- applyParserToFile fileToRangesParser "../input/day2.txt"
    let res = foldr (\(x1,x2) acc ->
                    acc + sum (filter hasRepeatedSequenceV2 [x1..x2])
                )
                0
                ranges
    print res