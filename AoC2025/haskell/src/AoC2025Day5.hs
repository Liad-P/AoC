module AoC2025Day5 where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Functor
import Data.List (nub)
import Control.Monad (foldM)

type Range = (Int,Int)

freshRangeParser:: Parser Range
freshRangeParser = do
    begin <- read <$> many1 digit
    char '-'
    end <- read <$> many1 digit
    optional endOfLine
    pure (begin, end)

idParser:: Parser Int
idParser = read <$> (many1 digit <* optional endOfLine)

fileParser:: Parser ([Range], [Int])
fileParser = do
    ranges <- many1 freshRangeParser
    endOfLine
    ids <- many1 idParser
    pure (ranges, ids)

parseFile:: String -> Parser a -> IO a
parseFile filePath p = do
    content <- readFile filePath
    let grid = case parse p "" content of
            Left _ -> error "Unable to Parse"
            Right res -> res
    pure grid

isInRanges:: [Range] -> Int -> Bool
isInRanges ranges n = case result of 
        Left _ -> True
        Right _ -> False
    where 
        result = foldM 
                    (\acc (x1, x2) -> 
                        if  x1 <= n && x2 >= n then Left True
                        else Right False
                        )
                        False
                        ranges

part1:: IO ()
part1 = do
    (ranges, ids) <- parseFile "../input/day5.txt" fileParser
    print $ length $ filter (isInRanges ranges) ids

rangesParser:: Parser [Range]
rangesParser = many1 freshRangeParser

combineRanges:: Range -> Range -> [Range]
combineRanges (x1,x2) (y1,y2)
    | x1 < y1 && y1 <= x2 = [(min x1 y1, max x2 y2)]
    | x1 <= y2 && y2 <= x2 = [(min x1 y1, max x2 y2)]
    | y1 <= x1 && x1 <= y2 = [(min x1 y1, max x2 y2)]
    | y1 <= x2 && x2 <= y2 = [(min x1 y1, max x2 y2)]
    | otherwise = [(x1,x2), (y1, y2)]

remove:: Int-> [a] -> [a]
remove i xs= fstP  ++ tail sndP
    where
        (fstP, sndP) = splitAt i xs

combineUntil:: [Range] -> [Range]
combineUntil ranges
    | null ranges = []
    | length (tail ranges) == length leftOver = combinedRanges : combineUntil leftOver
    | otherwise = combineUntil (combinedRanges : leftOver)
    where
        (combinedRanges, leftOver) = foldl (\(accR, othR) r ->
                let combinedRange = combineRanges accR r
                in
                    if length combinedRange > 1 then (accR, othR ++ [r])
                    else (head combinedRange, othR)
            )
            (head ranges, [])
            (tail ranges)

part2:: IO ()
part2 = do
    ranges <- parseFile "../input/day5.txt" rangesParser
    print $ length ranges
    let finalRanges = nub $ combineUntil ranges
    print $ sum $ map (\(x1, x2) -> x2 - x1 +1) finalRanges -- 342018167474526
