module AoC2025Day5 where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Functor
import Data.List (nub)
import qualified Debug.Trace as Debug
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

rangeOverlaps:: Range -> Range -> Either Bool Range
rangeOverlaps (x1,x2) (y1,y2) 
    | x1 <= y1 && x2 >= y1 = Right (x1, max x2 y2)
    | x1 <= y2 && x2 >= y2 = Right (min x1 y1, x2)
    | otherwise = Left False

part2:: IO ()
part2 = do
    (ranges, ids) <- parseFile "../input/day5.txt" fileParser
    print $ length $ nub $ concatMap (\(x1,x2) -> [x1..x2]) ranges