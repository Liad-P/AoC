-- https://adventofcode.com/2015/day/1
-- Part 1
module AoC2025Day1 where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Debug.Trace as Debug

lineParser:: Parser Int
lineParser = do
    op <- operator
    integer <- read <$> many1 digit
    pure (op * integer)
    where
        operator:: Parsec String st Int
        operator =
            try (char 'L') *> pure (-1)
            <|> (char 'R') *> pure 1

parseFile:: [String] -> Parser a -> [a]
parseFile lines p =
    foldl (\acc line ->
            let a = parse p "" line
            in case a of
                Left err -> error "Error passing file"
                Right res -> acc ++ [res])
                []
                lines

countZeroSums:: [Int] -> Int
countZeroSums ns =
    snd $
    foldl
        (\acc c ->
            let newAcc = ((fst acc) + c) `mod` 100
            in
                if newAcc == 0
                    then (newAcc, (snd acc) + 1)
                    else (newAcc, snd acc)
        )
            (50,0)
            ns

part1:: IO ()
part1 = do
    inputLines <- lines <$> readFile "../input/day1.txt"
    let turns = parseFile inputLines lineParser
    print $ countZeroSums turns


countPassesOfZeros:: [Int] -> Int
countPassesOfZeros ns =
    snd $
    foldl
        (\acc c ->

            let newSum = (fst acc) + c
                newAcc = newSum `mod` 100
                passesOf0 = calculatePassesOfZero (fst acc) c
            in
                Debug.trace ("STEP:\nOld acc = " ++ show acc ++ "\nChange val = " ++ show c ++ "\nCalculated Passes Of Zero = " ++ show passesOf0) (newAcc, snd acc + passesOf0)
        )
        (50,0)
        ns

calculatePassesOfZero:: Int -> Int -> Int
calculatePassesOfZero cur inc = val `div` 100
    where
        val
          | cur == 0 = abs inc
          | inc < 0 = 100 - cur + abs inc
          | otherwise = cur + inc

part2:: IO ()
part2 = do
    inputLines <- lines <$> readFile "../input/day1.txt"
    let turns = parseFile inputLines lineParser
    print $ countPassesOfZeros turns