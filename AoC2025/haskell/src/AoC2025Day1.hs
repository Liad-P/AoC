-- https://adventofcode.com/2015/day/1
-- Part 1
module AoC2025Day1 where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Debug.Trace as Debug
import Data.Functor

lineParser:: Parser [Int]
lineParser =
    many1 $
        (*) <$> ((char 'L' $> (-1)) <|> (char 'R' $> 1))
            <*> (read <$> many1 digit <* optional endOfLine)



parseFile:: String -> Parser a -> a
parseFile lines p =
    let a = parse p "" lines
        in case a of
            Left err -> error "Error passing file"
            Right res -> res

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
    inputLines <- readFile "../input/day1.txt"
    let turns = parseFile inputLines lineParser
    print $ countZeroSums turns


calculatePassesOfZero:: Int -> Int -> Int
calculatePassesOfZero cur inc = (val + abs inc) `div` 100
    where
        val
        -- if cur == 0, and inc < 0: example inc = -5, then cur + inc = -5, -5/100 = -1 but we didnt pass 0, therefore rather do cur + abs(inc) = 5 in this case
        -- Other case: example cur = 5, inc =98: then work as expected 103/100 = 1 which is as expected.
          | inc >= 0 || cur == 0 = cur
        -- Case where inc is negative and cur != 0: then shift curr into the positive space
          | otherwise = 100 - cur

part2:: IO Int
part2 = do
    inputLines <- readFile "../input/day1.txt"
    let turns = parseFile inputLines lineParser
    pure $
        snd $
            foldl
                (\(acc, count) c -> ((acc + c) `mod` 100, count + calculatePassesOfZero acc c))
                (50,0)
                turns