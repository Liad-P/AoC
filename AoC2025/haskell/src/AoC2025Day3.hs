module AoC2025Day3 where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Char (digitToInt)

fileToBanks:: String -> IO [[Int]]
fileToBanks inputFile = do
    lines <- lines <$> readFile inputFile
    let banks= map (\s -> map digitToInt s) lines
    pure banks

-- getNMaxDigits:: Int -> [Int] -> [Int]
-- getNMaxDigits n inputDigits = 
--     foldl (\acc elem -> 

--         )

get2MaxDigits:: [Int] -> (Int, Int)
get2MaxDigits inputDigits =
    foldr (\elem acc ->
            if (fst elem > fst acc) || 
                (fst elem == fst acc && snd elem > snd acc) 
                then elem 
            else acc
        ) (0,0)
        possiblePairs
    where
        possiblePairs::[(Int, Int)]
        possiblePairs = go inputDigits []
        go nums acc 
            | length nums < 2 = acc
            | length nums == 2 = acc ++ [(head nums, last nums)]
            | otherwise = go (tail nums) (acc ++ [(head nums, maximum $ tail nums)])
        


part1:: IO ()
part1 = do
    banks <- fileToBanks "../input/day3.txt"
    let maxDigits = map get2MaxDigits banks
        totalJoltage = sum $ map (\(a,b) -> a*10 + b) maxDigits
    print totalJoltage