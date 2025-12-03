module AoC2025Day3 where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Char (digitToInt)
import Data.List (find, findIndex, elemIndex)
import qualified Debug.Trace as Debug

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

intArrayToInt:: [Int] -> Int
intArrayToInt xs = go 0 xs
    where
        go acc arr
            | null arr = acc
            | otherwise = go (acc + (head arr * 10 ^ (length arr - 1))) (tail arr)

getNMaxDigits:: Int -> [Int] -> Int
getNMaxDigits n inputDigits = intArrayToInt possiblePairs
    where
        possiblePairs::[Int]
        possiblePairs = go inputDigits []
        go nums acc 
            | length acc == n = acc
            | (length nums + length acc) == n = acc ++ nums
            | otherwise = go (drop (indexOfNextMax+1) nums) (acc ++ [nextMax])
            where
                numsToGetMaxFrom = take (length nums - (n - length acc) + 1) nums
                nextMax = maximum numsToGetMaxFrom
                indexOfNextMax = case elemIndex nextMax numsToGetMaxFrom of 
                    Just m -> m
                    Nothing -> error ("The max was not found" ++ show nextMax)


part2:: IO ()
part2 = do
    banks <- fileToBanks "../input/day3.txt"
    let maxDigits = map (getNMaxDigits 12) banks
        totalJoltage = sum maxDigits
    print totalJoltage