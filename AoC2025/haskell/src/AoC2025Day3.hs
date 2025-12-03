module AoC2025Day3 where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Char (digitToInt)
import Data.List (find, findIndex, elemIndex)
import qualified Debug.Trace as Debug

fileToBanks:: String -> IO [[Int]]
fileToBanks inputFile = do
    lines <- lines <$> readFile inputFile
    let banks= map (map digitToInt) lines
    pure banks

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
intArrayToInt = go 0
    where
        go acc arr
            | null arr = acc
            | otherwise = go (acc + (head arr * 10 ^ (length arr - 1))) (tail arr)

getNJoltageFromBank:: Int -> [Int] -> Int
getNJoltageFromBank nBatteries bank = intArrayToInt $ go bank []
    where
        go nums acc 
            -- we have got the desired batteries that we want to turn on
            | length acc == nBatteries = acc
            -- we the number of batteries we have gotten + the number of batteries left in the bank is equal to the number of batteries we want
            | (length nums + length acc) == nBatteries = acc ++ nums
            -- Otherwise, we can continue iterating through the rest of the nums
            -- only consider betteries after the bettery we just added to our chosen collection
            | otherwise = go (drop (indexOfNextMax + 1) nums) (acc ++ [nextMax])
            where
                -- get the batteries that we should consider for adding to our collection
                -- we should only consider batteries that could be chosen while still allowing us to get to the n desired batteries in our collection
                numsToGetMaxFrom = take (length nums - (nBatteries - length acc) + 1) nums
                -- Choose the bettery with the maximum joltage from the options that make sense
                nextMax = maximum numsToGetMaxFrom
                -- get the index of the battery we chose with the maximum joltage
                indexOfNextMax = case elemIndex nextMax numsToGetMaxFrom of 
                    Just m -> m
                    Nothing -> error ("The max was not found" ++ show nextMax)


part2:: IO ()
part2 = do
    banks <- fileToBanks "../input/day3.txt"
    print $ sum $ map (getNJoltageFromBank 12) banks