-- https://adventofcode.com/2015/day/1
-- Part 1
module AoC2015Day1 where

calcFloorOfSanta:: String -> Int
calcFloorOfSanta = foldl (\acc x -> if x == '(' then acc +1 else acc - 1) 0

-- main :: IO ()
-- main = do
--     content <- readFile "inputDay1.txt"
--     let floor = calcFloorOfSanta content
--     print floor  -- 74

-- Part 2
foldUntil:: (b -> a -> b) -> (b -> Bool) -> b -> [a] -> b
foldUntil _ _ acc [] = acc
foldUntil f cond acc (x:xs) 
    | cond acc = acc
    | otherwise = foldUntil f cond (f acc x) xs


calcWhenSantaInBasement:: String -> Int
calcWhenSantaInBasement input= 
    let (acc, index) = foldUntil (\(acc, index) x -> if x == '(' then (acc +1, index+1) else (acc - 1, index+1)) (\(acc, _) -> acc < 0) (0, 0) input
    in index

main :: IO ()
main = do
    content <- readFile "inputDay1.txt"
    let index = calcWhenSantaInBasement content
    print index  -- 1795